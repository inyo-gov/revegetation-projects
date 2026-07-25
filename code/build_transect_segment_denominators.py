#!/usr/bin/env python3
"""Build transect_segment_denominators.csv for 1991-era reveg parcels.

2025 workbook columns are post-to-post segment legs (~60–100 m), not the 15 m
sub-segments in the 1991 field datasheets. Denominators use GIS distance between
segment endpoint posts at 0.5 m intercept spacing: n_possible_hits = length_m / 0.5.

Join key: parcel + segment_key (normalize _ to -).
"""

from __future__ import annotations

import json
import math
import re
from pathlib import Path

import geopandas as gpd
import pandas as pd

REPO = Path(__file__).resolve().parents[1]
OUT = REPO / "data" / "transect_segment_denominators.csv"
WORKBOOK = REPO / "Re_Veg Parcels 2025 IND_BLK_TIN_BGP_BIS_LAW.xlsx"
SPACING_M = 0.5

# 2025 workbook column -> length_m from blk16e_transects.geojson
BLK16E_WORKBOOK_KEYS = {
    "1s-1n": 83,
    "2n-2s": 100,
    "3s-3n": 60,
    "4n-4s": 60,
    "5s-5n": 80,
    "6n-6s": 100,
    "7s-7n": 69,
}


def norm_key(s: str) -> str:
    s = str(s).strip().replace("_", "-")
    s = re.sub(r"2\.\.2", "2.2", s)
    s = re.sub(r"A3\.20", "A3.2", s)
    return s


def norm_parcel(parcel: str) -> str:
    if parcel == "Hines south":
        return "Hines South"
    return str(parcel).strip()


def haversine_m(lon1: float, lat1: float, lon2: float, lat2: float) -> float:
    r = 6_371_000
    p1, p2 = math.radians(lat1), math.radians(lat2)
    dphi = math.radians(lat2 - lat1)
    dlmb = math.radians(lon2 - lon1)
    a = math.sin(dphi / 2) ** 2 + math.cos(p1) * math.cos(p2) * math.sin(dlmb / 2) ** 2
    return 2 * r * math.asin(math.sqrt(a))


def load_tin054_posts() -> dict[tuple[str, str], tuple[float, float]]:
    """Pair 32 AGOL TIN054 posts into 16 perimeter transects (red ↔ blue)."""
    path = REPO / "data/gis/TIN054_tpost_transects.json"
    if not path.exists():
        return {}

    try:
        from scipy.optimize import linear_sum_assignment
    except ImportError:
        linear_sum_assignment = None

    feats = json.loads(path.read_text())["features"]
    red: list[tuple[int, float, float]] = []
    blue: list[tuple[int, float, float]] = []
    for i, feat in enumerate(feats):
        if feat["geometry"]["type"] != "Point":
            continue
        lon, lat = feat["geometry"]["coordinates"][:2]
        if feat["properties"].get("marker-color") == "#f50057":
            red.append((i, lon, lat))
        else:
            blue.append((i, lon, lat))

    if len(red) != 16 or len(blue) != 16:
        return {}

    cost = [[haversine_m(blon, blat, rlon, rlat) for _, rlon, rlat in red] for _, blon, blat in blue]
    if linear_sum_assignment is not None:
        row_idx, col_idx = linear_sum_assignment(cost)
        matches = [(blue[row_idx[i]], red[col_idx[i]], cost[row_idx[i]][col_idx[i]]) for i in range(16)]
    else:
        used_red: set[int] = set()
        matches = []
        for bi, (bidx, blon, blat) in enumerate(blue):
            best = min(
                ((ci, haversine_m(blon, blat, rlon, rlat)) for ci, (_, rlon, rlat) in enumerate(red) if ci not in used_red),
                key=lambda item: item[1],
            )
            used_red.add(best[0])
            matches.append((blue[bi], red[best[0]], best[1]))

    pairs: list[dict] = []
    for (_, blon, blat), (_, rlon, rlat), dist in matches:
        if blat >= rlat:
            s1, s0 = (blon, blat), (rlon, rlat)
        else:
            s1, s0 = (rlon, rlat), (blon, blat)
        pairs.append({"dist": dist, "s1": s1, "s0": s0})

    cx = sum(p["s1"][0] for p in pairs) / 16
    cy = sum(p["s1"][1] for p in pairs) / 16
    for pair in pairs:
        lon, lat = pair["s1"]
        pair["angle"] = math.atan2(lat - cy, lon - cx)

    pairs.sort(key=lambda item: item["angle"], reverse=True)
    start = min(range(16), key=lambda idx: pairs[idx]["dist"])
    ordered = pairs[start:] + pairs[:start]

    posts: dict[tuple[str, str], tuple[float, float]] = {}
    for transect_num, pair in enumerate(ordered, start=1):
        posts[("TIN054", f"{transect_num}.1")] = pair["s1"]
        posts[("TIN054", f"{transect_num}.0")] = pair["s0"]
    return posts


def write_tin054_posts_csv(posts: dict[tuple[str, str], tuple[float, float]]) -> None:
    tin_posts = {
        station: coords for (parcel, station), coords in posts.items() if parcel == "TIN054"
    }
    if not tin_posts:
        return
    out = REPO / "data/gis/TIN054_transect_posts.csv"
    rows = [
        {"parcel": "TIN054", "station": station, "lon": coords[0], "lat": coords[1]}
        for station, coords in sorted(
            tin_posts.items(), key=lambda item: (float(item[0].split(".")[0]), item[0])
        )
    ]
    pd.DataFrame(rows).astype({"station": str}).to_csv(out, index=False)


def load_gis_posts() -> dict[tuple[str, str], tuple[float, float]]:
    posts: dict[tuple[str, str], tuple[float, float]] = {}

    with open(REPO / "data/gis/Revegetation_Transects91.geojson") as f:
        gj = json.load(f)
    for feat in gj["features"]:
        pr = feat["properties"]
        parcel = norm_parcel(pr["parcel"])
        station = str(pr["transct"]).strip()
        lon, lat = feat["geometry"]["coordinates"]
        posts[(parcel, station)] = (lon, lat)

    ind105_shp = REPO / "data/gis/IND105.shp"
    if ind105_shp.exists():
        ind = gpd.read_file(ind105_shp).to_crs(4326)
        for _, row in ind.iterrows():
            station = str(row["transect_p"]).strip()
            lon, lat = row.geometry.x, row.geometry.y
            posts[("IND105", station)] = (lon, lat)

    posts.update(load_tin054_posts())
    write_tin054_posts_csv(posts)

    return posts


def gis_segment_length(
    posts: dict[tuple[str, str], tuple[float, float]], parcel: str, segment_key: str
) -> float | None:
    if "-" not in segment_key:
        return None
    beg, end = segment_key.split("-", 1)
    a = posts.get((parcel, beg))
    b = posts.get((parcel, end))
    if a is None or b is None:
        return None
    return haversine_m(a[0], a[1], b[0], b[1])


def hits_from_length(length_m: float) -> int:
    return int(round(length_m / SPACING_M))


def workbook_segment_keys(sheet_names: set[str]) -> dict[str, set[str]]:
    keys: dict[str, set[str]] = {}
    if not WORKBOOK.exists():
        return keys
    for sheet in sheet_names:
        df = pd.read_excel(WORKBOOK, sheet_name=sheet, header=None)
        sheet_keys: set[str] = set()
        for c in df.iloc[0, 3:]:
            if pd.isna(c):
                continue
            s = str(c).strip()
            if s in ("TOTAL", "%COV", "%COMP", "ABS.ACRES") or s.startswith("%"):
                continue
            sheet_keys.add(norm_key(s))
        keys[sheet] = sheet_keys
    return keys


def record_from_length(
    parcel: str,
    segment_key: str,
    length_m: float,
    source: str,
    notes: str = "",
) -> dict:
    beg, end = (segment_key.split("-", 1) + [""])[:2]
    n = hits_from_length(length_m)
    return {
        "parcel": parcel,
        "segment_key": segment_key,
        "segment_beg": beg,
        "segment_end": end,
        "length_m": round(length_m, 2),
        "n_possible_hits": n,
        "intercept_spacing_m": SPACING_M,
        "source": source,
        "in_2025_workbook": True,
        "notes": notes,
    }


def main() -> None:
    posts = load_gis_posts()
    wb_sheets = set(pd.ExcelFile(WORKBOOK).sheet_names) if WORKBOOK.exists() else set()
    wb_keys = workbook_segment_keys(wb_sheets)

    records: list[dict] = []

    for parcel, keys in sorted(wb_keys.items()):
        for segment_key in sorted(keys):
            if parcel == "BLK16E" and segment_key in BLK16E_WORKBOOK_KEYS:
                length_m = BLK16E_WORKBOOK_KEYS[segment_key]
                records.append(
                    record_from_length(
                        parcel,
                        segment_key,
                        length_m,
                        "blk16e_transects.geojson",
                        "Full transect line; 2025 workbook uses line-level columns",
                    )
                )
                continue

            length_m = gis_segment_length(posts, parcel, segment_key)
            if length_m is not None:
                records.append(
                    record_from_length(
                        parcel,
                        segment_key,
                        length_m,
                        "gis_post_distance",
                        "Post-to-post distance; 0.5 m intercept spacing",
                    )
                )
            else:
                beg, end = (segment_key.split("-", 1) + [""])[:2]
                records.append(
                    {
                        "parcel": parcel,
                        "segment_key": segment_key,
                        "segment_beg": beg,
                        "segment_end": end,
                        "length_m": "",
                        "n_possible_hits": "",
                        "intercept_spacing_m": "",
                        "source": "NEEDS_ACQUISITION",
                        "in_2025_workbook": True,
                        "notes": "Missing GIS posts for segment endpoints",
                    }
                )

    tin054 = [r for r in records if r["parcel"] == "TIN054" and r["source"] == "gis_post_distance"]
    if tin054:
        print(
            f"TIN054 GIS segments: n={len(tin054)}, "
            f"length_m median={pd.Series([r['length_m'] for r in tin054]).median():.1f}"
        )

    out = pd.DataFrame(records)
    cols = [
        "parcel",
        "segment_key",
        "segment_beg",
        "segment_end",
        "length_m",
        "n_possible_hits",
        "intercept_spacing_m",
        "source",
        "in_2025_workbook",
        "notes",
    ]
    out = out[cols].sort_values(["parcel", "segment_key"]).drop_duplicates(
        ["parcel", "segment_key"]
    )
    OUT.parent.mkdir(parents=True, exist_ok=True)
    out.to_csv(OUT, index=False)
    print(f"Wrote {len(out)} rows to {OUT}")

    needs = out[out["source"] == "NEEDS_ACQUISITION"]
    if len(needs):
        print(f"NEEDS_ACQUISITION: {len(needs)} segments")
        print(needs[["parcel", "segment_key"]].to_string(index=False))

    gis = out[out["source"] == "gis_post_distance"]
    if len(gis):
        print(
            f"GIS segments: n={len(gis)}, "
            f"length_m median={gis['length_m'].median():.1f}, "
            f"hits median={gis['n_possible_hits'].median():.0f}"
        )


if __name__ == "__main__":
    main()
