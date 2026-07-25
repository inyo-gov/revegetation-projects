# 1999 Plan — Sustainability monitoring proposal (draft)

::: {.callout-important}
## Draft / sandbox — not an official determination

Published for open discussion. Not adopted County or LADWP policy; not a Technical Group finding. Some remote-sensing scores and field options are placeholders while tools and labeling catch up.
:::

**Status:** ICWD working draft for discussion with LADWP  
**Date:** 2026-07-24  
**Context:** Technical discussion 2026-07-22; LADWP pilot deck *Sustainability Presentation to ICWD* (LA watershed resources staff, 2026-07-23)  
**Deck:** local `reveg1999plan-docs/` (not in public push set)  
**Meeting notes:** local working notes (gitignored)

This note freezes a **shared metric**, predicts LADWP’s **remote-sensing** path from the pilot slides, and proposes a **slim field** path the County can agree to—including options that reuse existing t-posts if long legacy transects remain in force.

---

## 1. Shared decision rule (agree first)

From the 1999 Revegetation Plan (p. 2), as quoted in the LADWP deck:

> … cover is **90%** and composition is **75%** of the site-specific stated goal with an **80% confidence limit**. **At least 25% of vegetation cover must include recruits at least three years old that appear to have germinated without human intervention.**

### 1.1 Declaration — what \(R\) is

**\(R\) is a cover ratio, not a plant count and not a fraction of parcel area.**

\[
R = \frac{C_{\mathrm{recruit}}}{C_{\mathrm{live\ perennial}}} \;\geq\; 0.25
\]

| Symbol | Plain language |
|--------|----------------|
| \(C_{\mathrm{live\ perennial}}\) | **Cover** of live perennial vegetation on the parcel (annuals out) |
| \(C_{\mathrm{recruit}}\) | **Cover** of the recruit subset of those perennials (definition below) |
| \(R\) | Share of **live perennial cover** that is recruit cover — LA watershed resources staff pilot metric (e.g. 1.27% / 11.1% = 11.5%) |

So “25% of vegetation cover must include recruits…” means: **at least one quarter of the live perennial cover on the site is recruit cover.**

### 1.2 Declaration — project baseline (1999) and who counts as a recruit

**Baseline:** For sustainability scoring, **1999** (Plan / first monitoring) is the project start year unless LADWP and ICWD jointly document a site-specific alternative.

**Recruit (counts in \(C_{\mathrm{recruit}}\)):** natural perennial (no planting / human intervention) that

1. **established after the baseline year (1999)**, and  
2. is **at least 3 years old** at the scoring date.

**Does not count as recruit cover:**

- Plants (and canopy) that **pre-date 1999**, even on **unplanted** parcels — they recruited in nature at some point, but **before the project baseline**.
- **Lateral expansion** of those pre-1999 canopies after 1999 (more cover from an old individual ≠ new recruit).
- Annuals; planted stock; individuals **&lt; 3 years** old.

**Age window today (~2026):** countable recruits are about **3 to ~27 years** old — old enough for the Plan’s ≥3 yr rule, not older than time since baseline. Pre-1999 plants are **legacy** forever under this definition.

**Unplanted ≠ automatic pass.** IND105 / IND123 had no planting, but some cover likely existed before or at project start. Only cover from plants that **established since 1999** (and are ≥3 yr) enters \(C_{\mathrm{recruit}}\). Everything else stays in the denominator if live, not in the numerator.

| Term | Definition (proposed) |
|------|------------------------|
| Start year / baseline | **1999** |
| Compliance unit | **Area-weighted parcel average** — not every patch or stratum |
| Cover vs composition | Existing LPI / CI tests unchanged; sustainability is **additional** |
| Outcomes | **pass** / **fail** / **inconclusive** |

### 1.3 Implication for remote sensing

Pairwise “new since last flight” (e.g. 2022 vs 2017) estimates a **recent cohort**, not necessarily the full **post-1999** recruit stock (ages 3–27). A shrub that established in 2005 and appears in both 2017 and 2022 is post-baseline recruit cover under §1.2 but looks “old” in that pairwise test.

To align Track A with this declaration, RS should compare **current live cover to a near-baseline epoch** (≈1999/2000 imagery, or the earliest usable layer) — or otherwise accumulate post-baseline establishment — not only the last 3–5 year gap. Short-interval flights remain useful for **going-forward** persistence and concurrent ground work.

---

## 2. Track A — Remote sensing (what we expect LADWP / LA watershed resources staff to do)

LADWP has signaled preference for **post hoc** demonstration with high-resolution imagery, with ground work concurrent when new data are flown. The IND105 pilot implies the following operational path.

### 2.1 Predicted workflow (from slides 7–16)

1. **Baseline “current” live cover** from **~0.05 m (5 cm)** imagery **+ LiDAR** (pilot: August **2022**).  
   *(Slide 17 text says “&lt; 0.05 cm” — treat as typo for 0.05 m / 5 cm.)*
2. Georeference (spline).
3. Object recognition (eCognition / “eRecognition” on slide).
4. **Random Forest** → Bare / Live / Dead.
5. Merge and **explode** live-vegetation polygons.
6. Overlay year-of-interest live polys on a **prior epoch**. For alignment with §1.2, the prior epoch should be **near the 1999 baseline** (or an agreed surrogate) when scoring the full post-baseline recruit stock; short gaps (3–5 yr) score only a recent cohort (see §1.3).
7. Label **recruit polygons** as:
   - no overlap with prior live cover, **or**
   - overlap **&lt; 10%** of polygon area, **or**
   - prior feature undetectable (**&lt; 0.3 m**).
8. Compute  
   \(R = C_{\mathrm{recruit}} / C_{\mathrm{live}}\)  
   i.e. **recruit cover ÷ live perennial cover** — **not** ÷ parcel area (same definition as §1.1).

### 2.2 Pilot scores already shown (IND105)

| Epoch pair | Recruit share of live cover | Notes |
|------------|----------------------------|--------|
| 2022 vs 2017 | **11.5%** | Fail vs 25% |
| 2017 vs 2014 | **43.8%** unadj. / **49.7%** adj. | Pass after annual filter |
| 2014 vs 2009 | **13.7%** | Fail |

**Annual / phenology fix (slide 13–14):** 2017 RS live cover inflated by annuals; adjust using ground annual fraction (IND106 T10/T12 ≈ 25.7% → ×0.74) and/or require recruit polys to persist as live/dead in 2022.

### 2.3 LADWP recommendations we expect to see going forward (slide 17)

1. Acquire **~5 cm** imagery **with LiDAR** every **3–5 years**.
2. Conduct **ground monitoring concurrently** with those acquisitions.

### 2.4 County acceptance conditions for Track A (slim but bomb-proof)

Without these, RS remains a useful pilot, not a certifying method:

1. **Versioned SOP** — fixed thresholds (overlap %, min size, epoch gap), perennial rules, start year, image years used, annual adjustment recipe.
2. **Field audit** — small stratified sample (~30 stems) of “recruit” vs “legacy” polygons: confirm stem location vs prior live footprint + size class; report agreement. Below agreed agreement → **inconclusive**, not pass.
3. **Sensitivity appendix** — same parcel under nearby threshold choices; large swing → inconclusive or revise SOP.
4. **Deliverables** — recruit and live polygon layers (or equivalent) for the scored year, so ICWD can reproduce \(R\).

**Roles:** LADWP produces RS scores and polygons; ICWD jointly owns the metric definition, SOP acceptance, and audit.

---

## 3. Track B — Field (cost-effective, same \(R\))

Goal: something **faster than** the 2023 permanent 1×1 m plot letter, **more diagnostic than** long first-hit lines alone, and **compatible** with Track A.

### 3.1 Why legacy long transects are weak for sustainability

- First-hit LPI cannot separate **new individuals** from **lateral growth** of the same shrub (County point on the LADWP “Original Thoughts” slide).
- Very long segments (to ~100 m) oversample empty ground, inflate spatial autocorrelation, and—when hits are **tallied without distances**—lose point identity across years.
- Dominant cover on IND105 / IND123 is **Atriplex** (ATTO, ATPO, etc.). Woody chenopods have **anomalous secondary growth**; routine **ring counts are not reliable** for aging. Dendro calibration is appropriate for *Artemisia* (and maybe rabbitbrush after a local pilot), not as the saltbush workhorse.

### 3.2 Preferred field design (if layouts can be revised)

- **Few short lines** (e.g. 25 m or 50 m), **stratified** on current live pattern from RS (or high/med/low live cover).
- **Area weights** \(W_h\) so estimators return the **parcel average** (compliance target)—not a requirement that every stratum clear the bar.
- On each line, one pass:
  - **LPI** every 0.5 m with **point IDs recorded** (cover / composition if needed), and
  - **Skinny belt** (e.g. 0.5–1 m total width): perennial stems by **size class**, and/or  
  - At live hits (or every *k*th): **stem in prior-live footprint?** (tablet overlay of Track A polys).
- Optional: tag ~30 plants (stratified new/old) for **3-year persistence** when a cohort appears.
- **Do not oversample** — allocate just enough lines for an acceptable SE on parcel \(R\) (and cover if those lines also serve compliance). Stratification usually cuts total line-meters vs one long-line design.

### 3.3 If we are stuck with existing t-posts

Reuse infrastructure; change **what we measure** and **how much length we walk**.

| Option | Idea | Pros | Cons |
|--------|------|------|------|
| **A. Shorten** | Keep bearing; sample only first **25 m** or **50 m** from a post (or mid-segment window) | Same posts; less empty walking; point IDs feasible | Partial reuse of historic length series |
| **B. Radiate** | From existing posts, short rays (e.g. 10–25 m) in 2–3 fixed bearings | Good local spatial sample; posts already permanent | New geometry to document; not a 1:1 revisit of old lines |
| **C. Attribute-only add-on** | Walk full historic lines for cover continuity, but at live hits only record legacy vs recruit (prior-live checkbox) | Minimal redesign; directly feeds \(R\) | Still long walks; lateral issue only solved if stem vs prior footprint is checked |
| **D. Subsample segments** | Random or stratified subset of existing segments; full protocol on those only | Cuts field days hard | Need agreed rule so subset still represents parcel |

**Recommendation under constraint:** prefer **shorten or radiate from existing posts** + belt / hit-attribution for \(R\); keep a **thin** link to historic LPI (subset of long lines or shortened same bearing) so cover time series is not abandoned overnight. Coordinate with LADWP’s stated interest in **revising layouts**.

### 3.4 What not to rely on for IND105 / IND123

- Size class → age via **Atriplex ring counts** (anatomy).
- “New hit on old transect” = recruit (lateral growth).
- Stem counts without cover weighting (few large legacy shrubs dominate cover).

**Better calibrators for “post-1999”:** prior-live footprint from imagery; size class as a coarse prior; persistence tags; rings only where species support them.

---

## 4. How the two tracks fit together

```text
                    ┌─────────────────────────────┐
                    │  Parcel R ≥ 25%             │
                    │  (same definition)          │
                    └─────────────┬───────────────┘
                                  │
              ┌───────────────────┴───────────────────┐
              │                                       │
   Track A — RS (post hoc + future)        Track B — Field (forward)
   LADWP production                        ICWD / joint crews
   5 cm + LiDAR, overlap rules             Short / radiated lines + belt
   Annual filter, polygon deliverables     or hit-attribution on posts
              │                                       │
              └─────────── Field audit (~30) ─────────┘
                           (agreement gate)
```

- **Retrospective / low-hanging sites** (cover & composition already met): lead with **Track A**; County certifies only with SOP + audit.
- **Going forward:** fly RS on the 3–5 year cycle; run **Track B** in the same season; stop treating 100 m tally-only LPI as a sustainability method.

---

## 5. Pilot sequence (aligned with LADWP priorities)

1. Jointly freeze metric + Track A SOP + audit protocol (this document → signed methods note).
2. **IND105** — Stu: cover/comp already met; finish sustainability demo or call inconclusive.
3. **IND123** — push toward fully complete (cover/comp + sustainability).
4. Then other parcels that are ⚠️ on cover/comp met without sustainability.

---

## 6. Open items for LADWP / ICWD agreement

- [ ] Confirm **§1.1–1.2**: \(R\) = recruit **cover** / live perennial **cover**; baseline **1999**; pre-baseline and lateral expansion of legacy out; unplanted ≠ all recruits.
- [ ] Confirm whether Track A for certification uses **baseline-near imagery** (full post-1999 stock) vs last-interval pairs only (recent cohort).
- [ ] Written Track A thresholds and perennial / annual rules.
- [ ] Audit sample size and **minimum agreement** for certification.
- [ ] Whether shortened / radiated geometry is acceptable while long lines remain for interim cover.
- [ ] Delivery of IND105 (and later) **recruit / live polygon** layers to ICWD.
- [ ] Concurrent schedule: next ~5 cm + LiDAR flight ↔ field season.

---

## 7. One-sentence summary

**\(R\) is the share of live perennial cover that is post-1999 natural recruit cover (≥3 yr); LADWP estimates it from imagery under a frozen SOP, the County accepts when a small field audit agrees, and going forward we measure the same cover ratio on short or post-radiating belts—not by treating every plant on an unplanted parcel as a recruit.**
