---
title: "State-and-Transition Thinking and NMDS for Laws Type E Revegetation (draft)"
authors:
  - name: "Inyo County Water Department"
    affiliation: "Inyo County Water Department, Bishop, California, USA"
date: today
keywords:
  - state-and-transition model
  - NMDS
  - Type E transfer
  - abandoned agriculture
  - ecological site
  - gravelly loam
  - Owens Valley
  - open science
abstract: |
  Laws Type E transfer parcels were removed from irrigated agriculture and are managed toward precipitation-dependent native communities under the 2003 revegetation plan. Compliance monitoring (cover, composition, species caps) does not by itself describe ecological states or pathways. We outline how state-and-transition models (STMs) and non-metric multidimensional scaling (NMDS) — methods already used in ICWD’s Owens Valley STM work — apply to these sites. The central conceptual addition is an explicit abandoned-agriculture (ABAG) state: bare or eroded ground after irrigation ceases and topsoil is lost to wind, which stock ecological-site STMs for gravelly/loamy arid sites usually omit. NMDS of parcel–year relative species composition places reference scrub parcels and revegetation parcels in a common Bray–Curtis space so recovery toward reference-like composition can be visualized alongside plan goals. This paper is the citable companion to the STM and NMDS pages on the revegetation-projects site.
format:
  html:
    output-file: paper_stm_nmds.html
---

::: {.callout-important}
## Draft / sandbox — not an official determination

This manuscript is published in the spirit of **open science**: methods, drafts, and incomplete analyses stay visible while tools and labeling catch up. Treat it as a **sandbox**, not adopted County or LADWP policy, not a Technical Group finding, and not a certified compliance product.

Some tables, figures, or joins may be **placeholders** or sparsely labeled while pipelines are prototyped. Prefer the living repository over any single screenshot; challenge numbers by reading the code and sources.
:::

## Introduction

Revegetation success under multi-party water agreements is often scored with **percent cover** and **species lists**. Those metrics answer compliance questions; they do not fully answer **what state the site is in** or **which transitions are plausible**. State-and-transition models (STMs) organize plant communities, soils, and drivers on an **ecological site**; non-metric multidimensional scaling (NMDS) tests whether field compositions form coherent clusters that match those states [@McCuneGrace2002; @Bestelmeyer2017].

ICWD maintains an Owens Valley STM program (repository `stm`) that treats Green Book vegetation types as management-relevant states within NRCS ecological sites and uses NMDS on line-point composition to evaluate type separation. This paper applies that framing to **Laws Type E transfer** parcels (LAW090, LAW094, LAW095, LAW118/129) and their **reference** parcels.

## Ecological site and the missing state

The 2003 plan’s LAW118/129 planting guidance cites an NRCS ecological site described as **“gravely loam,”** including cover limits that motivate ATPO capping. Majority-component SSURGO attribution places LAW090, LAW094, LAW095, LAW118, and LAW129 on **Gravelly Loam 5-8" P.Z.** (**R029XG009CA**; NV Provisional twin **R029XY087NV**). That site is Tier 3 in ICWD’s `stm` catalog (no draft STM page yet); upland pages such as **Loamy 5-8" P.Z.** remain the closest narrative template. Those models emphasize grazing, drought, shrub dominance, and annual invasion among **native or lightly disturbed** phases.

Laws revegetation parcels are mapped in the Green Book as **Type A · Barren Lands – Abag**. Their history is **irrigated agriculture followed by retirement from irrigation**, often with **topsoil loss**. That condition is not a standard box on stock gravelly/loamy STMs. We therefore propose an explicit **ABAG (abandoned agriculture)** state: precip-only barren/eroded land that is the typical **start** of Type E transfer revegetation, distinct from native barren or intact shrub states.

```
Irrigated ag (E) → stop irrigation / erosion → ABAG → assisted early seral → reference-like Type A scrub
```

Failure pathways (chronic weeds, repeated drought, abandonment of maintenance) can return early seral stands toward ABAG; the plan’s option to revert to irrigated agriculture is a separate management branch.

## NMDS for reference vs revegetation

We ordinate **parcel–year** samples built from Laws revegetation workbooks and reference long tables. Species covers are aggregated within parcel–year, **row-normalized** to relative composition, rare species dropped, and Bray–Curtis dissimilarities mapped with `metaMDS` (same core steps as `stm/code/nmds_functions.R`). Absolute cover is reserved for compliance dashboards; NMDS asks whether **composition** of revegetation parcels approaches the **reference cloud**.

Live analysis: [NMDS page](https://inyo-gov.github.io/revegetation-projects/nmds.html). Conceptual STM: [STM page](https://inyo-gov.github.io/revegetation-projects/stm.html).

## Relation to plan goals and caps

Cover and composition goals, ATTO/ERNA caps, and rest periods remain the **compliance** layer. STM/NMDS are the **ecological narrative** layer: ABAG → early seral → reference-like scrub. Caps should eventually be interpreted as constraints on transitions (e.g., leaving assisted irrigation only when composition persists in a dry year), not as substitutes for state definitions. Public summary of shared direction vs the open cap question: [goals & caps summary](https://inyo-gov.github.io/revegetation-projects/amendment_summary.html).

## Open work

1. Draft a **Gravelly Loam 5-8"** STM page in `stm` (R029XG009CA / R029XY087NV) and cross-link; ESD IDs for Type E reveg are already in `data/processed/laws_type_e_esd.csv`.  
2. Join ESD to **reference** parcels on the map popup (reveg majority-component known).  
3. Extend LAW118/129 multi-year samples for trajectories.  
4. Represent bare ground explicitly in ordinations so ABAG barrenness is not invisible under relative cover alone.  
5. Optional PERMANOVA / envfit tests mirroring `stm/analysis/nmds_gb_types.qmd`.

## Data and code availability

Source and Quarto site: [inyo-gov/revegetation-projects](https://github.com/inyo-gov/revegetation-projects). STM/NMDS helpers: `code/nmds_laws.R`. Upstream methods: [inyo-gov/stm](https://github.com/inyo-gov/stm).

## References
