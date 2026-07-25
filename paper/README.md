# Manuscript — revegetation living repository

PeerJ-style data paper companion to the working dashboards in this repo.

## Render

```bash
cd paper
quarto render                          # both manuscripts → docs/
quarto render paper.md --to html       # → docs/paper.html
quarto render stm_nmds_type_e.md --to html  # → docs/paper_stm_nmds.html
quarto render paper.md --to docx
```

## Role

| Artifact | Role |
|----------|------|
| Site (`index`, `reference`, `stm`, `nmds`, `reveg1999plan`) | Living analytics / compliance + STM/NMDS |
| `paper/paper.md` | Citable narrative: sources, methods, gaps, open science framing |
| `paper/stm_nmds_type_e.md` | STM + ABAG state + NMDS framing for Type E |
| `WORKLOG.md` / meeting notes | Day-to-day decisions |

Update statistics and open-issue lists in the papers when survey years or methods change; keep the site as the source of truth for current tables and figures.
