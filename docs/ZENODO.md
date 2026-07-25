# Zenodo snapshots (annual versions)

**Goal:** one DOI family for the living repo, with a **new version each year**, and almost no manual upload UI work.

## How it works

[Zenodo’s GitHub integration](https://help.zenodo.org/docs/github/enable-repository/) watches **GitHub Releases**. When you publish a release:

1. Zenodo pulls the release ZIP (public repo contents at that tag).  
2. It mints a **version DOI** for that snapshot.  
3. A stable **concept DOI** always points at the latest version (cite the project) or you cite a specific version DOI (cite that year’s snapshot).

You do **not** re-upload files in the Zenodo web form each year if this link is enabled.

## One-time setup (human, ~10 minutes)

1. Sign in at [zenodo.org](https://zenodo.org) with a GitHub account that can see `inyo-gov/revegetation-projects` (org ownership or admin). Prefer an **ICWD / inyo-gov** Zenodo identity if you have one, so DOIs don’t sit under a personal account.  
2. Zenodo → account → **GitHub** → **Sync now**.  
3. Find `inyo-gov/revegetation-projects` → toggle **On**.  
4. Confirm metadata in repo root: `.zenodo.json` (Zenodo prefers this) and `CITATION.cff` (GitHub “Cite this repository”).

First release creates the concept DOI + v1. Later releases become versions automatically.

## Each year (low touch)

```bash
# 1. Bump version strings in .zenodo.json and CITATION.cff (e.g. 2026.1.0)
# 2. Commit, merge to main
# 3. Tag and release (GitHub UI or gh):
gh release create v2026.1.0 --title "2026 annual snapshot" --notes "Survey year 2026; see WORKLOG / paper for changes."
```

Zenodo then archives that tag and issues the new version DOI. Optional: paste the version DOI back into `CITATION.cff` / paper after minting (nice-to-have, not required for the next release).

## What gets archived

Only what is **in the public git tree** at the tag (same as the GitHub release ZIP). Things in `.gitignore` (large raw workbooks, secrets) are **not** on Zenodo. If a year’s snapshot must include specific data files, commit them (or attach release assets) before tagging.

## Optional further automation

- GitHub Action that creates a dated release when you merge an `annual-release` PR (still one intentional merge/tag, no Zenodo UI).  
- Mirror the same pattern already used on [`hydro-data`](https://github.com/inyo-gov/hydro-data) (`zenodo.json` / `CITATION.cff`).

## Citing

- **Living project:** concept DOI (once minted; update `paper/paper.md` and About).  
- **Specific year:** version DOI from that release.
