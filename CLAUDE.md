# CLAUDE.md — geoChronR (2.0, chron-only)

This repo is **geoChronR 2.0**: the age-modeling core of a five-package paleogeoscience family
(split out of the monolithic geoChronR, June 2026). It keeps only age modeling + LiPD model
infrastructure, and **re-exports the full ens + lipdViz APIs** so `library(geoChronR)` still
provides the complete 1.x interface — existing user scripts keep working.

## Package family (dependency DAG: ens ← lipdViz ← geoChronR; actR & compositeR on top)

| Repo (`~/GitHub/...`) | GitHub | Branch | Role |
|---|---|---|---|
| ens | nickmckay/ens | main | Ensemble methods + UQ engine |
| lipdViz | nickmckay/lipdViz | main | Plotting + mapping |
| **geoChronR-chronOnly** (this repo) | nickmckay/geoChronR-chronOnly | main | geoChronR 2.0: age modeling; re-exports ens+lipdViz |
| actR | **LinkedEarth/actR** | refactor | Abrupt-change detection |
| compositeR | nickmckay/compositeR | refactor | Record compositing |

## What lives here

- Age models: `R/bacon.lipd.R`, `R/run.bchron.LiPD.R`, `R/oxcal.R`, `R/BAM.lipd.R` (the
  LiPD-level `runBam()` wrapper; the computational `simulateBam`/`bamCorrect` moved to ens).
- LiPD model infra: `R/createModel.R`, `R/chronBuilding.R`, `R/flux.R`.
- **`R/reexports.R`** — re-exports every ens + lipdViz export. **GENERATED**; do not hand-edit.
  After ANY ens/lipdViz export change, regenerate with `Rscript tools/gen_reexports.R` (run
  from this repo root) and `devtools::document()`.

## Gotchas

- `@inheritParams ens::selectData` style for params defined in ens (the function moved there).
- Depends on heavy age-model software (rbacon, Bchron, oxcAAR) — this is the ONLY repo in the
  family that does; downstream actR/compositeR deliberately do not.
- CI: R CMD check on Windows/Linux/macOS with vignettes.

## Dev

`devtools::load_all()` · `devtools::document()` · `devtools::test()` · `devtools::check()`.
Needs `ens` and `lipdViz` installed from source first; push order for CI Remotes resolution is
ens → lipdViz → geoChronR. Commit work when complete.
Co-author trailer: `Co-Authored-By: Claude Fable 5 <noreply@anthropic.com>`.
