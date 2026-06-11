# geoChronR

Age-uncertain chronology development and analysis for paleogeoscientific data.

As of version 2.0, geoChronR is split into three focused packages:

| Package | Role |
|---|---|
| **geoChronR** (this repo) | Age modeling — `runBacon()`, `runBchron()`, `runOxcal()`, `runBam()` — plus LiPD model infrastructure (`createModel()`, `createChronMeasInputDf()`, `calculateFlux()`) |
| [ens](https://github.com/nickmckay/ens) | Ensemble methods and calculations — `corEns()`, `regressEns()`, `pcaEns()`, `computeSpectraEns()`, binning, `selectData()` |
| [lipdViz](https://github.com/nickmckay/lipdViz) | Visualization — `plotChronEns()`, `plotTimeseriesEnsRibbons()`, `plotCorEns()`, maps, and more |

**geoChronR imports and re-exports the complete ens and lipdViz APIs**, so
`library(geoChronR)` still provides the full geoChronR 1.x interface — existing
scripts and vignettes work unchanged. If you only need ensemble calculations or
plotting, depend on `ens` or `lipdViz` directly for a much lighter footprint.

## Installation

```r
remotes::install_github("nickmckay/geoChronR-chronOnly")
```

## Getting started

See the [Introduction vignette](vignettes/Introduction.Rmd) for the full
workflow: load LiPD data, build age models, map age ensembles to paleo
measurements, and analyze with ensemble methods.
