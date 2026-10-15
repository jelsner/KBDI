# Long-term drought change near Apalachicola National Forest

The active paper workflow centers on long-term station-derived drought change,
with seasonal wildfire prediction as an application. The refreshed evidence
and a proposed paper structure are in `manuscript/revision-plan.md`; a standalone
LaTeX evidence summary is in `manuscript/analysis-summary.tex`.

## Run the paper analysis

Use R >= 4.1 and run from the repository root:

```r
install.packages(c("sf", "terra", "MASS", "jsonlite", "ggplot2", "sandwich",
                   "dplyr", "rmarkdown", "knitr"))
```

```sh
Rscript scripts/run-all.R             # reproduce with cached snapshots
Rscript scripts/run-all.R --refresh   # update station, fire, and SOI data
```

R Markdown rendering requires Pandoc, bundled with RStudio. Package versions
and dependencies are pinned in `renv.lock`, which can be restored with
`renv::restore()`. System spatial libraries required by sf/terra are not
installed by this workflow. Nothing installs packages or runs Stan automatically.
RStudio workspace restoration is disabled so saved `.RData` cannot supply
hidden state. The original workspace files remain intact.

`run-all.R` rebuilds the data, count models, climate analyses, robustness checks,
historical PRISM comparison, generated paper summary, tests, and four HTML
reports. Generated files will be replaced on rerun; edit the `.in` manuscript
templates to retain editorial changes to the generated summaries.

## Active notebooks

- `KBDI.Rmd`: primary drought-change results, seasonality, uncertainty, and robustness.
- `ANF_Fires.Rmd`: updated fire data and descriptive application context.
- `PredictionModel.Rmd`: corrected count targets, preceding-year hindcasts,
  trend baselines, and paired skill uncertainty.
- `Gridded_KBDI.Rmd`: historical PRISM station-representativeness check.

Their historical versions and the original hydrology functions are preserved
in `legacy/`. Peripheral exploratory notebooks (WeatherSTEM, personal weather,
Zach thesis/fire analyses, generic temperature and day-of-year experiments,
and early gridded download experiments) remain unchanged in the root for
reference. They are not part of the refreshed paper pipeline and are not
asserted to run with current dependencies. No pre-existing Git deletions,
untracked datasets, or historical figure files are restored or removed.

## Sources and data scope

- [NOAA GHCN-Daily](https://www.ncei.noaa.gov/pub/data/ghcn/daily/readme.txt),
  station USW00093805 (Tallahassee airport): PRCP, TMAX, TMIN. The initial updated
  source snapshot was imported from the verified ANF refresh on 2026-10-15 UTC;
  `--refresh` retrieves new data directly. Raw observations extend into 2026,
  but climate analyses exclude partial calendar years. Effective endpoints
  differ by outcome because complete periods are required.
- [Short (2026), FPA FOD seventh edition](https://doi.org/10.2737/RDS-2013-0009.7),
  1992–2024. The pipeline retrieves IDs in the study bounding box, checks batch
  completeness, then selects unique ignition points intersecting ANF.
- [NOAA PSL SOI](https://psl.noaa.gov/data/correlation/soi.data): replaces the
  dead NCEI SOI CSV endpoint. -99.99 sentinels are missing.
- `Data/raw/anf-boundary.gpkg`: extracted from this repository's original
  NFSLandUnit shapefile. The fixed study footprint is preserved; it is not a
  refreshed administrative boundary.
- [PRISM](https://prism.oregonstate.edu/): original local stable 4-km daily
  precipitation/maximum-temperature archive, 2009–2018. It has been recomputed
  with terra and an explicit forest footprint; observations are not extended
  beyond 2018. A compact forest extraction reproduces the comparison without
  the continental archive. To rebuild it, use:

```sh
Rscript scripts/prism-check.R --reextract
```

Original source files under `Data/` remain intact. Updated downloads are in
`Data/raw/`; derived tables are in `Data/processed/`. Existing
`SeasonalForecastModelData.csv` is retained as a historical artifact; new
models read the derived tables instead.

## Statistical choices

KBDI is a weather-based index, not observed soil water. The corrected code
handles 0.2-inch rainfall interception per wet spell, subtracts net rain before
the drying increment, bounds KBDI to 0–800, and rejects invalid inputs. The
main analysis uses initial Q=400, annual mean rainfall R=60 inches, and 365
consecutive complete days after initialization or a weather gap. Nonblank GHCN
quality flags are rejected; missing rain is not set to zero. Trace rain uses
its reported numerical value. Complete eligible months/years enter trends.

Trend intervals use lag-3 Newey–West covariance on annual observations, without
prewhitening, and a t reference. Lag is measured in retained annual observations;
missing years create irregular spacing. Monthly p-values are adjusted across
12 months for each outcome and endpoint, and annual p-values across five
outcomes per endpoint. Later-start records and covariance lags 0/1/3/5 are
reported as sensitivity analyses. KBDI sensitivity covers 27 combinations of
rainfall (55/60/65 inches), initial Q (0/400/800), and spin-up (180/365/730 days).

The fire application fits separate negative binomial count models by cause,
using only earlier eligible seasons in each hindcast, with 15 minimum training
seasons. All comparisons use the same response and evaluation years.
Climatology, drought, year trend, and drought-plus-year models are compared.
Paired skill intervals use 2,000 circular moving-block bootstrap samples with
three-year blocks and a fixed seed. They do not rerun the full estimation or
model-selection process. Conditional NB prediction intervals omit parameter
uncertainty. The small evaluation sample limits comparisons among models.

PRISM correspondence is calculated on aligned dates after spin-up. Both raw
R² and calendar-day-anomaly R² are reported, with forest intersection-area
weights. PRISM uses station observations, so this is not independent validation.
An 80-year forest-wide trend or a causal climate-change attribution requires
additional evidence. Unhomogenized station history and complete-case selection
remain material limitations.

## Outputs and verification

- `outputs/KBDI.html`: main drought report.
- `outputs/ANF_Fires.html`, `PredictionModel.html`, `Gridded_KBDI.html`.
- `outputs/annual-trends.csv`, `monthly-trends.csv`, `enso-adjusted-trends.csv`.
- `outputs/kbdi-sensitivity.csv`, `period-covariance-sensitivity.csv`,
  `algorithm-trend-sensitivity.csv`.
- `outputs/metrics.csv`, `forecast-check-metrics.csv`, `forecast-skill-uncertainty.csv`.
- `outputs/manifest.json`, `climate-manifest.json`, `prism-manifest.json`,
  `session-info.txt`: provenance, settings, and software versions.
- `tests/`: rainfall/KBDI, data alignment, seasonal counts, forecast windows,
  climate completeness, multiplicity, and spatial integrity checks.

`scripts/bayesian.R` is an optional extension requiring brms and a working Stan
compiler. It has not been run during modernization. The original exploratory
burned-area model is preserved rather than asserted to be a validated forecast.

The existing Overleaf paper could not be accessed from the available browser.
The revision plan and generated summary are proposals based on verified updated
results; the actual manuscript and bibliography still need reconciliation.
