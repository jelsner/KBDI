# Reproducing the revised paper

Use R >=4.1 and Pandoc. Required packages are sf, terra, MASS, jsonlite,
ggplot2, sandwich, dplyr, rmarkdown, and knitr; exact versions and dependencies
are in `renv.lock`. The workflow does not install packages automatically.

```sh
Rscript scripts/run-all.R              # cached analysis, tests, reports, paper assets
Rscript scripts/run-all.R --refresh    # refresh NOAA station, FPA FOD, and SOI sources
```

Run from the directory containing `KBDI.Rproj`. Current reports are under
`analysis/`; `scripts/render.R` sets the project root explicitly so moving the
notebooks does not change data or image lookup. The NOAA refresh URL uses the
case-sensitive `/pub/data/` path. RStudio workspace restoration remains off.

## Workflows

| Script | Purpose |
|---|---|
| `pipeline.R` | Source snapshots, ANF selection, daily KBDI, seasonal counts, primary count models |
| `climate.R` | Complete annual/monthly summaries, trend intervals, adjusted p-values, ENSO, KBDI settings |
| `robustness.R` | Record/covariance sensitivity and original-algorithm comparison |
| `forecast-checks.R` | Year baselines, paired skill bootstrap, antecedent-burn check |
| `prism-check.R` | Historical spatial comparison from the compact forest extraction |
| `paper-tables.R` | Publication tables and common-year high-deficit-threshold check |
| `paper-figures.R` | Four main and three supplemental figures in PDF and PNG |
| `render.R` | Four analysis HTML reports |
| `render-manuscript.R` | Self-contained HTML previews with resolved citations and MathML equations |

`paper-figures.R` can run independently after analysis results exist; it makes
no network requests. To change figure appearance, edit that script and rerun
it. Markdown text remains editable and is not overwritten by reproduction.
Reconcile its numerical statements after any new data or model changes.

## Inputs and provenance

`Data/raw/` contains compact GHCN station, fire-service, SOI, and fixed-boundary
snapshots plus the historical PRISM forest extraction. The source checksums and
settings are recorded in `outputs/manifest.json`, `climate-manifest.json`,
`prism-manifest.json`, and `manuscript/figures/manifest.json`. Exact software
versions used are recorded in `outputs/session-info.txt`. Other large historical
inputs remain local under `Data/` and are ignored by Git. Compact raw and
processed data are permitted by the revised ignore rules, making them available
for a future reproducible Git commit without adding the continental archive.

To rebuild the historical PRISM extraction from the local 2009-2018 archive:

```sh
Rscript scripts/prism-check.R --reextract
```

This does not extend PRISM beyond 2018. Original source archives and saved
sessions are preserved in `archive/`; archived notebooks are not repaired or
executed. `scripts/bayesian.R` remains an optional, unrun extension requiring
brms and Stan. The original burned-area exploratory model is not a validated
forecast in this revision.

## Statistical interpretation

The main calculation uses initial KBDI=400, fixed mean annual rain=60 inches,
and 365 consecutive complete days before an estimate is eligible. Rainfall is
not imputed; nonblank quality flags are rejected. Only complete eligible months
and years enter corresponding trends. KBDI is an index proxy, not observed soil
water or a homogenized forest-wide series.

Trend uncertainty uses lag-3 Newey-West covariance on retained annual
observations and a t reference. Intervals are pointwise; p-values are adjusted
across 12 months per outcome/endpoint and five annual outcomes per endpoint.
Sensitivity fits cover later record starts, covariance lags, index settings,
and high-deficit thresholds. The original 160-mm threshold is included on the
same 69 annual periods as the primary >650 KBDI result.

Forecasts use only preceding eligible seasons. The natural-fire application
has 15 evaluation years. Paired error intervals use 2,000 circular three-year
block bootstrap samples; they do not rerun full estimation/model selection.
Final archive revisions and publication latency are not reconstructed. These
are retrospective hindcasts, not operationally issued forecasts.

See the manuscript for the full methods, results, limitations, and references.
