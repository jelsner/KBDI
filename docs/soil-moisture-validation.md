# Independent soil-moisture comparison: Wakulla #1 SCAN

Assessment date: 2026-10-15. Status: exploratory comparison included in the preprint supplement.

## Source and relevance

The USDA NRCS Wakulla #1 SCAN station (`2009:FL:SCAN`) is at
30.30585 N, 84.42477 W in official AWDB metadata. It intersects the project's
fixed ANF polygon and is 12.03 km from the Tallahassee airport weather station
(distance calculated with `sf`). This is a local in-situ observation record,
independent of the airport weather inputs and the KBDI equation. Its spatial
representativeness for the entire forest is untested.

Sensors measure volumetric soil moisture at 2, 4, 8, 20, and 40 inches
(5.08, 10.16, 20.32, 50.8, and 101.6 cm). The first four series start on
1996-12-10; the 40-inch series starts on 1997-04-29. The downloaded snapshot
ends on 2026-10-12, matching the weather snapshot. Coverage is discontinuous.
NRCS says that soil moisture is derived using a loam equation; it does not
publish further maintenance notes on the station notes page. Absolute water
contents and apparent shifts need scrutiny before interpreting trends.

Unlike comparison against PRISM-derived KBDI, this tests correspondence
with measured soil moisture. Following the motivation in Hoylman et al. (2024),
it evaluates each depth and correspondence beyond broad seasonality.
It is not a replication of their standardized soil moisture index methodology.

## Reproduce

```sh
Rscript scripts/soil-moisture-check.R           # fixed local snapshots; no network
Rscript scripts/soil-moisture-check.R --refresh # redownload through 2026-10-12
```

This diagnostic and its supplementary exports are included in `scripts/run-all.R`. Refreshing
preserves the fixed end date but may retrieve provider revisions. Raw JSON,
URLs and checksums are in `Data/raw/scan-wakulla/`. The paired data, both QC and
QA flags, sensor dates and nominal KBDI dates are retained in
`Data/processed/wakulla-soil-moisture-kbdi.csv`. A missing date is not imputed;
annual coverage includes years with no returned observations.

## Comparison rules

- NRCS `SMS` is instantaneous: the DAILY product selects the midnight
  observation, not a daily average. Pair observation date D with nominal
  end-of-day KBDI for D-1. A same-date comparison is also reported because
  the airport's daily observation window may differ from local midnight.
  No lag is selected to maximize correlation. Station time zone is UTC-5.
- Primary results retain QC flag `V` only, physically bounded 0–100% values,
  and KBDI after at least 365 complete weather days. The API filters suspect
  data (`returnSuspectData=false`). A separate sensitivity includes `N`
  (no automated validation profile). `E` and other flags are excluded.
  A V flag alone does not establish sensor homogeneity or absence of drift.
- Report Pearson and Spearman raw correlation, Pearson correlation after
  separately subtracting each variable's month-of-year mean on paired dates,
  and correlation after centering within each year/month. The latter tests
  shorter-term correspondence while removing persistent monthly offsets.
  These are in-sample descriptive comparisons, not calibrated predictions.
- All months, May, and May–July are reported for the full record and 2007–2024
  sensitivity period. Depths are not selected based on the best correlation.
- The monthly plot uses only months with at least 80% of calendar days paired;
  gaps are drawn as breaks. Correlations use available eligible daily pairs.
- Do not interpret ordinary daily correlation p-values: observations are
  serially dependent. This exploratory output reports no p-values or
  confidence intervals. Formal inference would require temporal resampling
  and explicit treatment of sensor changes.

## Initial results

Primary, all-month, full-record results (negative r is the expected sign):

| Depth (cm) | Paired days | Raw r | Month-anomaly r |
|---|---:|---:|---:|
| 5.08 | 7,617 | -0.230 | -0.175 |
| 10.16 | 7,367 | -0.114 | -0.092 |
| 20.32 | 7,783 | -0.348 | -0.308 |
| 50.80 | 8,124 | -0.381 | -0.349 |
| 101.60 | 7,655 | -0.416 | -0.423 |

Validated paired observations extend to midnight 2024-09-23, matched with
KBDI on 2024-09-22. Later returned observations have N flags and are confined
to the sensitivity analysis. The 2007–2024 month-anomaly correlations range
from approximately -0.20 to -0.34 across depths. Same-date correlations and
May-only results are available in `outputs/soil-moisture-correspondence.csv`.

The correspondence is modest and varies with depth, period, and timing.
The observations do not support calling KBDI a directly measured soil-water
deficit. KBDI in mm and volumetric moisture in percent are different quantities;
no absolute-mm bias or RMSE is meaningful without a separately justified
soil profile, storage capacity and calibration.

The time-series plot exposes pronounced level changes, especially around
2001 at 10 cm, 2017 at 5 cm, and 2020–2022 at 51 cm. These are review targets,
not diagnosed sensor failures. Do not remove them solely because agreement
improves. Obtain maintenance/calibration history or independent evidence
before defining exclusions or fitting segmented adjustments.

This record can test recent drought variability and seasonal timing, but
cannot independently verify the 1944-onward station-derived drought trend.
The preprint includes this comparison as an exploratory supplement, with
its limitations stated. Before making a stronger validation claim, review
sensor histories, use a predefined seasonal anomaly method and temporal
uncertainty estimates, and retain the single-site and depth limitations.

## References and provider documentation

- [Wakulla station information](https://wcc.sc.egov.usda.gov/nwcc/site?sitenum=2009).
- [Sensor definitions and midnight timing](https://wcc.sc.egov.usda.gov/nwcc/sensors?sitenum=2009).
- [Site notes: loam equation](https://wcc.sc.egov.usda.gov/nwcc/sitenotes?sitenum=2009).
- [NRCS SCAN overview](https://www.nrcs.usda.gov/resources/data-and-reports/soil-climate-analysis-network).
- [AWDB REST API](https://wcc.sc.egov.usda.gov/awdbRestApi/swagger-ui.html).
- Hoylman, Z. H., Holden, Z., Bocinsky, R. K., Ketchum, D., Swanson, A., and
  Jencso, K. (2024). Optimizing drought assessment for soil moisture deficits.
  *Water Resources Research*, 60, e2023WR036087.
  https://doi.org/10.1029/2023WR036087.
