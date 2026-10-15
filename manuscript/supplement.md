---
title: "Supplement: Long-term station-derived drought change near Apalachicola National Forest"
date: "Revised draft, 15 October 2026"
bibliography: references.bib
link-citations: true
lang: en-US
---

# Daily structure of the station record

![**Figure S1. Daily station-derived moisture deficit.** KBDI-equivalent deficit on eligible days in complete candidate calendar years through 2025. Missing and spin-up-affected days are blank. Calendar dates are aligned to a leap-year reference so February 29 is represented explicitly; it is not treated as a common non-leap-year day. The display describes daily structure, whereas trend inference uses complete monthly and annual summaries.](figures/figS01-daily-drought.png){width=100%}

# Record-period and covariance sensitivity

![**Figure S2. Sensitivity to record start and covariance lag.** Point estimates and pointwise 95% intervals for annual mean deficit and high-deficit days, using requested record starts of 1944, 1960, and 1980 and annual covariance lags of 0, 1, 3, and 5. The first eligible annual KBDI record in the full calculation is 1945. High-deficit days use KBDI greater than 650. Actual sample sizes vary with the retained record.](figures/figS02-period-covariance-sensitivity.png){width=100%}

Complete estimates are in [Supplementary Table S5](tables/tableS05-record-covariance.csv).
HAC lags apply to retained annual observations, not a regularized calendar-year
series. These are sensitivity fits on overlapping records, not independent
replications.

# Historical PRISM correspondence

![**Figure S3. Airport-forest daily anomaly correspondence.** Squared correlation between each PRISM cell's KBDI-derived deficit and the station-derived deficit after removal of each location's mean calendar-day seasonal cycle. The polygon outlines the fixed ANF footprint and the star marks the airport station. The original 2009-2018 PRISM archive is used with complete-day spin-up; it has not been extended. Grid cells are shown at their native resolution, including cells only partly intersecting ANF. Area-weighted summaries use each cell's forest overlap.](figures/figS03-prism-anomaly-correspondence.png){width=100%}

The area-weighted mean raw squared correlation is 0.77 and the anomaly statistic
is 0.72. These are means of cellwise statistics, not the squared correlation of
a measured forest-wide mean. PRISM incorporates station observations and this
is not independent validation. Cellwise counts, bias, RMSE, area weights, and
April 30 correspondence are in [Supplementary Table S7](tables/tableS07-prism-cells.csv).

# Exploratory comparison with Wakulla soil moisture

## Observations, timing, and quality control

The Wakulla #1 SCAN site lies inside the fixed ANF footprint, 12.03 km from
the Tallahassee airport. Its five soil-moisture sensors sample depths of 2,
4, 8, 20, and 40 inches (5.08-101.6 cm). The first four records begin on
10 December 1996 and the deepest begins on 29 April 1997 [@NRCSWakulla].
The soil-moisture observations are independent of the airport weather inputs
used to compute KBDI. Following the depth-specific motivation of @Hoylman2024, we report all depths rather than selecting the sensor
with the highest correspondence.

NRCS's DAILY SMS product contains instantaneous midnight observations, not
daily averages. We pair midnight moisture on date D with nominal end-of-day
KBDI on D-1, and retain same-date pairing as a sensitivity check. The SCAN
station uses UTC-5; the airport daily observation window may not coincide
exactly with local midnight. No lag is optimized to increase agreement.

Primary comparisons retain only NRCS QC flag V (valid) observations bounded
between 0 and 100% and KBDI following at least 365 consecutive complete input
days. The API request excludes suspect observations. Both QC and QA flags
are preserved in the paired dataset. A sensitivity adds N observations,
which lack an automated validation profile; E and other flags are excluded.
The source snapshot extends to 12 October 2026, but the last V-flagged soil
observation is 23 September 2024. Primary pairing therefore ends with KBDI
on 22 September 2024. Missing days are not filled.

![**Figure S4. Wakulla soil-moisture observations.** Monthly means of eligible midnight observations by sensor depth, requiring at least 80% of calendar days to have paired observations. Lines break across missing or insufficiently covered months. Panels have different vertical scales. Pronounced level changes are visible, but their causes have not been established from maintenance history.](figures/figS04-wakulla-soil-moisture.png){width=100%}

![**Figure S5. Coverage of the Wakulla comparison.** Fraction of calendar days with V-flagged, physically bounded soil moisture and an eligible paired KBDI value. Years with no eligible pairs are shown explicitly. The first and last years are partial source years; the denominator remains the full calendar year.](figures/figS05-wakulla-coverage.png){width=100%}

## Descriptive correspondence

We report raw Pearson and Spearman correlations. To remove the broad seasonal
cycle, we separately subtract each variable's mean for its calendar month,
calculated on the same paired dates, and correlate the residuals. This
month-of-year centering is simpler than the standardized soil-moisture-index
methodology in @Hoylman2024 and does not reproduce that study.
We also center within each year/month as a diagnostic of shorter-term
correspondence despite persistent offsets. Results are reported for all
months, May, and May-July, using both the full available record and 2007-2024.
The later period starts after the prominent multi-year gap; it is a sensitivity
choice, not evidence that subsequent observations are homogeneous.

| Depth (cm) | Paired days | Raw Pearson r | Month-anomaly r |
|:--|--:|--:|--:|
| 5.08 | 7,617 | -0.230 | -0.175 |
| 10.16 | 7,367 | -0.114 | -0.092 |
| 20.32 | 7,783 | -0.348 | -0.308 |
| 50.80 | 8,124 | -0.381 | -0.349 |
| 101.60 | 7,655 | -0.416 | -0.423 |

**Table S9.** Primary full-record, all-month descriptive correspondence, using
midnight soil moisture and preceding-day KBDI. Correlations are negative when
higher drought deficit corresponds to lower soil moisture. Paired-day counts
are not independent sample sizes. The [full diagnostic table](tables/tableS09-wakulla-correspondence.csv)
contains the seasonal, period, quality-flag, and timing sensitivities.
[Table S10](tables/tableS10-wakulla-coverage.csv) reports annual coverage.

Month-anomaly correlations for 2007-2024 range from -0.20 to -0.34. Using
same-date KBDI in the full record gives raw correlations from -0.14 to -0.41.
Thus the expected sign persists, while strength depends on depth, period, and
timing. Daily values are serially dependent; we report no ordinary
correlation p-values or inferential intervals. The analysis is exploratory
and does not involve tuning KBDI to the observations or evaluating a fitted
soil-moisture model out of sample.

## Interpretation and limitations

NRCS documents a loam conversion equation for these sensors [@NRCSWakulla].
V flags do not establish stable calibration or rule out sensor drift. The
series show pronounced level changes, including at approximately 10 cm around
2001, 5 cm around 2017, and 51 cm during 2020-2022. These are review targets,
not diagnosed instrument failures. We do not delete intervals solely because
doing so would improve correlation, and do not fit trends to these records.
Maintenance and calibration histories are needed before defining defensible
segments or adjustments.

KBDI-equivalent deficit in millimeters and volumetric soil moisture in percent
are different quantities. An absolute deficit bias or RMSE would require a
specified soil profile, storage capacity, and separately justified calibration.
The observed inverse correspondence supports a limited interpretation of KBDI
as a local dryness indicator. Its modest strength also cautions against
substituting KBDI for measured soil moisture. This single-profile comparison
cannot verify the 1944-onward drought trend or represent the entire ANF.
Any stronger validation would require sensor-history review, a prespecified
seasonal normalization, temporal uncertainty estimates, and evaluation on data
not used for calibration.

# High-deficit threshold sensitivity

| Deficit threshold (mm) | Equivalent KBDI | Days per decade | Pointwise 95% interval |
|:--|--:|--:|:--|
| 152.4 | 600.0 | 3.04 | -0.29 to 6.36 |
| 160.0 (original paper) | 629.9 | 2.80 | 0.56 to 5.04 |
| 165.1 (primary revision) | 650.0 | 2.59 | 0.67 to 4.51 |
| 177.8 | 700.0 | 1.32 | 0.37 to 2.27 |

**Table S2.** All threshold checks retain the same 69 complete eligible annual
periods, with lag-3 covariance. These exploratory pointwise intervals are not
adjusted as a new family of confirmatory tests. The thresholds do not define
validated local ecological damage levels. The [machine-readable table](tables/tableS02-threshold-sensitivity.csv)
retains full precision.

# Additional tabular diagnostics

- [Table S3: KBDI settings](tables/tableS03-kbdi-settings.csv): 27 combinations
  of rainfall climatology, initial deficit, and spin-up, for April-July slopes.
  Original units in this table are mm per year; multiply slopes and intervals
  by 10 to express change per decade. Intervals are unadjusted.
- [Table S4: ENSO adjustment](tables/tableS04-enso-adjustment.csv): year and
  contemporary SOI coefficients for each month. Year slopes are mm per year;
  SOI coefficients are mm per SOI unit. Adjusted p-values are calculated across
  months within each term.
- [Table S6: original versus corrected algorithm](tables/tableS06-algorithm-comparison.csv):
  identical observations and complete eligible years; slopes are mm per year.
- [Table S8: weather gaps](tables/tableS08-weather-gaps.csv): missing or rejected
  required daily inputs. The audit spans the full daily processing period,
  including dates outside complete climate-trend years.

The full monthly deficit estimates are in [Supplementary Table S1](tables/tableS01-monthly-deficit-trends.csv), with slopes in mm per decade. The main manuscript tables are [Table 1: annual trends](tables/table01-annual-trends.csv) and [Table 2: natural-fire forecast metrics](tables/table02-natural-fire-forecasts.csv).

# Reproduction

Run `Rscript scripts/run-all.R` from the project root to rebuild the analysis,
publication figures, tables, and HTML previews. Figure-only regeneration uses
`Rscript scripts/paper-figures.R`; it requires existing prepared results and
makes no network requests. The Wakulla diagnostic uses `Rscript scripts/soil-moisture-check.R`, followed
by `Rscript scripts/paper-soil-supplement.R` to export its figures and tables.
The complete workflow includes these cached-data steps. Every figure is
exported as a vector PDF and a 300-dpi PNG; figure manifests record inputs and
export checksums.

# References

::: {#refs}
:::
