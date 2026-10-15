---
title: "Supplement: Long-term station-derived drought change near Apalachicola National Forest"
date: "Revised draft, 15 October 2026"
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
makes no network requests. Every figure is exported as a vector PDF and a
300-dpi PNG. The figure manifest records input and export checksums.
