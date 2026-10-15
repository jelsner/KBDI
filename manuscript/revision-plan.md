# Proposed revision: drought change first, wildfire prediction second

This is a proposed structure based on the updated repository analyses. The
existing Overleaf manuscript was inaccessible; its exact text, citations,
author list, and journal requirements have not been reviewed or changed.

## Working title

**Long-term increases in station-derived moisture deficit near Apalachicola
National Forest, with an application to seasonal wildfire prediction**

Use “near” and “station-derived” until a homogenized spatial record can support
a forest-wide long-term trend. The historical PRISM comparison provides useful
correspondence evidence, but is neither independent validation nor an
80-year spatial trend analysis.

## The central result

At Tallahassee airport, mean annual KBDI-derived moisture deficit increased
2.63 mm per decade (95% interval 0.58–4.67)
across 69 eligible annual records spanning 1945–2024.
Days with KBDI > 650 increased 2.59 per decade
(95% interval 0.67–4.51). These are observed station-record
trends, not attribution to anthropogenic climate change or direct observations
of soil water throughout ANF.

## Draft abstract

Long-term changes in drought can alter the environmental setting for wildfire,
but local drought trends and seasonal predictive value need to be distinguished.
We updated daily precipitation and temperature observations at Tallahassee
airport, recomputed the Keetch–Byram Drought Index with corrected rainfall
interception and drying calculations, and estimated trends using complete
annual and monthly records with uncertainty allowing for serial dependence.
Mean annual moisture deficit increased 2.63 mm per decade
(95% interval 0.58–4.67), while days with KBDI above 650
increased 2.59 per decade (0.67–4.51).
May exhibited the clearest monthly drying signal: 4.64 mm per decade
(1.68–7.61), with a false-discovery-rate-adjusted p-value of
0.031. April's trend remained uncertain. A historical PRISM comparison
showed daily anomaly correspondence between airport and forest-grid estimates
(area-weighted R² = 0.72). As an application, April 30 moisture deficit
improved point-estimate forecasts of May–July natural-fire counts, but the
paired skill interval included no improvement. The results support a focus
on long-term station-derived drought exposure and its seasonal structure,
with wildfire prediction as a limited application rather than the primary claim.

## Proposed section order

1. **Introduction: why local drought exposure matters.** Frame two questions:
   whether drought exposure has changed, and when in the year change is clearest.
   Introduce wildfire prediction only as an application. Retain and update the
   manuscript's existing literature after its source becomes available.
2. **Data and methods.** Define the station and study footprint; distinguish
   effective trend records from raw observation coverage. Describe KBDI units,
   corrections, missing-data policy, annual/monthly aggregation, HAC intervals,
   multiplicity, sensitivity checks, and the historical PRISM check. End with
   fire data and expanding-window hindcasts.
3. **Results: long-term annual change.** Lead with mean deficit and high-deficit
   days, estimates and intervals together. Separate direction and magnitude
   from significance. Explain the incomplete years that were excluded.
4. **Results: seasonal structure.** Show all 12 months; emphasize May without
   treating it as a preregistered month. Retire the old significant-April claim.
   Do not infer increasing drought in every month merely from positive slopes.
5. **Results: robustness.** Show that the May slope stays positive under the
   KBDI settings tested and compare record starts and HAC lags. Mean-deficit
   changes persist in later-start records, but the post-1980 high-deficit-day
   trend is uncertain. ENSO-adjusted May estimates stay positive but do not
   pass the adjusted 12-month threshold. The old and corrected algorithms
   yield similar annual trend estimates on the same observations.
6. **Application: natural-fire seasonal forecasts.** Use one consistent response
   and retrospective evaluation. Report climatology and temporal-trend baselines,
   paired skill uncertainty, and why 15 test seasons limit conclusions. Avoid
   ranking added predictors as established winners after trying them here.
7. **Discussion.** Interpret changing exposure, observational limitations, and
   plausible mechanisms separately. Discuss unhomogenized station records,
   complete-case selection, fixed rain climatology, reporting changes in fire
   records, short spatial evidence, and uncertainty in predictive value.
8. **Conclusion.** State the annual drought result and May pattern, then one
   restrained sentence on the wildfire application. Do not claim a forest-wide
   causal climate-change effect from this station analysis.

## Figure plan

- Main Figure 1: study area and station/data scope, with the forest footprint
  and airport shown clearly.
- Main Figure 2: `outputs/annual-drought-change.png` (annual deficit and
  high-deficit days). Pair with a small slope-and-interval table.
- Main Figure 3: `outputs/monthly-drought-trends.png` (all months, intervals,
  and multiplicity adjustment).
- Optional Main Figure 4: natural-fire hindcasts, using the natural panel of
  `outputs/hindcasts.png` and a brief uncertainty table.
- Supplement: daily drought calendar, original-versus-updated endpoint results,
  covariance/period sensitivity, KBDI settings, ENSO fits, PRISM anomaly map,
  cause and size distributions, burned acreage, and prior-burn predictor checks.
- Remove duplicated monthly fire-frequency charts, parameter-draw galleries,
  and exploratory day-of-year rankings from the main narrative. Preserve
  historical outputs as provenance rather than evidence of new results.

## Claims to revise or qualify

- Replace “April drought significantly increased” with its updated estimate
  and interval: 2.30 mm per decade (-1.06–5.65).
- Avoid “ENSO adjustment confirms significant drying in May across months”:
  the adjusted monthly test family does not support that stronger claim.
- Replace “airport data explain forest drought” with “the historical daily
  anomaly correspondence is R² = 0.72”; PRISM uses station observations.
- Replace a general claim of skillful natural-fire prediction with the actual
  MAE change, 9.93 to 8.06 fires per season, and its
  paired improvement interval -0.26–4.02 fires.
- Do not equate KBDI with observed soil moisture or reported final fire size
  with acreage actually burned inside the forest boundary.
- Do not state that rainfall decreased significantly or temperature caused
  the deficit trend. Association in this analysis does not identify mechanisms.

## Work still needed before submission

Reconcile this structure with the actual Overleaf source and bibliography;
review station history and possible inhomogeneity; decide whether to extend
PRISM or obtain a long-term spatial product; examine the missing-data selection
assumption; and assess whether a specialist statistical review should replace
the approximate HAC and short-hindcast uncertainty calculations. Bayesian fits
are preserved but unrun because brms/Stan is unavailable. No journal-ready
claim is made for a burned-area model.
