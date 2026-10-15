---
title: "Long-term increases in station-derived drought near Apalachicola National Forest, Florida"
author: "James B. Elsner"
date: "Revised preprint draft, 15 October 2026"
bibliography: references.bib
link-citations: true
lang: en-US
---

Department of Geography, Florida State University, Tallahassee, FL 32306, USA

**Corresponding author:** James B. Elsner, <jelsner@fsu.edu>

*Revision draft of the April 2022 EarthArXiv preprint
[10.31223/X5H350](https://eartharxiv.org/repository/view/3268/). This manuscript has not been peer reviewed.*

# Abstract

Long local weather records can reveal changes in drought exposure, but a drought
index calculated at one station must be distinguished from soil-water
measurements across a forest. We updated daily temperature and precipitation
observations at Tallahassee airport and recomputed the Keetch-Byram Drought
Index (KBDI) with explicit quality control, rainfall interception, and missing-data
rules. Across 69 complete eligible annual records spanning 1945-2024, mean
moisture deficit increased by 2.63 mm per decade (95% interval: 0.58-4.67), and
the annual number of days with KBDI above 650 increased by 2.59 days per decade
(0.67-4.51). May showed the clearest monthly drying signal after adjustment for
12 monthly comparisons: 4.64 mm per decade (1.68-7.61; adjusted p = 0.031).
April's trend remained uncertain. May slopes stayed positive across 27 tested
KBDI settings, although uncertainty in high-deficit-day trends depended on the
record's starting year. A historical 2009-2018 PRISM comparison showed
airport-forest daily anomaly correspondence, with an area-weighted squared
correlation of 0.72. As an application, April 30 moisture deficit reduced
point-estimate hindcast error for May-July natural-fire counts, but the paired
skill interval included no improvement. The strongest evidence therefore
concerns increasing station-derived drought exposure and its seasonal
structure. It does not establish a forest-wide soil-moisture trend, an
anthropogenic attribution, or a validated operational wildfire forecast.

**Keywords:** Keetch-Byram Drought Index; station record; moisture deficit;
seasonal drought; wildfire; Florida.

# 1. Introduction

Long weather records provide a way to detect changes in local drought exposure
that shorter observational networks cannot resolve. Their interpretation requires
care: a weather-derived drought index describes a particular response to
meteorological conditions, whereas soil moisture, ecological stress, and fire
activity depend on additional processes. Recent work has sharpened both the
need to assess change and the need to distinguish these quantities. @Hoylman2022 show that historical reference periods can misrepresent
contemporary drought severity when climate distributions shift. Estimating a
long-term trend and classifying present drought against a stationary baseline
are therefore different tasks.

The Keetch-Byram Drought Index (KBDI) provides a daily measure of accumulated
weather-related moisture deficiency from maximum temperature and precipitation
[@KeetchByram1968]. Its long history in fire applications makes it useful for
examining how drying conditions vary across years and seasons. However, the
index's nominal water-deficit units do not make it a measurement of water
storage in a specified soil profile. In a national comparison of drought
indicators with observed soil moisture, @Hoylman2024 found
that correspondence depends on depth and timescale, with several soil-moisture
models outperforming the meteorological indices they evaluated. Their study
did not directly validate KBDI. It motivates comparing our index with local
observations rather than assuming equivalence with measured soil moisture.

Apalachicola National Forest (ANF), in the Florida Panhandle, offers a useful
setting for such an assessment. The nearby Tallahassee airport record permits
consistent drought-index calculations over eight decades. The regional fire
context is also changing: @Donovan2023 report spatially
heterogeneous changes in large wildfires across the eastern United States.
That regional evidence concerns a different spatial scale and fire response
from natural-fire counts in ANF. Western US attribution studies likewise
motivate investigation without supplying a local causal explanation
[@AbatzoglouWilliams2016].

We ask how annual mean KBDI-derived deficit and high-deficit-day exposure have
changed in the station record, and how the changes differ among calendar
months. The main contribution is a reproducible assessment of long-term
station-derived drought change, with explicit missing-data rules and
sensitivity to index settings, record period, serial dependence, and ENSO.
A historical PRISM comparison assesses temporal correspondence across the
forest footprint. A supplemental comparison with in-situ Wakulla soil moisture
examines recent local correspondence and its limitations.

Seasonal wildfire prediction is an application of this drought record. We test
whether April 30 deficit improves retrospective forecasts of May-July natural
fire counts over a climatological baseline, training each hindcast on preceding
seasons. This separates detection of station-derived drought change from the
predictive value of a pre-season indicator. It also avoids assuming that a
trend in drought must produce an equivalent trend in fires: fuels, ignitions,
suppression, and prescribed burning can alter that relationship. Recent
southeastern US research treats prescribed burning and wildfire as interacting
responses to climate and management [@Hashida2025].

This manuscript revises the earlier ANF preprint [@LawElsner2022], extending
the weather and fire snapshots, correcting the rainfall implementation, and
reorganizing the analysis around long-term drought change. The wildfire season
and April 30 predictor are retained from that work; the updated hindcasts
use preceding-year training and explicitly quantify paired skill uncertainty.

# 2. Study setting and data

## 2.1. Airport observations and the forest footprint

We use the Tallahassee airport record, GHCN-Daily station USW00093805, from
NOAA's National Centers for Environmental Information. GHCN-Daily provides
quality-controlled historical daily observations suitable for examining local
weather variability [@MenneEtAl2012; @GHCNDaily]. The station metadata locate the
airport at approximately 30.39354 degrees N, 84.35136 degrees W [@NOAAStation].
ANF lies nearby in the Florida Panhandle. The fixed Forest Service
administrative polygon used in the original study encompasses approximately
2,567 square kilometers. We retain this footprint rather than allowing
administrative-boundary revisions to alter the comparison through time
(Figure 1A). It defines the fire-selection area; it is not a measurement of
forest cover or a claim that all enclosed land has identical fuels or management.

The updated station snapshot contains observations through 12 October 2026. We
begin the calculation on 1 January 1944 and exclude partial calendar years
from climate-trend analyses. Complete eligible annual KBDI records span
1945-2024, with gaps; some complete monthly records extend into 2025. The
candidate endpoint of 2025 must therefore be distinguished from the actual
last year represented in each estimate. Weather observations were not
homogenized for station moves, instrumentation, or observation-time changes.

We use daily precipitation and maximum temperature for KBDI and retain minimum
temperature for climatic context. Native GHCN units are converted from tenths
of millimeters and tenths of degrees Celsius. Values marked missing or having
nonblank quality flags are rejected, and the daily calendar is completed
explicitly. Trace rainfall uses its reported numerical value. Required-input
gaps are not replaced by zero rainfall or undocumented manual fills.

![**Figure 1. Study setting and seasonal drought distribution.** (A) Fixed ANF administrative footprint and the airport weather station (solid dot). (B) Distribution of complete eligible monthly mean KBDI-derived deficits, using available months from 1945-2025. Boxes show the interquartile range, center lines the median, and whiskers extend to 1.5 times the interquartile range; remaining observations are plotted individually.](figures/fig01-setting-seasonality.png){#fig-setting width=100%}

## 2.2. Historical gridded comparison

We recompute KBDI from the existing daily PRISM precipitation and maximum
temperature archive for 2009-2018 at approximately 4-km resolution [@PRISMData].
The comparison includes 170 cells intersecting ANF, with weights proportional
to each cell's area of overlap with the footprint. Dates and input variables
are paired explicitly. Grid calculations use the same KBDI assumptions and
spin-up rule as the station calculation. This is a historical correspondence
check; the PRISM observations have not been extended beyond 2018.

PRISM incorporates station observations and is not independent of the station
network. Its time-series products can also contain inhomogeneities associated
with changing observations [@PRISMData]. Neither high temporal correlation over
this short period nor the availability of a long airport record establishes
spatially homogeneous drought change throughout the forest.

## 2.3. Independent soil-moisture observations

For an exploratory supplemental comparison, we use the USDA NRCS Wakulla #1
Soil Climate Analysis Network (SCAN) station (`2009:FL:SCAN`) at 30.30585 degrees
N, 84.42477 degrees W [@NRCSWakulla]. This location intersects our fixed ANF
polygon and is 12.03 km from the airport. Observations begin in December 1996
at depths of 5.08, 10.16, 20.32, and 50.8 cm, and in April 1997 at 101.6 cm.
The downloaded record extends to 12 October 2026, with substantial gaps.
These are in-situ sensor observations independent of the airport weather
inputs, but they describe one soil profile. NRCS documents conversion using
a loam equation; undocumented sensor changes and calibration uncertainty
limit interpretation of absolute values and long-term shifts [@NRCSWakulla].
The complete comparison rules and diagnostics are given in the supplement.

## 2.4. Wildfire observations for the application

The seventh edition of the Fire Program Analysis Fire-Occurrence Database
(FPA FOD) supplies ignition locations, discovery dates, reported causes, and
final sizes for 1992-2024 [@Short2026]. We select unique ignition points
intersecting the fixed ANF polygon, including boundary points. This yields
1,294 recorded fires, of which 483 are classified as natural. Of the natural
fires, 385 (79.7%) were discovered during May-July. We retain this three-month
season from the original study rather than searching the updated observations
for an optimal prediction window.

Unknown causes remain in the all-fire series. Natural, human, and all-fire
responses are kept separate throughout fitting and evaluation. Every archive
year is represented in the seasonal counts, including zero-fire seasons.
Reported final sizes describe entire fires igniting inside the footprint;
they do not measure the area burned within the boundary. Changes in reporting,
classification, or management remain possible influences on the fire record.

# 3. Methods

## 3.1. Daily drought index and complete-period summaries

KBDI represents cumulative weather-related moisture deficiency
[@KeetchByram1968]. Let $Q_t$ denote the index in hundredths of an inch,
$T_t$ daily maximum temperature in degrees Fahrenheit, and $R$ fixed mean annual
rainfall in inches. For effective daily rainfall $P_t^*$ in inches, we first
reduce the preceding deficit:

$$Q_t^- = \max(0, Q_{t-1} - 100 P_t^*).$$

We then calculate the daily drying increment and bound the resulting index:

$$Q_t = \min\left[800,\; Q_t^- + \max\left\{0,
\frac{(800-Q_t^-)(0.968\exp(0.0486T_t)-8.3)}
{1000[1+10.88\exp(-0.0441R)]}\right\}\right].$$

The metric moisture deficit is $D_t=0.254Q_t$ millimeters, ranging from 0 to
203.2 mm. These are index-equivalent deficits, not measurements of water
stored in a specified soil profile. Effective rainfall removes 0.2 inches
of interception once per consecutive wet spell, rather than independently
from every rainy day. We correct the original implementation's treatment of
consecutive small rainfalls crossing this threshold and subtract effective
rain before evaluating the drying increment.

The primary calculation uses $R=60$ inches and initial $Q=400$. Here $R$ is a
fixed climatological assumption, not the accumulated rainfall in an individual
year. Following any missing required input, we restart at 400 and withhold
estimates until at least 365 consecutive complete days have accrued. A monthly
mean is eligible only if every day in that calendar month has an eligible
index. Annual summaries similarly require every day of the year; annual mean
deficit weights months by their day counts. We do not scale partially observed
periods to represent complete ones.

We examine two primary annual summaries: mean deficit and the number of days
with KBDI greater than 650 (165.1 mm). The latter is an index-specific
high-deficit threshold, not a validated local ecological-impact threshold.
Monthly means describe seasonal structure (Figure 1B). Complete-period
requirements yield 69 annual KBDI records and between 72 and 74 monthly records
per calendar month in the updated trend analysis.

## 3.2. Trend estimates and uncertainty

We fit a linear regression of each annual or calendar-month summary against
calendar year. Slopes are expressed per decade. To allow for serial dependence
between annual observations, uncertainty uses a lag-3 Newey-West covariance
estimate without prewhitening, with finite-sample adjustment and a t reference
[@NeweyWest1987; @Zeileis2004]. These intervals are approximate; daily KBDI observations are
not treated as independent replicates. Lags refer to retained annual
observations, so missing years introduce irregular spacing.

We report pointwise 95% intervals and Benjamini-Hochberg-adjusted p-values
[@BenjaminiHochberg1995]. Adjustment covers 12 monthly tests within each outcome
and endpoint, and five annual outcomes within each endpoint. Annual rainfall
and mean daily maximum/minimum temperature provide context for the two
principal drought summaries. We do not interpret their regressions as a
causal decomposition of KBDI change. The 2020 endpoint, corresponding to the
original study period, is recomputed with the same updated observations and
corrected implementation as an endpoint sensitivity check.

## 3.3. Robustness and spatial correspondence

We evaluate 27 combinations of annual mean rainfall (55, 60, 65 inches), initial
KBDI (0, 400, 800), and complete-day spin-up (180, 365, 730 days). These values
define sensitivity scenarios rather than tuned parameters. Additional trend
fits use record starts of 1960 and 1980 and annual covariance lags of 0, 1, 3,
and 5. We compare original and corrected algorithms using identical refreshed
observations and eligible annual periods. High-deficit-day sensitivity also
includes thresholds of 152.4, 160, and 177.8 mm, retaining the same annual years
as the primary 165.1-mm analysis.

For a descriptive ENSO adjustment, we regress each monthly deficit mean on year
and contemporary monthly Southern Oscillation Index (SOI) from NOAA PSL
[@NOAASOI]. The shorter common record and adjusted monthly test family are
reported explicitly. This adjustment neither identifies ENSO's causal effect
nor constitutes a forecast using only information available before a season.

For each PRISM cell, we calculate temporal correlations with the station
series on aligned eligible dates. We report both raw squared correlations and
squared correlations after subtracting each location's mean calendar-day
seasonal cycle. The latter separates anomaly correspondence from shared
seasonality. Forest-area-weighted averages summarize the cellwise statistics;
they are not the squared correlation of an independently observed forest-wide
mean.

## 3.4. Seasonal wildfire prediction as an application

We model the number of natural wildfires during May-July with a negative
binomial distribution. For April 30 deficit $D_y^{cm}$ in centimeters,

$$N_y\sim\operatorname{NegBin}(\mu_y,\theta),\qquad
\log\mu_y=\beta_0+\beta_1D_y^{cm},$$

where the variance is $\mu_y+\mu_y^2/\theta$. Maximum-likelihood estimation uses
`MASS::glm.nb` [@VenablesRipley2002]. An intercept-only negative binomial model
provides the climatological baseline. Year-only and drought-plus-year models
are specification checks, rather than winners selected for operational use.
The fixed seasonal window requires no varying-duration offset.

Hindcasts train only on earlier eligible seasons, beginning with 15 training
seasons. The 30 eligible natural-fire seasons yield 15 evaluation seasons,
2010-2024. Models are compared using identical targets and evaluation years,
with mean absolute error (MAE), root mean squared error (RMSE), and mean
negative log probability of the observed count. Smaller values are better.
This corrects the earlier notebook's mismatch between natural-fire predictions
and all-fire observations and avoids training on later seasons to predict an
earlier one.

Uncertainty in the paired MAE difference uses 2,000 circular moving-block
bootstrap resamples of evaluation-year errors, with three-year blocks and a
fixed seed. This approximate interval resamples the realized hindcast errors;
it does not refit the entire model-selection process. Negative binomial count
intervals condition on fitted coefficients and dispersion. We do not represent
these intervals as Bayesian posterior predictions or as including parameter
uncertainty. All evaluations are retrospective: archive revisions and source
publication delays have not been reconstructed as they would have been known
at a historical issue date.

# 4. Results: long-term station-derived drought change

## 4.1. Annual mean deficit and high-deficit exposure increased

The complete eligible annual record supports increasing station-derived drought
exposure (Figure 2; Table 1). Mean annual deficit increased by 2.63 mm per decade
(95% interval: 0.58-4.67), and days with KBDI above 650 increased by 2.59 days per
decade (0.67-4.51). Both annual drought outcomes retain adjusted p-values below
0.05 when considered with the three contextual weather outcomes. The estimates
summarize a variable record with substantial differences among individual years;
they do not imply a monotonic increase or provide an extrapolation beyond the
observed period.

![**Figure 2. Annual station-derived drought change.** (A) Mean annual moisture deficit and (B) days with KBDI above 650, for 69 complete eligible years spanning 1945-2024. Points are annual values and lines are least-squares fits. Slope annotations report changes per decade with pointwise 95% intervals using lag-3 annual covariance. Missing or spin-up-affected years are excluded, not filled.](figures/fig02-annual-drought-change.png){#fig-annual width=100%}

| Annual outcome | Change per decade | 95% interval | Adjusted p | Years |
|:--|--:|:--|--:|--:|
| Mean deficit (mm) | 2.63 | 0.58 to 4.67 | 0.021 | 69 |
| Days with KBDI > 650 | 2.59 | 0.67 to 4.51 | 0.021 | 69 |
| Rainfall (mm) | -19.06 | -53.48 to 15.37 | 0.274 | 78 |
| Mean daily maximum temperature (degrees C) | 0.200 | 0.115 to 0.285 | <0.001 | 75 |
| Mean daily minimum temperature (degrees C) | 0.141 | -0.032 to 0.313 | 0.135 | 74 |

**Table 1.** Annual trends from the updated complete-period analysis. All effective
annual records end in 2024; drought records begin in 1945 and contextual weather
records in 1944, with variable-specific gaps. Intervals are pointwise, whereas
p-values are adjusted across the five outcomes.

Mean daily maximum temperature also increased, by approximately 0.20 degrees C
per decade. The rainfall interval includes both decline and increase, and the
minimum-temperature interval includes zero. These results are compatible with
more than one explanation for increased deficit. In particular, they do not
establish a significant decrease in annual rainfall or quantify the part of
the deficit trend caused by warming.

## 4.2. May has the clearest monthly drying signal

Monthly deficit slopes are positive in point estimate throughout the year,
but their magnitudes and uncertainty differ (Figure 3). May's increase is
4.64 mm per decade (95% interval: 1.68-7.61), with an adjusted p-value of 0.031.
It is the only calendar month meeting the adjusted 0.05 threshold. Positive
slopes in August, September, and October have pointwise intervals above zero,
but do not meet that threshold after adjustment across the monthly family.

![**Figure 3. Seasonal structure of drought change.** Slopes of monthly mean deficit against year, expressed in mm per decade, with pointwise lag-3 covariance intervals. Color identifies months meeting the Benjamini-Hochberg-adjusted 0.05 threshold across 12 monthly tests. Available complete monthly records span 1945-2025; actual ending years and sample sizes vary by month. The month comparison is exploratory, not a preregistered May-specific test.](figures/fig03-monthly-drought-trends.png){#fig-monthly width=100%}

April's slope is 2.30 mm per decade, with an interval of -1.06 to 5.65 and an
adjusted p-value of 0.265. Thus the updated evidence does not sustain the earlier
paper's emphasis on a significant April increase. May remains relevant to the
seasonal setting of fire, but a positive May trend is not evidence that all
months are drying at an established rate or that the fire season is lengthening.

## 4.3. Robustness supports the mean-deficit result but qualifies exposure trends

May's point-estimate slope ranges from 4.19 to 5.24 mm per decade across the 27
KBDI settings. Its unadjusted 95% interval remains above zero in every setting.
These are overlapping sensitivity analyses, not independent replications or a
joint confidence statement. The corrected and original algorithms give very
similar mean annual slopes on identical eligible observations: 2.63 and
2.66 mm per decade, respectively. The annual increase is therefore not produced
by the implementation correction alone.

The mean-deficit trend also remains positive in later-start records. For the
record beginning in 1960, the slope is 4.35 mm per decade (2.88-5.82); for the
record beginning in 1980 it is 3.17 (0.89-5.46), using the primary covariance
lag. High-deficit-day trends are more period-sensitive. The post-1980 estimate
is 0.63 days per decade with an interval of -4.27 to 5.53. The long-record
exposure increase should therefore not be described as an established recent
acceleration (Supplementary Figure S2).

Changing the high-deficit threshold changes the result's magnitude and
uncertainty. On the same 69 annual records, the original paper's 160-mm
threshold gives 2.80 additional days per decade (0.56-5.04). At 152.4 mm the
point estimate is positive but its interval includes zero; at 177.8 mm the
increase is smaller with a positive interval. These exploratory checks support
reporting the threshold explicitly rather than claiming an identical result
for every definition of high deficit (Supplementary Table S2).

With contemporary SOI adjustment, May's year slope remains positive at
4.82 mm per decade (1.22-8.41), but its adjusted monthly p-value is 0.104.
The common May record contains 69 eligible observations spanning 1952-2025.
It would therefore be too strong to claim that ENSO adjustment confirms
significant May drying across the tested monthly family. Similarly, describing
SOI as the cause of the observed deficit change is not supported by this
regression.

## 4.4. Spatial and soil-moisture comparisons delimit the interpretation

Across the 170 intersecting PRISM cells, the area-weighted mean raw squared
correlation with station-derived deficits is 0.77. After removing each
location's mean calendar-day seasonal cycle, the corresponding anomaly
statistic is 0.72 (Supplementary Figure S3). Shared seasonality thus explains
part, but not all, of the correspondence. These cellwise squared correlations
indicate similarity in temporal variations during the short historical
comparison; they do not establish unbiased forest-wide moisture estimates.

Because PRISM uses station observations and this comparison is limited to
2009-2018 with spin-up exclusions, it cannot independently validate the
long-term station trend. We retain the station-derived description of the main
finding rather than treating these correlations as evidence of equivalent
change throughout ANF.

The independent Wakulla comparison provides a different check. Across depths,
raw correlations between measured moisture and the preceding day's index
range from -0.11 to -0.42; removing month-of-year means gives -0.09 to -0.42.
The inverse sign is physically expected, but the correspondence is modest.
Record gaps, pronounced level shifts, and sensitivity to depth and date
alignment qualify interpretation (Supplementary Figures S4-S5 and Table S9).
We report this as exploratory correspondence, not calibration of absolute
water deficit or independent verification of the historical trend.

# 5. Application: pre-season drought and natural wildfire counts

The drought-only negative binomial model has a positive coefficient on April
30 deficit. In the full eligible sample, the coefficient is 0.168 per centimeter
(standard error 0.046), equivalent to an estimated 18.3% change in expected
seasonal count per additional centimeter, calculated as
$100[\exp(\beta_1)-1]$. This is a fitted association, not a causal effect of
changing moisture deficit or evidence that a long-term drought trend must
increase future fire counts.

In preceding-year hindcasts, drought-only MAE is 8.06 fires per season compared
with 9.93 for the climatological baseline (Figure 4; Table 2). RMSE and negative
log score also improve in point estimate. However, the paired MAE improvement
is 1.87 fires with an approximate 95% interval of -0.26 to 4.02. With only
15 evaluation seasons, the evidence does not establish a reliably positive
improvement for natural-fire forecasts.

![**Figure 4. Wildfire prediction as an application.** (A) Observed May-July natural-fire counts (black points) and forecast means from drought-only and climatological negative binomial models, evaluated in 2010-2024 using only preceding eligible seasons for training. (B) MAE for the two primary models, with the paired improvement and its approximate three-year-block bootstrap interval. These are retrospective hindcasts, not forecasts issued in real time.](figures/fig04-natural-fire-application.png){#fig-fire width=100%}

| Natural-fire model | MAE (fires) | RMSE (fires) | Mean negative log score |
|:--|--:|--:|--:|
| Climatology | 9.93 | 11.18 | 3.273 |
| April 30 drought | 8.06 | 9.45 | 3.175 |
| Year trend | 9.08 | 11.36 | 3.197 |
| Drought and year trend | 7.13 | 9.53 | 3.103 |

**Table 2.** Identical 15-season evaluation sets for the natural-fire response.
Year-only and combined models are exploratory specification checks. Ranking
several specifications on these same years does not provide an independent
validation of the best-performing specification.

Adding a year trend changes the skill comparison, illustrating that the fire
record contains temporal structure beyond the drought predictor. A supplementary
antecedent-burn-area check uses an aligned May 1-April 30 window, but final
reported sizes might not have been available at a historical forecast date.
We do not infer management effectiveness from that predictor's performance,
and we do not use the fitted count model to project future wildfire activity
from extrapolated drought trends.

# 6. Discussion

## 6.1. What the updated record establishes

The principal result is an increase in station-derived annual moisture deficit,
together with increased exposure above a specified high-deficit threshold,
over the available long record. The seasonal comparison places May ahead of
April as the clearest monthly signal under the present specification and
multiplicity adjustment. The long record and seasonal analysis provide the principal evidence; the
small natural-fire evaluation sample gives a more tentative assessment of
predictive utility.

The agreement between corrected and original algorithms on the same annual
periods supports the persistence of the mean-deficit finding. It does not
remove uncertainty arising from observations or missing-data choices. Likewise,
robustness to the tested KBDI settings supports the May pattern within this
index framework, but is not a validation of KBDI against direct soil-water
measurements. Differences between long-record and later-start exposure trends
show why the estimated period belongs alongside any rate of change.

Long-term change and operational drought severity also require different
reference frames. @Hoylman2022 caution against treating a long
historical distribution as stationary when assessing present conditions.
Here we retain a fixed KBDI formulation to describe change through time; we
do not estimate contemporary return periods from the pooled record. Updating
a monitoring baseline would answer a different question and would not remove
the need to examine the full record for change.

Annual means and threshold counts describe two aspects of exposure. They do
not resolve drought onset, duration, recovery, or rainfall interruptions.
The KBDI-based drought-feature framework of @Elias2024 illustrates
how these characteristics can be separated in a Mediterranean setting.
Applying that approach locally could test seasonal timing, but our monthly
slopes alone do not establish a lengthening drought or wildfire season.

## 6.2. Mechanisms, scale, and management interpretation

Temperature, rainfall timing, and antecedent conditions jointly influence the
calculated deficit. An increase in maximum temperature is consistent with one
contribution to drying, but the regressions here do not separate those
contributions or attribute them to anthropogenic forcing. Regional attribution
studies use additional modeling and evidence [@AbatzoglouWilliams2016]; our
single-station analysis does not perform that attribution.

Ignition, suppression, prescribed fire, fuels, and reporting all influence
observed fires in this managed landscape. @Hashida2025
explicitly model the interaction between climate, prescribed burning, and
large wildfire in the southeastern United States. Southeast FireMap also
provides regional evidence on changes in frequent burning across ownerships
and landscapes [@Nowell2026]. Those burned-area and management analyses concern
different outcomes from our ignition counts. Their results support considering
management in future local work without implying that our count model measures
its effectiveness.
A weak or uncertain burn-area predictor cannot show that management is
effective, and a positive drought coefficient does not identify how management
would change the response. The forecast application instead shows how a drought
indicator can be evaluated against an explicit baseline with uncertainty.

## 6.3. Limitations and next research steps

The station series has not undergone a dedicated homogenization analysis.
Changes in siting, instruments, or observation practices could contribute to
apparent long-term change. Reviewing station history and comparing neighboring
long-term records are therefore priorities. The complete-period policy avoids
undocumented imputations but may select an unrepresentative set of years if
missingness is related to weather conditions. KBDI resets and the fixed rain
climatology are further assumptions, even though sensitivity checks address
part of their influence.

Trend intervals use an approximate covariance treatment with irregularly spaced
retained annual observations. Alternative time-series or block-based climate
inference could refine uncertainty. The May comparison remains exploratory,
and the shorter ENSO-adjusted record does not justify a stronger monthly-family
claim. The 2009-2018 spatial check cannot supply a homogenized forest-wide
long-term trend; a suitable spatial record or independent soil/fuel-moisture
measurements would be needed to extend that interpretation. The Wakulla
observations begin much later and show level changes whose causes are
unresolved. They add independent local context but do not validate the
1944-onward trend. Consistent with @Hoylman2024, depth and
season should be specified in any soil-moisture assessment; maintenance
history and temporal uncertainty analysis are prerequisites for a stronger
validation claim.

For wildfire prediction, only 15 hindcast seasons are available. Source release
delays were not reconstructed, conditional count intervals omit parameter
uncertainty, and comparisons among added specifications reuse the same test
period. Independent future evaluation, a longer stable fire record, and an
explicitly reconstructed issue-date workflow would be needed before judging
operational utility.

# 7. Conclusions

The Tallahassee airport record shows increasing KBDI-derived annual moisture
deficit and more days above a specified high-deficit threshold across
69 complete eligible annual records spanning 1945-2024. May has the clearest
monthly drying signal under adjustment for 12 comparisons, while April's
trend remains uncertain. These findings characterize long-term station-derived
drought exposure near ANF. Historical airport-grid correspondence and seasonal
natural-fire hindcasts provide supporting context and an application, but do
not establish a forest-wide causal climate-change effect or a validated
operational wildfire forecast.

# Acknowledgements

The initial work was conducted by Zach T. Law as a graduate student at
Florida State University and formed the basis of the initial manuscript
[10.31223/X5H350](https://doi.org/10.31223/X5H350). His contribution to the
foundation of this study is gratefully acknowledged. This work was supported
by Florida State University.

# Data and code availability

Source data are available from NOAA GHCN-Daily and NOAA PSL, PRISM, the
Forest Service FPA FOD archive, and USDA NRCS SCAN. The project repository is
<https://github.com/jelsner/KBDI>. The revised reproduction workflow uses
`scripts/run-all.R`; `scripts/paper-figures.R` redraws all manuscript figures
from prepared results, and `scripts/paper-tables.R` exports the tables.
The supplemental Wakulla comparison is reproduced with
`scripts/soil-moisture-check.R` and `scripts/paper-soil-supplement.R`.
Small source snapshots, derived data, settings, and checksums are documented
in the repository. Large historical PRISM grids remain local, with a compact
forest extraction sufficient to repeat the spatial comparison. Figure and
analysis manifests identify the specific inputs used here. The repository
URL identifies the project; this revision requires its own release or commit
identifier before publication.

# References

::: {#refs}
:::
