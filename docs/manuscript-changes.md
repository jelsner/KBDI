# Changes from the original paper

Original source: `archive/original-manuscript/main.md` (April 2022), preserved
byte-for-byte along with its figure files and ZIP export. Current draft:
`manuscript/main.md`, with `manuscript/supplement.md` and `references.bib`.

## Narrative and claim changes

| Original emphasis or claim | Revised treatment |
|---|---|
| Seasonal wildfire prediction leads the paper | Annual station-derived drought exposure leads; prediction is an application |
| Airport-derived index represents soil moisture throughout ANF | Describe station-derived KBDI explicitly; PRISM correspondence has a short, partly shared observational basis |
| A significant April increase is a principal result | Report April's uncertain interval; show all 12 months and the adjusted May result |
| Upward trends plus the fitted fire association imply greater future fire risk | Remove extrapolation and causal inference; report retrospective predictive uncertainty |
| Leave-one-out scores approximate operational performance | Use preceding-year training; state that source-publication delays are not reconstructed |
| Natural prediction versus all-fire observations in code | Use consistent cause-specific targets and identical evaluation years |
| Positive deficit coefficient interpreted with 1 - exp(beta) | Correct percentage change to 100[exp(beta) - 1] |
| KBDI ranges to 200 mm; annual rainfall in the formula is an individual year's accumulation | Use exact 0-203.2 mm range and fixed mean annual rainfall assumption |
| No urbanization/roadway confounding; a weak prior-burn predictor indicates effective management | Remove unsupported claims; discuss fuels, ignitions, management, and reporting |
| Monthly lightning, point-pattern analysis, multiple exploratory figures | Preserve original material in the archive; omit it from the focused main narrative |
| Bayesian posterior curves support reported uncertainty | Do not use unrun Bayesian fits; distinguish conditional count intervals and paired error bootstrap |

## Added clarity and robustness

- Separate raw weather coverage through October 2026 from effective annual
  KBDI records (69 years, 1945-2024, with gaps) and variable-specific monthly
  endpoints.
- Specify complete-period selection, quality flags, the 365-complete-day
  requirement, KBDI initialization, fixed rain climatology, and serial-dependence
  treatment.
- State that 95% intervals are pointwise while p-values are adjusted by family.
- Report the post-1980 uncertainty in high-deficit-day trends and the adjusted
  uncertainty in ENSO-conditioned monthly trends.
- Add a threshold sensitivity that includes the original paper's 160-mm
  criterion, using the same 69 annual periods. Primary KBDI >650 corresponds
  to 165.1 mm and is not asserted to be an ecological damage threshold.
- Use daily anomaly correspondence as well as raw correlation for PRISM.
- Reduce 11 historical figures to four main figures plus three supplements.

## References and metadata

The supplied export contains Markdown citation keys but no bibliography or
LaTeX source. This draft retains verified relevant keys such as
`KeetchByram1968`, `AbatzoglouWilliams2016`, and `VenablesRipley2002`, and supplies
full entries for all citations actually used, including updated NOAA/Forest
Service data and statistical methods. Original unused keys remain available
in the archived manuscript; they were not fabricated or silently assigned
new bibliographic identities. The ambiguous original `Alexander1992` reference
is not used in the revised bibliography; the implemented calculation is stated
explicitly and the original code retained for comparison.

Confirm author names/affiliations, the retained Florida State University
funding acknowledgement, and journal style before submission. A dedicated
station-history/homogenization assessment and broader independent spatial
validation remain substantive research limitations, described in the paper.
The revised files have not been uploaded to Overleaf, committed, or pushed.
