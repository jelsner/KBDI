# Current paper

- **Edit:** `main.md` and `supplement.md`.
- **Read:** `main.html` and `supplement.html` after rendering.
- **References:** `references.bib` contains the citations used in this revision.
- **Figures:** `figures/` holds four main and five supplementary figures, each
  in vector PDF and 300-dpi PNG form. `figures/manifest.json` and `figures/soil-moisture-manifest.json` record provenance.
- **Tables:** `tables/` exports the two main and ten supplementary tables.

Run from the repository root:

```sh
Rscript scripts/paper-tables.R
Rscript scripts/paper-figures.R
Rscript scripts/soil-moisture-check.R
Rscript scripts/paper-soil-supplement.R
Rscript scripts/render-manuscript.R
Rscript scripts/export-preprint.R
```

The core figure script reads prepared data/results without downloading sources or
refitting models; `paper-soil-supplement.R` exports the two additional Wakulla
figures from the prepared comparison. `scripts/run-all.R` rebuilds the complete analysis and paper
assets. A new data refresh requires editorial review of the numbers and prose
in `main.md` and `supplement.md`; figures/tables regenerate automatically,
but manuscript text is intentionally editable rather than overwritten.

The original April 2022 export and its figures are preserved in
`archive/original-manuscript/`. The export did not include its bibliography;
references used in the rewritten text have been reconstructed from primary
sources and package citations. See `docs/manuscript-changes.md` for the
claim-by-claim changes and remaining editorial checks.

`analysis-summary.tex` is the earlier standalone evidence note that was
already open in the editor. It is retained unchanged and is not the revised
paper source. No new LaTeX document replaces it.

The Markdown sources remain the editable manuscript. EarthArXiv requires a
single PDF rather than a Markdown upload. `scripts/export-preprint.R` combines
the main paper, supplement, and a shared bibliography into
`manuscript/output/pdf/anf-drought-preprint.pdf`, using vector figures and the
existing XeLaTeX installation. The small `preprint-header.tex` file supplies
layout settings; it is not a separate manuscript source.

James B. Elsner is the sole author of this revision, as requested.
Zach T. Law is credited in the acknowledgements for the initial graduate-student
work and manuscript. The affiliation follows the original EarthArXiv PDF;
James B. Elsner is the corresponding author at jelsner@fsu.edu, as requested.
This is a revision of preprint 10.31223/X5H350, not a new unrelated submission.
The export remains a review draft pending scientific review and confirmation
of funding, conflicts, license and the versioned code/data release. See
`docs/eartharxiv-preparation.md` for the remaining submission metadata and
release details. No upload has been made.
