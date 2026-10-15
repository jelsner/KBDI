# Current paper

- **Edit:** `main.md` and `supplement.md`.
- **Read:** `main.html` and `supplement.html` after rendering.
- **References:** `references.bib` contains the citations used in this revision.
- **Figures:** `figures/` holds four main and three supplementary figures, each
  in vector PDF and 300-dpi PNG form. `figures/manifest.json` records provenance.
- **Tables:** `tables/` exports the two main and eight supplementary tables.

Run from the repository root:

```sh
Rscript scripts/paper-tables.R
Rscript scripts/paper-figures.R
Rscript scripts/render-manuscript.R
```

The figure script reads prepared data/results without downloading sources or
refitting models. `scripts/run-all.R` rebuilds the complete analysis and paper
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
