# Long-term drought change near Apalachicola National Forest

This project analyzes long-term **station-derived drought change**, with
seasonal wildfire prediction as an application.

## Start here

- [Revised paper](manuscript/main.md) and [supplement](manuscript/supplement.md).
- [Figure-redrafting script](scripts/paper-figures.R): PDF and 300-dpi PNG exports.
- `analysis/KBDI.Rmd` is the primary drought analysis; rendered reports are in `outputs/`.
- [Manuscript guide](manuscript/README.md) explains writing, rendering, and figure assets.
- [Methods and reproduction](docs/methods-and-reproduction.md) explains data,
  statistical assumptions, dependencies, and limitations.
- [Repository map](docs/repository-map.md) explains the directory structure.

```sh
Rscript scripts/run-all.R              # reproduce from cached data
Rscript scripts/run-all.R --refresh    # refresh station, fire, and SOI sources
Rscript scripts/paper-figures.R        # redraft figures only from prepared results
Rscript scripts/render-manuscript.R    # preview the editable paper as HTML
```

Run from the directory containing `KBDI.Rproj`. Use `renv.lock` to restore the
recorded R package versions; R Markdown rendering requires Pandoc.

## Repository layout

| Directory | Contents |
|---|---|
| `analysis/` | Four current R Markdown analysis reports |
| `scripts/` | Data preparation, modeling, diagnostics, and rendering |
| `tests/` | Hydrology, data integrity, and statistical workflow checks |
| `Data/raw/` | Small source snapshots used by the active pipeline |
| `Data/processed/` | Derived, reproducible analysis tables |
| `Data/` (other entries) | Large historical local datasets, including PRISM |
| `outputs/` | Reproducible analysis results and diagnostic plots |
| `manuscript/` | Current manuscript and publication assets |
| `docs/` | Reproduction details, repository map, and move inventory |
| `archive/` | Original paper, historical figures, notebooks, and fits |

The cleanup relocates historical files without deleting them. The original
paper is in `archive/original-manuscript/main.md`; the move inventory is
`docs/repository-moves.csv`. Archived code is reference material and is not
part of the active workflow. Large saved R sessions are kept locally under
`archive/workspaces/` and ignored by Git.
