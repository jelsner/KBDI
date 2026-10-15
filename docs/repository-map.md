# Repository map

The root contains the project file, README, package lock, and directories.
Begin with the primary drought report in `analysis/KBDI.Rmd`. The remaining
active notebooks provide wildfire context, forecast evaluation, and the
historical PRISM comparison. They consume prepared tables and are rendered
from the project root by `scripts/render.R`.

`Data/raw/` and `Data/processed/` hold the small, reproducible active inputs.
Large historical archives remain in `Data/` and are ignored. The 2009–2018
PRISM forest extraction in `Data/raw/` reproduces the spatial check without
reading the complete continental archive.

`archive/original-manuscript/` preserves the supplied paper and figures.
`archive/original-manuscript.zip` preserves its source export.
`archive/legacy/` preserves the earlier core analysis and original hydrology
functions used in the algorithm comparison.
`archive/notebooks/` holds peripheral temperature, weather-station, thesis,
and exploratory notebooks.
`archive/figures/`, `archive/fits/`, and `archive/data/` retain old outputs.
`archive/reference/` holds the historical national forest boundary shapefile;
the active workflow uses the compact fixed ANF boundary in `Data/raw/`.
`archive/workspaces/` holds old R sessions/history and is ignored by Git.
`archive/compatibility/` preserves the redundant root hydrology copies.
`archive/planning/` holds preliminary paper-revision notes superseded by the
current manuscript. The already-open `manuscript/analysis-summary.tex` is
retained as an earlier working note; it is not the current paper source.

Paths in historical notebooks have not been repaired or executed. Current
scripts, reports, and tests use the reorganized paths. No commits or remote
pushes are performed as part of the repository cleanup.
