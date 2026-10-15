# EarthArXiv preprint preparation

Prepared 15 October 2026. This is a local review draft; no submission has been made.

## Files and format

Edit `manuscript/main.md`, `manuscript/supplement.md`, and `manuscript/references.bib`.
Run `Rscript scripts/render-manuscript.R` for local HTML previews.
Run `Rscript scripts/export-preprint.R` for one PDF containing the paper,
supplement and shared references. The export uses vector figures. Relative
CSV links are rendered as text in the PDF; the CSVs remain in the repository
and are linked in local HTML previews. Export requires the existing XeLaTeX
installation and Pandoc; it does not replace the open `analysis-summary.tex` note.

EarthArXiv's official moderation policy asks for a single PDF. The eScholarship
submission guide asks for the article PDF and links to supplemental data.
Markdown is the source for this workflow, not the primary submission file.
Software and tabular data should have stable external repository links.

- https://eartharxiv.github.io/moderation.html
- https://help.escholarship.org/support/solutions/articles/9000222892-submit-your-preprint-to-eartharxiv-ecoevorxiv

## Scientific framing

The paper leads with the station-derived annual and calendar-month drought
trends. Wildfire counts are a retrospective prediction application. Six newer
research articles were added: Hoylman (2022, 2024), Donovan (2023), Elias (2024),
Hashida (2025), and Nowell (2026). Their use is limited to relevant drought
assessment, soil-moisture interpretation, drought features and regional fire
management context. Regional findings are not evidence of local attribution.
The retracted Gebrechorkos et al. (2025) paper is not cited.

Wakulla SCAN is included as an exploratory supplement (Figures S4-S5, Tables
S9-S10), with a short description in the main paper. It supplies independent
in-situ observations but is not a calibration of KBDI's nominal mm deficit or
validation of the historical trend. Sensor level shifts and gaps are retained;
no intervals were removed to improve correlation. No daily correlation
significance tests or inferential intervals are claimed.

## Before submitting

The original is EarthArXiv preprint 10.31223/X5H350 (record 3268), posted
15 April 2022. The user confirms it was posted there only. Use the existing
record's revision workflow rather than creating a duplicate preprint. Author
order and affiliation were verified from its original PDF: Zachery T. Law and
James B. Elsner; Department of Geography, Florida State University, Tallahassee,
FL 32306, USA. The revised author order is James B. Elsner followed by Zachery T. Law,
as requested by the user; the original preprint citation retains its original
author order. The corresponding email is jelsner@fsu.edu.
The original record lists CC BY 4.0; confirm the license for the revised version
when completing the live submission form.
Confirm the inherited Florida State University funding statement and supply
conflict-of-interest and author-contribution statements as applicable.
Choose the submission license with all authors' agreement.

Create and publish a versioned code/data release, and insert its actual commit
or archive DOI into the data-availability statement. Local revisions and new
supplemental assets must be committed/pushed before their availability can be
claimed. Do not describe an uncreated DOI or unpublished release as available.
Confirm that snapshot redistribution and any previous publication agreement
permit posting the selected version. Rebuild and review the final PDF after
metadata changes, then use the live submission form to confirm current fields.

The local PDF remains labelled as a revision draft and has not been uploaded. This document records preparation requirements; it does not grant
permission to upload or publish.
