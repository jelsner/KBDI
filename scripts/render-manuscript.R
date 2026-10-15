# Self-contained HTML previews from the editable Markdown manuscript.
if (!requireNamespace("rmarkdown",quietly=TRUE)) stop("Install rmarkdown and Pandoc.")
local({
  original_dir <- getwd()
  on.exit(setwd(original_dir))
  setwd("manuscript")
  for (name in c("main","supplement"))
    rmarkdown::pandoc_convert(input=paste0(name,".md"),from="markdown",to="html5",
      output=paste0(name,".html"),
      options=c("--standalone","--self-contained","--mathml",rmarkdown::pandoc_citeproc_args(),"--css=paper.css"),verbose=FALSE)
})
cat("Rendered manuscript/main.html and manuscript/supplement.html.\n")
