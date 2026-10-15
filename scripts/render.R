if (!requireNamespace("rmarkdown", quietly=TRUE)) stop("Install rmarkdown and Pandoc.")
for (file in c("KBDI.Rmd", "ANF_Fires.Rmd", "PredictionModel.Rmd", "Gridded_KBDI.Rmd"))
  rmarkdown::render(file, output_dir="outputs", envir=new.env(parent=globalenv()), quiet=TRUE)
