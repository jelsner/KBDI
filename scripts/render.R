if (!requireNamespace("rmarkdown", quietly=TRUE)) stop("Install rmarkdown and Pandoc.")
project_root <- normalizePath(".")
for (file in c("KBDI.Rmd", "ANF_Fires.Rmd", "PredictionModel.Rmd", "Gridded_KBDI.Rmd"))
  rmarkdown::render(file.path(project_root,"analysis",file), knit_root_dir=project_root,
    output_dir=file.path(project_root,"outputs"), envir=new.env(parent=globalenv()),
    output_options=list(pandoc_args=paste0("--resource-path=",project_root)), quiet=TRUE)
