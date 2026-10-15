# Export the editable Markdown manuscript and supplement as one EarthArXiv PDF.
# Requires Pandoc, pandoc-citeproc (for older Pandoc), and an existing XeLaTeX.
if(!requireNamespace('rmarkdown',quietly=TRUE))stop('Install rmarkdown.')
if(!nzchar(Sys.which('xelatex')))stop('XeLaTeX is required for this Markdown PDF export.')
local({
 old<-getwd();on.exit(setwd(old));setwd('manuscript')
 dir.create('output/pdf',recursive=TRUE,showWarnings=FALSE)
 strip_yaml<-function(x){if(length(x)&&x[1]=='---'){end<-which(x[-1]=='---')[1]+1L;x<-x[-seq_len(end)]};x}
 main<-readLines('main.md');supp<-strip_yaml(readLines('supplement.md'))
 # One shared bibliography at the end of the combined submission PDF.
 drop_refs<-function(x){i<-which(x=='# References');if(length(i))x<-head(x,i[1]-1);x}
 combined<-c(drop_refs(main),'','\\clearpage','','# Supplementary material','',
   drop_refs(supp),'','\\clearpage','','# References','','::: {#refs}',':::')
 # Use vector publication assets in PDF, while HTML previews use PNG.
 combined<-gsub('(figures/[^)]+)\\.png', '\\1.pdf',combined)
 combined<-gsub('\\[([^]]+)\\]\\(tables/[^)]+\\)', '\\1',combined)
 tmp<-tempfile(pattern='eartharxiv-',tmpdir='.',fileext='.md');on.exit(unlink(tmp),add=TRUE)
 writeLines(combined,tmp)
 rmarkdown::pandoc_convert(input=tmp,from='markdown',to='latex',
 output='output/pdf/anf-drought-preprint.pdf',options=c('--standalone',
 rmarkdown::pandoc_citeproc_args(),'--pdf-engine=xelatex','--top-level-division=section',
 '--include-in-header=preprint-header.tex','-V','geometry:margin=0.85in','-V','fontsize:11pt','-V','colorlinks:true',
 '-V','linkcolor:teal','-V','urlcolor:teal','-V','linestretch:1.12'),verbose=FALSE)
})
cat('Exported manuscript/output/pdf/anf-drought-preprint.pdf (paper, supplement, references).\n')
