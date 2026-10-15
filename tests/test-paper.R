# Check figure assets, citation resolution, and the meaning of key table values.
stems <- c("fig01-setting-seasonality","fig02-annual-drought-change","fig03-monthly-drought-trends",
 "fig04-natural-fire-application","figS01-daily-drought","figS02-period-covariance-sensitivity","figS03-prism-anomaly-correspondence")
for (stem in stems) {
  pdf <- file.path("manuscript/figures",paste0(stem,".pdf"))
  png <- file.path("manuscript/figures",paste0(stem,".png"))
  stopifnot(file.exists(pdf),file.exists(png),file.info(pdf)$size>1000,file.info(png)$size>1000)
  con <- file(pdf,"rb");magic <- readChar(con,4,useBytes=TRUE);close(con)
  stopifnot(magic=="%PDF")
}
text <- paste(readLines("manuscript/main.md"),collapse="\n")
keys <- unique(substring(regmatches(text,gregexpr("@[A-Za-z][A-Za-z0-9]+",text))[[1]],2))
bib <- paste(readLines("manuscript/references.bib"),collapse="\n")
stopifnot(all(vapply(keys,function(k) grepl(paste0("{",k,","),bib,fixed=TRUE),logical(1))))
annual <- read.csv("manuscript/tables/table01-annual-trends.csv")
original <- read.csv("outputs/annual-trends.csv");original <- subset(original,Endpoint==max(Endpoint))
stopifnot(isTRUE(all.equal(annual$Estimate,original$Estimate*10)),
          identical(annual$LastYear,original$LastYear),identical(annual$N,original$N))
thresholds <- read.csv("manuscript/tables/tableS02-threshold-sensitivity.csv")
primary <- subset(annual,Variable=="HighDays")
check <- subset(thresholds,abs(ThresholdMm-165.1)<1e-6)
stopifnot(isTRUE(all.equal(check$DaysPerDecade,primary$Estimate)),check$N==primary$N)
cat("Paper figure, citation, unit conversion, and primary-threshold checks passed.\n")
