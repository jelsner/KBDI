# Publication tables and a threshold check on identical complete annual years.
stopifnot(requireNamespace("sandwich",quietly=TRUE))
dir.create("manuscript/tables",recursive=TRUE,showWarnings=FALSE)
a <- read.csv("outputs/annual-trends.csv"); a <- subset(a,Endpoint==max(Endpoint))
for (name in c("Estimate","SE","Lower95","Upper95")) a[[name]] <- a[[name]]*10
write.csv(a,"manuscript/tables/table01-annual-trends.csv",row.names=FALSE)
m <- read.csv("outputs/monthly-trends.csv");m <- subset(m,Endpoint==max(Endpoint)&Variable=="MeanDeficit")
for (name in c("Estimate","SE","Lower95","Upper95")) m[[name]] <- m[[name]]*10
write.csv(m,"manuscript/tables/tableS01-monthly-deficit-trends.csv",row.names=FALSE)
f <- subset(read.csv("outputs/forecast-check-metrics.csv"),Cause=="Natural")
write.csv(f,"manuscript/tables/table02-natural-fire-forecasts.csv",row.names=FALSE)
w <- read.csv("Data/processed/daily-weather.csv")
a <- read.csv("Data/processed/annual-climate.csv");years <- a$Year[is.finite(a$MeanDeficit)]
thresholds <- c(152.4,160,165.1,177.8)
results <- list()
for (threshold in thresholds) {
 d <- data.frame(Year=years,Days=vapply(years,function(y) sum(w$Qlm[w$Year==y]>threshold),numeric(1)))
 fit <- lm(Days~Year,data=d)
 se <- sqrt(diag(sandwich::NeweyWest(fit,lag=3,prewhite=FALSE,adjust=TRUE)))["Year"]
 b <- coef(fit)["Year"];z <- qt(.975,df.residual(fit))
 results[[length(results)+1L]] <- data.frame(ThresholdMm=threshold,EquivalentKBDI=threshold/.254,N=length(years),
   DaysPerDecade=b*10,Lower95=(b-z*se)*10,Upper95=(b+z*se)*10)
}
write.csv(do.call(rbind,results),"manuscript/tables/tableS02-threshold-sensitivity.csv",row.names=FALSE)
for (pair in list(c("kbdi-sensitivity.csv","tableS03-kbdi-settings.csv"),
                  c("enso-adjusted-trends.csv","tableS04-enso-adjustment.csv"),
                  c("period-covariance-sensitivity.csv","tableS05-record-covariance.csv"),
                  c("algorithm-trend-sensitivity.csv","tableS06-algorithm-comparison.csv"),
                  c("prism-station-check.csv","tableS07-prism-cells.csv"),
                  c("weather-gaps.csv","tableS08-weather-gaps.csv")))
  file.copy(file.path("outputs",pair[1]),file.path("manuscript/tables",pair[2]),overwrite=TRUE)
