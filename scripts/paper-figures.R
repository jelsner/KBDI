# Redraft every manuscript figure from prepared results, without network access.
# Run from the repository root: Rscript scripts/paper-figures.R
for (pkg in c("ggplot2", "sf", "jsonlite")) stopifnot(requireNamespace(pkg, quietly=TRUE))
library(ggplot2)
requested_figures <- sub("^--figure=", "", commandArgs(TRUE)[grepl("^--figure=",commandArgs(TRUE))])
figure_dir <- "manuscript/figures"
dir.create(figure_dir, recursive=TRUE, showWarnings=FALSE)
theme_set(theme_minimal(base_size=10, base_family="sans") +
  theme(panel.grid.minor=element_blank(), plot.title=element_text(face="bold",size=11),
        legend.position="bottom", plot.margin=margin(8,10,8,8)))
blue <- "#007A87"; orange <- "#C26220"
annual <- read.csv("Data/processed/annual-climate.csv")
monthly <- read.csv("Data/processed/monthly-climate.csv")
trends <- read.csv("outputs/annual-trends.csv")
monthly_trends <- read.csv("outputs/monthly-trends.csv")
latest <- max(trends$Endpoint)
trends <- subset(trends, Endpoint==latest)
boundary <- sf::st_transform(sf::st_read("Data/raw/anf-boundary.gpkg",quiet=TRUE),4326)
# Coordinates from NOAA CDO station metadata, checked 2026-10-15.
airport <- sf::st_as_sf(data.frame(Longitude=-84.35136,Latitude=30.39354),
                         coords=c("Longitude","Latitude"),crs=4326)
export_panels <- function(stem, plots, columns=1, width=7.2, height=5) {
  if (length(requested_figures) && !stem %in% requested_figures) return(invisible(NULL))
  draw <- function() {
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(layout=grid::grid.layout(ceiling(length(plots)/columns),columns)))
    for (i in seq_along(plots)) print(plots[[i]],newpage=FALSE,
      vp=grid::viewport(layout.pos.row=ceiling(i/columns),layout.pos.col=((i-1)%%columns)+1))
    grid::popViewport()
  }
  grDevices::pdf(file.path(figure_dir,paste0(stem,".pdf")),width=width,height=height,
                 onefile=TRUE,family="Helvetica",useDingbats=FALSE,paper="special")
  tryCatch(draw(),finally=grDevices::dev.off())
  grDevices::png(file.path(figure_dir,paste0(stem,".png")),width=width,height=height,
                 units="in",res=300,type=if (capabilities("aqua")) "quartz" else getOption("bitmapType"),bg="white")
  tryCatch(draw(),finally=grDevices::dev.off())
  stopifnot(file.exists(file.path(figure_dir,paste0(stem,".pdf"))),
            file.exists(file.path(figure_dir,paste0(stem,".png"))))
}
map <- ggplot() + geom_sf(data=boundary,fill="#DCE8D9",color="#465844",linewidth=.4) +
  geom_sf(data=airport,shape=16,size=2.5,color=blue) +
  annotate("text",x=-84.52,y=30.49,label="Tallahassee airport",size=3,color=blue) +
  coord_sf(xlim=c(-85.15,-84.19),ylim=c(29.86,30.53),expand=FALSE) +
  labs(title="A  Study footprint and station",x=NULL,y=NULL)
climatology <- ggplot(subset(monthly,is.finite(MeanDeficit)),
  aes(factor(Month,levels=1:12,labels=month.abb),MeanDeficit)) +
  geom_boxplot(fill="#D9EAEC",color=blue,outlier.size=.7,linewidth=.35) +
  labs(title="B  Seasonal drought distribution",x=NULL,y="Monthly mean deficit (mm)") +
  theme(axis.text.x=element_text(angle=45,hjust=1))
export_panels("fig01-setting-seasonality",list(map,climatology),columns=2,width=7.5,height=3.5)
annual_panel <- function(variable,title,ylabel) {
  d <- data.frame(Year=annual$Year,Value=annual[[variable]])
  d <- subset(d,is.finite(Value)); t <- subset(trends,Variable==variable)
  annotation <- sprintf("Trend: %.2f per decade\n95%% interval: %.2f to %.2f",t$Estimate*10,t$Lower95*10,t$Upper95*10)
  ggplot(d,aes(Year,Value)) + geom_point(size=1.4,color="#333333") +
    geom_smooth(method="lm",formula=y~x,se=FALSE,color=blue,linewidth=.8) +
    annotate("text",x=min(d$Year)+2,y=Inf,label=annotation,hjust=0,vjust=1.2,size=3) +
    scale_y_continuous(expand=expansion(mult=c(.04,.2))) +
    labs(title=title,x=NULL,y=ylabel)
}
export_panels("fig02-annual-drought-change",list(
  annual_panel("MeanDeficit","A  Mean annual moisture deficit","Deficit (mm)"),
  annual_panel("HighDays","B  Annual high-deficit exposure","Days with KBDI > 650")),height=6)
t <- subset(monthly_trends,Endpoint==latest & Variable=="MeanDeficit")
p <- ggplot(t,aes(factor(Month,levels=1:12,labels=month.abb),Estimate*10)) +
  geom_hline(yintercept=0,color="grey50",linewidth=.4) +
  geom_errorbar(aes(ymin=Lower95*10,ymax=Upper95*10),width=.15,linewidth=.5) +
  geom_point(aes(color=PAdjusted<.05),size=2.5) +
  scale_color_manual(values=c("#777777",blue),labels=c("No","Yes"),name="BH-adjusted p < 0.05") +
  labs(x=NULL,y="Deficit trend (mm per decade)")
export_panels("fig03-monthly-drought-trends",list(p),height=3.8)
h <- subset(read.csv("outputs/hindcasts.csv"),Cause=="Natural")
observed <- subset(h,Model=="Drought")
p1 <- ggplot(h,aes(Year,Expected,color=Model)) + geom_line(linewidth=.7) +
  geom_point(data=observed,aes(y=Observed),inherit.aes=TRUE,color="#222222",size=1.5) +
  scale_color_manual(values=c(Climatology=orange,Drought=blue),name="Forecast mean") +
  scale_x_continuous(breaks=c(2010,2015,2020,2024)) +
  labs(title="A  Preceding-year hindcasts",x=NULL,y="Natural fires, May-July")
met <- subset(read.csv("outputs/metrics.csv"),Cause=="Natural")
met$Model <- factor(met$Model,levels=c("Climatology","Drought"))
u <- subset(read.csv("outputs/forecast-skill-uncertainty.csv"),Cause=="Natural")
p2 <- ggplot(met,aes(Model,MAE,fill=Model)) + geom_col(width=.55) +
  geom_text(aes(label=sprintf("%.2f",MAE)),vjust=-.4,size=3.4) +
  scale_fill_manual(values=c(Climatology=orange,Drought=blue),guide="none") +
  scale_y_continuous(limits=c(0,14),expand=expansion(mult=c(0,.03))) +
  annotate("text",x=1.5,y=13,label=sprintf("Paired gain: %.2f fires\n95%% interval: %.2f to %.2f",u$MAEImprovement,u$Lower95,u$Upper95),size=3) +
  labs(title="B  Forecast error and paired uncertainty",x=NULL,y="Mean absolute error (fires)")
export_panels("fig04-natural-fire-application",list(p1,p2),columns=2,width=7.5,height=3.7)
w <- read.csv("Data/processed/daily-weather.csv");w$Date <- as.Date(w$Date)
w <- subset(w,Year<=latest & days_since_gap>=365 & is.finite(Qlm))
w$CalendarDay <- as.Date(paste0("2000-",format(w$Date,"%m-%d")))
p <- ggplot(w,aes(CalendarDay,Year,fill=Qlm)) + geom_tile() +
  scale_fill_distiller(palette="BrBG",direction=-1,name="Deficit (mm)") +
  scale_x_date(date_breaks="2 months",date_labels="%b") + labs(x=NULL,y=NULL)
export_panels("figS01-daily-drought",list(p),height=5)
s <- read.csv("outputs/period-covariance-sensitivity.csv")
s$Outcome <- ifelse(s$Variable=="MeanDeficit","Mean deficit (mm per decade)","High-deficit days (days per decade)")
s$Start <- factor(s$Start,levels=c(1944,1960,1980))
s$Lag <- factor(s$HACLag)
p <- ggplot(s,aes(Start,Estimate*10,color=Lag)) +
  geom_hline(yintercept=0,color="grey60",linewidth=.4) +
  geom_errorbar(aes(ymin=Lower95*10,ymax=Upper95*10),width=.12,position=position_dodge(width=.6)) +
  geom_point(position=position_dodge(width=.6),size=1.7) +
  facet_wrap(~Outcome,scales="free_y") +
  scale_color_brewer(palette="Dark2",name="Annual HAC lag") +
  labs(x="Requested record start",y="Trend per decade")
export_panels("figS02-period-covariance-sensitivity",list(p),width=7.5,height=4)
s <- read.csv("outputs/prism-station-check.csv")
p <- ggplot(s,aes(Longitude,Latitude,fill=AnomalyR2)) + geom_tile() +
  geom_sf(data=boundary,inherit.aes=FALSE,fill=NA,color="#333333",linewidth=.4) +
  geom_sf(data=airport,inherit.aes=FALSE,shape=8,size=2.5,color="#111111") +
  scale_fill_viridis_c(limits=c(0,1),name=expression("Daily anomaly "*R^2)) +
  coord_sf() + labs(x=NULL,y=NULL)
export_panels("figS03-prism-anomaly-correspondence",list(p),height=4.8)
inputs <- c("Data/raw/anf-boundary.gpkg","Data/processed/annual-climate.csv","Data/processed/monthly-climate.csv",
  "Data/processed/daily-weather.csv","outputs/annual-trends.csv","outputs/monthly-trends.csv",
  "outputs/hindcasts.csv","outputs/metrics.csv","outputs/forecast-skill-uncertainty.csv",
  "outputs/period-covariance-sensitivity.csv","outputs/prism-station-check.csv")
jsonlite::write_json(list(created_utc=format(Sys.time(),tz="UTC",usetz=TRUE),
  station_metadata=list(latitude=30.39354,longitude=-84.35136,
    url="https://www.ncdc.noaa.gov/cdo-web/datasets/GHCND/stations/GHCND:USW00093805/detail",checked="2026-10-15"),
  inputs_md5=as.list(tools::md5sum(inputs)),output_md5=as.list(tools::md5sum(list.files(figure_dir,pattern="\\.(pdf|png)$",full.names=TRUE)))),
  file.path(figure_dir,"manifest.json"),auto_unbox=TRUE,pretty=TRUE)
if (length(requested_figures)) cat("Redrafted:",paste(requested_figures,collapse=", "),"as vector PDF and 300-dpi PNG.\n") else
  cat("Redrafted four main and three supplementary figures as vector PDF and 300-dpi PNG.\n")
