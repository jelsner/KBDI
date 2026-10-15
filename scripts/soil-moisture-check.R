# Independent, exploratory comparison with in-situ Wakulla #1 SCAN observations.
# Run from repo root: Rscript scripts/soil-moisture-check.R [--refresh]
# --refresh downloads official metadata and SMS data; otherwise uses fixed snapshots.
for (p in c('jsonlite','sf','ggplot2')) stopifnot(requireNamespace(p,quietly=TRUE))
args <- commandArgs(TRUE)
raw <- 'Data/raw/scan-wakulla'
dir.create(raw,recursive=TRUE,showWarnings=FALSE)
base <- 'https://wcc.sc.egov.usda.gov/awdbRestApi/services/v1/'
urls <- c(metadata=paste0(base,'stations?stationTriplets=2009:FL:SCAN&returnStationElements=true&activeOnly=false'),
  observations=paste0(base,'data?stationTriplets=2009:FL:SCAN&elements=SMS:*&duration=DAILY&beginDate=1996-12-10&endDate=2026-10-12&periodRef=END&returnFlags=true&returnSuspectData=false'))
paths <- file.path(raw,paste0(names(urls),'.json'));names(paths)<-names(urls)
if ('--refresh' %in% args) {
  options(timeout=max(120,getOption('timeout')))
  for (nm in names(urls)) {
    tmp<-tempfile();download.file(urls[[nm]],tmp,mode='wb',quiet=TRUE)
    parsed<-jsonlite::fromJSON(tmp,simplifyVector=FALSE)
    stopifnot(length(parsed)==1,parsed[[1]]$stationTriplet=='2009:FL:SCAN')
    stopifnot(file.copy(tmp,paths[[nm]],overwrite=TRUE));unlink(tmp)
  }
  jsonlite::write_json(list(retrieved_utc=format(Sys.time(),tz='UTC',usetz=TRUE),
    urls=as.list(urls),md5=as.list(tools::md5sum(paths))),file.path(raw,'download-manifest.json'),pretty=TRUE,auto_unbox=TRUE)
}
stopifnot(all(file.exists(paths)))
m<-jsonlite::fromJSON(paths[['metadata']],simplifyVector=FALSE)[[1]]
o<-jsonlite::fromJSON(paths[['observations']],simplifyVector=FALSE)[[1]]
stopifnot(m$stationTriplet=='2009:FL:SCAN',o$stationTriplet==m$stationTriplet)
getfield<-function(x,k,default=NA_character_)if(is.null(x[[k]]))default else x[[k]]
x<-do.call(rbind,lapply(o$data,function(s) {
  stopifnot(s$stationElement$elementCode=='SMS',s$stationElement$storedUnitCode=='pct',
    s$stationElement$durationName=='DAILY',s$stationElement$ordinal==1)
  do.call(rbind,lapply(s$values,function(v)data.frame(ObservationDate=v$date,
    DepthInches=abs(s$stationElement$heightDepth),SoilMoisturePct=as.numeric(getfield(v,'value')),
    QCFlag=getfield(v,'qcFlag'),QAFlag=getfield(v,'qaFlag'))))
}))
x$ObservationDate<-as.Date(x$ObservationDate)
stopifnot(!anyDuplicated(x[,c('ObservationDate','DepthInches')]),
  identical(sort(unique(x$DepthInches)),c(2L,4L,8L,20L,40L)))
x$DepthCm<-x$DepthInches*2.54
# SMS is instantaneous; DAILY selects midnight, NOT a daily average.
# Pair midnight D with airport end-of-day KBDI for D-1; retain same-date sensitivity.
x$KBDIDate<-x$ObservationDate-1L
w<-read.csv('Data/processed/daily-weather.csv');w$Date<-as.Date(w$Date)
i<-match(x$KBDIDate,w$Date)
x$DeficitMm<-w$Qlm[i];x$SpinupDays<-w$days_since_gap[i]
x$DeficitSameDateMm<-w$Qlm[match(x$ObservationDate,w$Date)]
x$SpinupSameDate<-w$days_since_gap[match(x$ObservationDate,w$Date)]
x$ValidRange<-is.finite(x$SoilMoisturePct)&x$SoilMoisturePct>=0&x$SoilMoisturePct<=100
x$PrimaryEligible<-x$ValidRange&!is.na(x$QCFlag)&x$QCFlag=='V'&
  is.finite(x$DeficitMm)&!is.na(x$SpinupDays)&x$SpinupDays>=365
x$Year<-as.integer(format(x$KBDIDate,'%Y'));x$Month<-as.integer(format(x$KBDIDate,'%m'))
x$YearMonth<-format(x$KBDIDate,'%Y-%m')
x<-x[order(x$DepthInches,x$ObservationDate),]
write.csv(x,'Data/processed/wakulla-soil-moisture-kbdi.csv',row.names=FALSE,na='')
coverage<-do.call(rbind,lapply(split(x,list(x$DepthInches,x$Year),drop=TRUE),function(a) {
  yr<-a$Year[1];ncal<-as.integer(as.Date(paste0(yr+1,'-01-01'))-as.Date(paste0(yr,'-01-01')))
  data.frame(DepthInches=a$DepthInches[1],DepthCm=a$DepthCm[1],Year=yr,CalendarDays=ncal,
    ReturnedRows=nrow(a),ValidFlagDays=sum(a$ValidRange&a$QCFlag=='V',na.rm=TRUE),
    NoProfileDays=sum(a$ValidRange&a$QCFlag=='N',na.rm=TRUE),
    EFlagDays=sum(a$QCFlag=='E',na.rm=TRUE),PairedEligibleDays=sum(a$PrimaryEligible),
    PairedFraction=sum(a$PrimaryEligible)/ncal,
    MeanSoilMoisturePct=if(any(a$PrimaryEligible))mean(a$SoilMoisturePct[a$PrimaryEligible]) else NA_real_)
}))
grid<-expand.grid(DepthInches=sort(unique(x$DepthInches)),Year=seq(min(x$Year),max(x$Year)))
coverage<-merge(grid,coverage,all.x=TRUE)
coverage$DepthCm<-coverage$DepthInches*2.54
coverage$CalendarDays<-as.integer(as.Date(paste0(coverage$Year+1,'-01-01'))-as.Date(paste0(coverage$Year,'-01-01')))
for(k in c('ReturnedRows','ValidFlagDays','NoProfileDays','EFlagDays','PairedEligibleDays','PairedFraction'))coverage[[k]][is.na(coverage[[k]])]<-0
write.csv(coverage,'outputs/soil-moisture-coverage.csv',row.names=FALSE,na='')
rfun<-function(a,b,method='pearson')if(length(a)>2&&sd(a)>0&&sd(b)>0)cor(a,b,method=method) else NA_real_
metrics<-list();n<-0L
for (d in sort(unique(x$DepthInches))) for (qc in c('V','V_and_N')) for(period in c('Full','2007-2024')) for(season in c('All','May','May-July')) {
  a<-subset(x,DepthInches==d&ValidRange&is.finite(DeficitMm)&!is.na(SpinupDays)&SpinupDays>=365&
    QCFlag %in% (if(qc=='V') 'V' else c('V','N')))
  if(period=='2007-2024')a<-subset(a,Year>=2007&Year<=2024)
  if(season=='May')a<-subset(a,Month==5)
  if(season=='May-July')a<-subset(a,Month %in% 5:7)
  if(nrow(a)<3)next
  # In-sample month-of-year centering removes broad seasonality, not sensor drift.
  sm<-a$SoilMoisturePct-ave(a$SoilMoisturePct,a$Month,FUN=mean)
  kb<-a$DeficitMm-ave(a$DeficitMm,a$Month,FUN=mean)
  # Within-year/month centering diagnoses short-term correspondence despite offsets.
  smshort<-a$SoilMoisturePct-ave(a$SoilMoisturePct,a$YearMonth,FUN=mean)
  kbshort<-a$DeficitMm-ave(a$DeficitMm,a$YearMonth,FUN=mean)
  same<-is.finite(a$DeficitSameDateMm)&!is.na(a$SpinupSameDate)&a$SpinupSameDate>=365
  n<-n+1L;metrics[[n]]<-data.frame(DepthInches=d,DepthCm=d*2.54,QCSelection=qc,Period=period,Season=season,
    N=nrow(a),Years=length(unique(a$Year)),FirstDate=min(a$KBDIDate),LastDate=max(a$KBDIDate),
    RawPearsonR=rfun(a$SoilMoisturePct,a$DeficitMm),RawSpearmanR=rfun(a$SoilMoisturePct,a$DeficitMm,'spearman'),
    MonthAnomalyR=rfun(sm,kb),WithinYearMonthR=rfun(smshort,kbshort),
    SameDateN=sum(same),SameDateR=rfun(a$SoilMoisturePct[same],a$DeficitSameDateMm[same]))
}
metrics<-do.call(rbind,metrics)
write.csv(metrics,'outputs/soil-moisture-correspondence.csv',row.names=FALSE,na='')
# Complete-enough months only, with full calendar denominator and matching paired days.
a<-subset(x,PrimaryEligible)
monthly<-do.call(rbind,lapply(split(a,list(a$DepthInches,a$YearMonth),drop=TRUE),function(z){
  first<-as.Date(paste0(z$YearMonth[1],'-01'));last<-seq(first,by='month',length.out=2)[2]-1
  data.frame(DepthCm=z$DepthCm[1],Date=first,N=nrow(z),CalendarDays=as.integer(last-first)+1L,
    SoilMoisturePct=mean(z$SoilMoisturePct),DeficitMm=mean(z$DeficitMm))
}))
monthly$Coverage<-monthly$N/monthly$CalendarDays
monthly<-subset(monthly,Coverage>=.8)
write.csv(monthly,'outputs/soil-moisture-monthly.csv',row.names=FALSE)
# Draw gaps explicitly rather than connecting across absent months.
monthly<-monthly[order(monthly$DepthCm,monthly$Date),]
monthly$Segment<-ave(as.integer(format(monthly$Date,'%Y'))*12+as.integer(format(monthly$Date,'%m')),
  monthly$DepthCm,FUN=function(v)cumsum(c(TRUE,diff(v)!=1)))
library(ggplot2)
theme_set(theme_minimal(base_size=11)+theme(panel.grid.minor=element_blank()))
p<-ggplot(monthly,aes(Date,SoilMoisturePct,group=interaction(DepthCm,Segment)))+
  geom_line(color='#007A87',linewidth=.4)+facet_wrap(~DepthCm,ncol=1,scales='free_y',labeller=label_both)+
  labs(title='Observed soil moisture inside the ANF: Wakulla #1 SCAN',
    subtitle='Monthly means of validated midnight observations; at least 80% paired-day coverage',
    x=NULL,y='Volumetric soil moisture (%)',caption='Exploratory: gaps and sensor offsets require review before trend interpretation.')
ggsave('outputs/wakulla-soil-moisture-timeseries.png',p,device=grDevices::png,width=9,height=8,dpi=180,bg='white',
  type=if(capabilities('aqua'))'quartz' else getOption('bitmapType'))
p<-ggplot(coverage,aes(Year,factor(DepthCm),fill=PairedFraction))+geom_tile()+
  scale_fill_viridis_c(limits=c(0,1),name='Paired fraction')+
  labs(title='Wakulla soil-moisture / airport KBDI coverage',x=NULL,y='Sensor depth (cm)',
    subtitle='Validated (V) observations and KBDI after 365 complete weather days; calendar-year denominator')
ggsave('outputs/wakulla-soil-moisture-coverage.png',p,device=grDevices::png,width=9,height=3.5,dpi=180,bg='white',
  type=if(capabilities('aqua'))'quartz' else getOption('bitmapType'))
b<-sf::st_transform(sf::st_read('Data/raw/anf-boundary.gpkg',quiet=TRUE),4326)
point<-sf::st_as_sf(data.frame(x=m$longitude,y=m$latitude),coords=c('x','y'),crs=4326)
airport<-sf::st_as_sf(data.frame(x=-84.35136,y=30.39354),coords=c('x','y'),crs=4326)
jsonlite::write_json(list(created_utc=format(Sys.time(),tz='UTC',usetz=TRUE),station=m$stationTriplet,
  latitude=m$latitude,longitude=m$longitude,InsideStudyPolygon=lengths(sf::st_intersects(point,b))>0,
  AirportDistanceKm=as.numeric(sf::st_distance(point,airport))/1000,
  units='Percent volumetric soil moisture; KBDI deficit in mm',
  timing='Midnight SMS on D paired with end-of-day KBDI on D-1; same-date sensitivity reported',
  primary_qc='V only; V and N sensitivity; E excluded; API returnSuspectData=false',
  status='Exploratory independent-observation comparison, not historical trend validation or calibrated water-deficit validation',
  input_md5=as.list(tools::md5sum(c(paths,'Data/processed/daily-weather.csv','Data/raw/anf-boundary.gpkg')))),
  'outputs/soil-moisture-manifest.json',pretty=TRUE,auto_unbox=TRUE)
print(subset(metrics,QCSelection=='V'&Period=='Full'&Season=='All'))
