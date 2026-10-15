# Export exploratory Wakulla supplementary figures and tables from prepared results.
# Run scripts/soil-moisture-check.R first. No network or fitting here.
for(p in c('ggplot2','jsonlite'))stopifnot(requireNamespace(p,quietly=TRUE))
library(ggplot2)
dir.create('manuscript/figures',recursive=TRUE,showWarnings=FALSE)
dir.create('manuscript/tables',recursive=TRUE,showWarnings=FALSE)
monthly<-read.csv('outputs/soil-moisture-monthly.csv');monthly$Date<-as.Date(monthly$Date)
monthly<-monthly[order(monthly$DepthCm,monthly$Date),]
monthly$Segment<-ave(as.integer(format(monthly$Date,'%Y'))*12+as.integer(format(monthly$Date,'%m')),
 monthly$DepthCm,FUN=function(v)cumsum(c(TRUE,diff(v)!=1)))
coverage<-read.csv('outputs/soil-moisture-coverage.csv')
theme_set(theme_minimal(base_size=10)+theme(panel.grid.minor=element_blank(),
 plot.title=element_text(face='bold',size=11),legend.position='bottom'))
export<-function(stem,p,width,height){
 path<-file.path('manuscript/figures',stem)
 ggsave(paste0(path,'.pdf'),p,device=grDevices::pdf,width=width,height=height,family='Helvetica',useDingbats=FALSE)
 ggsave(paste0(path,'.png'),p,device=grDevices::png,width=width,height=height,units='in',res=300,
   type=if(capabilities('aqua'))'quartz' else getOption('bitmapType'),bg='white')
}
p<-ggplot(monthly,aes(Date,SoilMoisturePct,group=interaction(DepthCm,Segment)))+
 geom_line(color='#007A87',linewidth=.35)+facet_wrap(~DepthCm,ncol=1,scales='free_y',
 labeller=as_labeller(function(x)paste(x,'cm')))+labs(x=NULL,y='Volumetric soil moisture (%)')
export('figS04-wakulla-soil-moisture',p,7.2,7.3)
p<-ggplot(coverage,aes(Year,factor(DepthCm),fill=PairedFraction))+geom_tile()+
 scale_fill_viridis_c(limits=c(0,1),name='Fraction of calendar days paired')+
 labs(x=NULL,y='Sensor depth (cm)')
export('figS05-wakulla-coverage',p,7.2,3.2)
file.copy('outputs/soil-moisture-correspondence.csv','manuscript/tables/tableS09-wakulla-correspondence.csv',overwrite=TRUE)
file.copy('outputs/soil-moisture-coverage.csv','manuscript/tables/tableS10-wakulla-coverage.csv',overwrite=TRUE)
inputs<-c('outputs/soil-moisture-monthly.csv','outputs/soil-moisture-coverage.csv','outputs/soil-moisture-correspondence.csv','outputs/soil-moisture-manifest.json')
assets<-list.files('manuscript/figures',pattern='^figS0[45].*\\.(pdf|png)$',full.names=TRUE)
jsonlite::write_json(list(created_utc=format(Sys.time(),tz='UTC',usetz=TRUE),
 input_md5=as.list(tools::md5sum(inputs)),output_md5=as.list(tools::md5sum(assets))),
 'manuscript/figures/soil-moisture-manifest.json',pretty=TRUE,auto_unbox=TRUE)
cat('Exported Wakulla Figures S4-S5 and Tables S9-S10.\n')
