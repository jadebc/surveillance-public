##########################################
# Shoo the Flu evaluation
# Surveillance analysis

# Figure with vaccine effectiveness 
# from relevant years
##########################################

rm(list=ls())
source(here::here("0-config.R"))

d=read.csv("~/Dropbox/Literature/0Flu/0Vaccine effectiveness/CDC/CDC_VE.csv")
d <- d %>% select(vaccine_type,type,strain,child_ve,lb,ub,seas,
                  domstrain,citation,child_age) %>%
  filter(!is.na(seas))

d=d[d$type!="Any",]
d=d[d$vaccine_type!="LAIV4",]
d=d[d$vaccine_type!="IIV4",]

d$season[d$seas==1415]="2014-15"
d$season[d$seas==1516]="2015-16"
d$season[d$seas==1617]="2016-17"
d$season[d$seas==1718]="2017-18"
d$season=as.factor(d$season)

d$strain[d$strain=="B/Yamagata"]="B"

d$strain=factor(d$strain,levels=c("B","A/H1N1pdm09","A/H3N2"))

ann1=data.frame(strain=3,child_ve=50,lab = "Dominant/nstrain",
           season = factor("2014-15",levels = levels(d$season)))
ann2=data.frame(strain=2,child_ve=90,lab = "Dominant/nstrain",
                season = factor("2015-16",levels = levels(d$season)))
ann3=data.frame(strain=3,child_ve=78,lab = "Dominant/nstrain",
                season = factor("2016-17",levels = levels(d$season)))
ann4=data.frame(strain=3,child_ve=83,lab = "Dominant/nstrain",
                season = factor("2017-18",levels = levels(d$season)))

# manually remove lb and ub
# for 2015-16 LAIV because the bounds
# are too wide
d$lb[d$seas==1516 & d$vaccine_type=="LAIV"
     & d$strain=="A/H1N1pdm09"]=-80
# d$ub[d$seas==1516 & d$vaccine_type=="LAIV"
#      & d$strain=="A/H1N1pdm09"]=NA 

# plot on log10 scale
g=ggplot(d,aes(y=child_ve,x=strain))+
  geom_point(aes(shape=vaccine_type),position=position_dodge(width=0.3),size=3)+
  geom_errorbar(aes(shape=vaccine_type,ymin=lb,ymax=ub),position=position_dodge(width=0.3),width=0.2)+
  coord_flip()+
  geom_hline(yintercept=0,linetype="dashed")+
  facet_grid(~season)+theme_bw()+xlab("Influenza strain")+
  ylab("Vaccine effectiveness (%)")+
  scale_y_continuous(limits=c(-80,100),
                     breaks=seq(-150,140,25),labels=seq(-150,140,25))+
  scale_shape_manual("Vaccination type",values=c(18,20))+
  theme(legend.position="bottom",
        axis.text.x = element_text(size=5))

gp=g+geom_text(data=ann1,label="*",size=5)+
  geom_text(data=ann2,label="*",size=5)+
  geom_text(data=ann3,label="*",size=5)+
  geom_text(data=ann4,label="*",size=5)

ggsave(plot=gp,filename=paste0(fig_dir,"fig-cdc-ve.pdf"),width=8,height=3)




