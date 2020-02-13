##########################################
# Shoo the Flu evaluation
# Surveillance analysis

# Figure with rates by district by year
# for ICU and mortality
##########################################
rm(list=ls())
source(here::here("0-config.R"))

# Load data
icu_death = readRDS(paste0(data_dir, "ceip-icu_death-data-age-sex-zip.RDS"))

#----------------------------------------------------
# Obtain rates
#----------------------------------------------------
icu_table = icu_death %>% 
  filter(seas>=1112) %>%
  group_by(season, dist) %>%
  summarise(icu = sum(icu),
            pyears = sum(N)) %>%
  mutate(tr = ifelse(dist == "OUSD", "Intervention", "Comparison"),
         rate = icu/pyears * 100000, 
         lb = binom.confint(icu, pyears,methods="wilson",type="central")$lower * 100000,
         ub = binom.confint(icu, pyears,methods="wilson",type="central")$upper * 100000)

deaths_table = icu_death %>% group_by(season, dist) %>%
  filter(seas>=1112) %>%
  summarise(deaths = sum(deaths),
            pyears = sum(N)) %>%
  mutate(tr = ifelse(dist == "OUSD", "Intervention", "Comparison"),
         rate = deaths/pyears * 100000, 
         lb = binom.confint(deaths, pyears,methods="wilson",type="central")$lower * 100000,
         ub = binom.confint(deaths, pyears,methods="wilson",type="central")$upper * 100000)

#----------------------------------------------------
# Make plots
#----------------------------------------------------
pdf(file=paste0(fig_dir, "flu_mort_to201718.pdf"),
    onefile=TRUE,width=7,height=4)
ggplot(deaths_table)+
  geom_point(aes(x=season,y=rate,col=tr),
             position=position_dodge(width=0.3))+
  geom_errorbar(aes(x=season,ymin=lb,ymax=ub,col=tr),
                width=0.1,position=position_dodge(width=0.3))+
  ylab("Influenza mortality per 100,000")+xlab("Influenza season")+
  scale_y_continuous(limits=c(0,8), breaks = seq(0,8,1),
                     labels = seq(0,8,1))+
  scale_colour_manual(name="",  values=c("#ff9715","#2185c5"))+
  theme_complete_bw()+theme(legend.position="bottom") +
  
  # Label program period
  annotate(geom="text", x = 2, y = 7.8,
           label="Pre-intervention period", size = 4)+
  
  annotate(geom="text", x = 4.5, y = 7.8,
           label="Intervention period", size = 4)+
  
  annotate(geom="text", x = 4.5, y = 7.25,
           label="(Ineffective vaccine)", size = 3)+
  
  annotate(geom="text", x = 6.5, y = 7.8,
           label="Intervention period", size = 4)+
  
  annotate(geom="text", x = 6.5, y = 7.25,
           label="(Moderately effective vaccine)", size = 3)+
  
  geom_vline(xintercept = 3.5, linetype = "dashed")
dev.off()


pdf(file=paste0(fig_dir,"flu_icu_to201718.pdf"),
    onefile=TRUE,width=7,height=4)
ggplot(icu_table)+
  geom_point(aes(x=season,y=rate,col=tr),
             position=position_dodge(width=0.3))+
  geom_errorbar(aes(x=season,ymin=lb,ymax=ub,col=tr),
                width=0.1,position=position_dodge(width=0.3))+
  ylab("Influenza ICU admissions per 100,000")+xlab("Influenza season")+
  scale_colour_manual(name="",  values=c("#ff9715","#2185c5"))+
  theme_complete_bw()+theme(legend.position="bottom") +
  scale_y_continuous(limits = c(0,25), breaks = seq(0,25,5),
                     labels = seq(0,25,5)) + 
  
  # Label program period
  annotate(geom="text", x = 2, y = 24,
           label="Pre-intervention period", size = 4)+
  
  annotate(geom="text", x = 4.5, y = 24,
           label="Intervention period", size = 4)+
  
  annotate(geom="text", x = 4.5, y = 22,
           label="(Ineffective vaccine)", size = 3)+
  
  annotate(geom="text", x = 6.5, y = 24,
           label="Intervention period", size = 4)+
  
  annotate(geom="text", x = 6.5, y = 22,
           label="(Moderately effective vaccine)", size = 3)+
  
  geom_vline(xintercept = 3.5, linetype = "dashed")
  
dev.off()

