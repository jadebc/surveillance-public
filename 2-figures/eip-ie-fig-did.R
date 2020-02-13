##########################################
# Shoo the Flu evaluation
# Analysis of lab-confirmed flu data

# Plot difference-in-differences
# Primary analysis
##########################################
rm(list=ls())
source(here::here("0-config.R"))

# load DID results
res = readRDS(paste0(res_dir, "flu_surv_differences.RDS"))

# load data used to estimate DID to get Ns
data_zip_race = readRDS(paste0(data_dir, "ceip-flu-data-age-sex-race-zip.RDS"))

N_data = data_zip_race$All$all %>%
  group_by(dist, seas) %>% 
  summarise(N = sum(N)) %>%
  group_by(dist) %>%
  summarise(N = mean(N))

N_ousd = as.numeric(N_data %>% 
                      filter(dist=="OUSD") %>% 
                      select(N))

primary_data = res %>%
  filter(label=="Census data subset by zipcode (Primary)" &
           covariates=="agecat, sex, race" &
           parameter =="Difference-in-difference" & 
           subset == "fluseasCDPH_2_5")  %>%
  mutate(N_ousd = as.numeric(N_data %>% 
                               filter(dist=="OUSD") %>% 
                               select(N)),
         N_wcc = as.numeric(N_data %>% 
                              filter(dist=="WCC") %>% 
                              select(N))) 

peak_data = res %>%
  filter(label=="Census data subset by zipcode (Primary)" &
           covariates=="agecat, sex, race" &
           parameter =="Difference-in-difference" & 
           subset == "ceip_peakwk") 

process_data = function(data){
  data = data %>%
    mutate(Season = factor(
      case_when(
        seas == 1415 ~ "2014-15",
        seas == 1516 ~ "2015-16",
        seas == 1617 ~ "2016-17",
        seas == 1718 ~ "2017-18"
      )
    ),
    Age = factor(
      case_when(
        agecat == "all" ~ "All ages",
        agecat == "eld" ~ "Elderly",
        agecat == "nonelem" ~ "Non-elementary aged"
      ), levels = c("Non-elementary aged", "Elderly","All ages")
    ),
    effect = factor(
      case_when(
        agecat == "all" ~ "Overall effect",
        agecat == "eld" ~ "Indirect effect",
        agecat == "nonelem" ~ "Indirect effect"
      ), levels = c("Indirect effect", "Overall effect")
    )
    )  %>%
    # rescale
    mutate(
      estimate = estimate * 100000,
      lb = lb * 100000,
      ub = ub * 100000
    )
  return(data)
} 

primary_data = process_data(primary_data)
peak_data = process_data(peak_data)

orange =  "#E69F00"
blue = "#0072B2"
green = "#009E73"
cols = c(orange, blue, green)

shapes = c(17, 16)


pdf(file=paste0(fig_dir, "fluhosp_did.pdf"), width=7, height=3)
ggplot(primary_data, aes(x = Season, y = estimate)) + 
  geom_point(aes(col = Age, shape = effect), position = position_dodge(width=0.5), size = 2) +
  geom_linerange(aes(ymin = lb, ymax = ub, col = Age, shape = effect), 
                position = position_dodge(width=0.5), width=0.2) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(limits = c(-300,55), breaks = seq(-300, 50, 50),
      labels = seq(-300, 50, 50),
      sec.axis = sec_axis(~.*N_ousd / 100000, 
                          name = "Difference-in-difference in\ntotal hospitalization",
                          labels = seq(from=-1400, to=240, by=200),
                          breaks = seq(from=-1400, to=240, by=200))) +

  ylab("Difference-in-difference in\ncumulative incidence per 100,000") +
  scale_color_manual(values = cols) +
  scale_shape_manual("Type of effect", values = shapes) + 
  theme_complete_bw()
dev.off()


pdf(file=paste0(fig_dir, "fluhosp_did_peakwk.pdf"), width=7, height=3)
ggplot(peak_data, aes(x = Season, y = estimate)) + 
  geom_point(aes(col = Age, shape = effect), position = position_dodge(width=0.5), size = 2) +
  geom_linerange(aes(ymin = lb, ymax = ub, col = Age, shape = effect), 
                 position = position_dodge(width=0.5), width=0.2) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(limits = c(-105,40), breaks = seq(-100, 40, 20),
                     labels = seq(-100, 40, 20)) +
  ylab("Difference-in-difference in\ncumulative incidence per 100,000") +
  scale_color_manual(values = cols) +
  scale_shape_manual("Type of effect", values = shapes) + 
  theme_complete_bw()
dev.off()


