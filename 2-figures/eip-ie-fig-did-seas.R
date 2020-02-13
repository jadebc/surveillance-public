##########################################
# Shoo the Flu evaluation
# Analysis of lab-confirmed flu data

# Plot difference-in-differences
# Alternative flu season subsets
##########################################
rm(list=ls())
source(here::here("0-config.R"))

res = readRDS(paste0(res_dir, "flu_surv_differences.RDS"))

res_data = res %>%
  filter(label=="Census data subset by zipcode (Primary)" &
           covariates=="agecat, sex, race" &
           parameter =="Difference-in-difference" & 
           subset!="peakwk_2_5" &
           subset!="ceip_peakwk") %>%
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
  ),
  subset = factor(
    case_when(
      subset == "All" ~ "CEIP flu season definition\n(Sensitivity analysis)",
      subset == "fluseasCDPH_2_5" ~ "Flu season definition based on CDPH ILI\n(Primary analysis)",
      subset == "peakwk_2_5" ~ "Peak week based on CDPH ILI"
    ), levels = c("Flu season definition based on CDPH ILI\n(Primary analysis)",
                  "CEIP flu season definition\n(Sensitivity analysis)", 
                  "Peak week based on CDPH ILI")
  )
  )  %>%
  # rescale
  mutate(
    estimate = estimate * 100000,
    lb = lb * 100000,
    ub = ub * 100000
  )

orange =  "#E69F00"
blue = "#0072B2"
green = "#009E73"
cols = c(orange, blue, green)

shapes = c(16, 17)

pdf(file=paste0(fig_dir, "fluhosp_did_seas.pdf"), width=10, height=3)
ggplot(res_data, aes(x = Season, y = estimate)) + 
  geom_point(aes(col = Age, shape = effect), position = position_dodge(width=0.5), size = 2) +
  geom_linerange(aes(ymin = lb, ymax = ub, col = Age, shape = effect), 
                 position = position_dodge(width=0.5), width=0.2) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  facet_wrap(~subset) + 
  
  scale_y_continuous(limits = c(-300,55), breaks = seq(-300, 50, 50), 
                     labels = seq(-300, 50, 50)) + 
  ylab("Difference-in-difference in\ncumulative incidence per 100,000") +
  scale_color_manual(values = cols) +
  scale_shape_manual("Type of effect", values = shapes) + 
  theme_complete_bw()
dev.off()



