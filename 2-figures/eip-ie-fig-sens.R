##########################################
# Shoo the Flu evaluation
# Surveillance analysis

# Analysis of Shoo the Flu indirect effects
# on flu hospitalization

# Figure comparing sensitivity analyses
##########################################
rm(list=ls())
source(here::here("0-config.R"))

#-----------------------------------------
# Plot difference in differences 
#-----------------------------------------
res = readRDS(paste0(res_dir, "flu_surv_differences.RDS"))

res = res %>%
  filter(subset == "All") %>%
  mutate(Season = factor(
    case_when(
      seas == 1415 ~ "2014-15",
      seas == 1516 ~ "2015-16",
      seas == 1617 ~ "2016-17",
      seas == 1718 ~ "2017-18"
    )
  ),
  age = factor(
    case_when(
      agecat == "all" ~ "All ages",
      agecat == "eld" ~ "Elderly",
      agecat == "nonelem" ~ "Non-elementary aged"
    )
  )) %>%
  mutate(age = factor(age, levels = c("Non-elementary aged",
                                      "Elderly", "All ages"))) %>%
  rename(Analysis = label) %>%
  # scale by 100,000 
  mutate(
    estimate = estimate * 100000,
    lb = lb * 100000,
    ub = ub * 100000
  )

res_did = res %>% filter(parameter=="Difference-in-difference")
res_diff = res %>% filter(parameter=="Mean difference")

pdf(file=paste0(fig_dir,"flu_hosp_did_sens.pdf"), width=11, height=4)
ggplot(res_did, aes(x = Season, y = estimate)) + 
  geom_point(aes(col = Analysis), position = position_dodge(width=0.5)) +
  geom_linerange(aes(ymin = lb, ymax = ub, col = Analysis), 
                position = position_dodge(width=0.5)) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ age, scales = "free") +
  theme_complete_bw() +
  theme(legend.position = "bottom")  +
  guides(col = guide_legend(nrow=4)) +
  ylab("Difference-in-differences\nper 100,000(95% CI)")
dev.off()

pdf(file=paste0(fig_dir,"flu_hosp_diff_sens.pdf"), width=11, height=4)
ggplot(res_diff, aes(x = Season, y = estimate)) + 
  geom_point(aes(col = Analysis), position = position_dodge(width=0.5)) +
  geom_linerange(aes(ymin = lb, ymax = ub, col = Analysis), 
                 position = position_dodge(width=0.5)) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ age, scales = "free") +
  theme_complete_bw() +
  theme(legend.position = "bottom")  +
  guides(col = guide_legend(nrow=4))+
  ylab("Mean differences\n(95% CI)")
dev.off()



#-----------------------------------------
# Plot ratios
#-----------------------------------------
irr = readRDS(paste0(res_dir, "flu_surv_ratios.RDS"))

irr = irr %>%
  mutate(Season = factor(
    case_when(
      seas == 1415 ~ "2014-15",
      seas == 1516 ~ "2015-16",
      seas == 1617 ~ "2016-17",
      seas == 1718 ~ "2017-18"
    )
  ),
  age = factor(
    case_when(
      agecat == "all" ~ "All ages",
      agecat == "eld" ~ "Elderly",
      agecat == "nonelem" ~ "Non-elementary aged"
    )
  )) %>%
  mutate(age = factor(age, levels = c("Non-elementary aged",
                                      "Elderly", "All ages"))) %>%
  rename(Analysis = label)

res_rir = res %>% filter(parameter=="Difference-in-difference")
res_ir = res %>% filter(parameter=="Mean difference")

pdf(file=paste0(fig_dir,"flu_hosp_irr_sens.pdf"), width=11, height=4)
ggplot(res_rir, aes(x = Season, y = estimate)) + 
  geom_point(aes(col = Analysis), position = position_dodge(width=0.5)) +
  geom_linerange(aes(ymin = lb, ymax = ub, col = Analysis), 
                 position = position_dodge(width=0.5)) + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  facet_wrap(~ age, scales = "free") +
  theme_complete_bw() +
  theme(legend.position = "bottom")  +
  guides(col = guide_legend(nrow=4))+
  ylab("Ratio of cumulative incidence\nratios (95% CI)")
dev.off()


pdf(file=paste0(fig_dir,"flu_hosp_ir_sens.pdf"), width=11, height=4)
ggplot(res_ir, aes(x = Season, y = estimate)) + 
  geom_point(aes(col = Analysis), position = position_dodge(width=0.5)) +
  geom_linerange(aes(ymin = lb, ymax = ub, col = Analysis), 
                 position = position_dodge(width=0.5)) + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  facet_wrap(~ age, scales = "free") +
  theme_complete_bw() +
  theme(legend.position = "bottom")  +
  guides(col = guide_legend(nrow=4))+
  ylab("Cumulative incidence ratio \n(95% CI)")
dev.off()
