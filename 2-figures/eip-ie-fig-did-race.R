##########################################
# Shoo the Flu evaluation
# Analysis of lab-confirmed flu data

# Plot difference-in-differences
# Primary analysis stratified by race
##########################################
rm(list=ls())
source(here::here("0-config.R"))

res = readRDS(paste0(res_dir, "flu_surv_differences_race.RDS"))

res_data = res %>%
  filter(parameter =="Difference-in-difference" &
           race !="American Indian/Alaska Native") %>%
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
      agecat == "all" ~ "Total effect",
      agecat == "eld" ~ "Herd effect",
      agecat == "nonelem" ~ "Herd effect"
    ), levels = c("Herd effect", "Total effect")
    )
  )  %>%
  # rescale
  mutate(
    estimate = estimate * 100000,
    lb = lb * 100000,
    ub = ub * 100000
  ) %>%
  rename(Race = race) %>%
  mutate(Race = case_when(
    Race == "Black" ~ "African American",
    Race == "API" ~ "Asian / Pacific Islander",
    TRUE ~ Race
  )) %>%
  mutate(Race = factor(Race, levels = c("African American", "Asian / Pacific Islander",
                                        "Hispanic","White","Other","Two or more races")))

# drop other in 2017-18 since so few
drops = which(res_data$Race=="Other" & res_data$Season=="2017-18")
res_data = res_data[-drops,]

red = "#D80E30"
blue = "#0E6AD8"
green = "#47BA67"
cols = c(red, blue, "black")
shapes = c(17, 16)

pdf(file=paste0(fig_dir, "fluhosp_did_race.pdf"), width=12, height=4)
ggplot(res_data, aes(x = Season, y = estimate)) + 
  geom_point(aes(col = Race, shape = effect), position = position_dodge(width=0.5), size = 2) +
  geom_linerange(aes(ymin = lb, ymax = ub, col = Race, shape = effect), 
                position = position_dodge(width=0.5)) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("Difference-in-difference in\ncumulative incidence per 100,000") +
  scale_shape_manual("Type of effect", values = shapes) +
  facet_wrap(~Age, scales = "free") + 
  theme_complete_bw()+
  theme(legend.position = "bottom")
dev.off()



