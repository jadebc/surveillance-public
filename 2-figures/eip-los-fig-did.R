##########################################
# Shoo the Flu evaluation
# Analysis of lab-confirmed flu data

# Plot difference-in-differences
# Length of stay
##########################################
rm(list=ls())
source(here::here("0-config.R"))

los = readRDS(paste0(res_dir, "los_differences.RDS"))
los_outlier = readRDS(paste0(res_dir, "los_differences_outlier.RDS"))

process_data = function(data){
  data %>%
  mutate(Season = factor(
    case_when(
      season == 1415 ~ "2014-15",
      season == 1516 ~ "2015-16",
      season == 1617 ~ "2016-17",
      season == 1718 ~ "2017-18"
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
  )
}


res_data = process_data(los)
res_outlier_data = process_data(los_outlier)

orange =  "#E69F00"
blue = "#0072B2"
green = "#009E73"
cols = c(orange, blue, green)

shapes = c(17, 16)

pdf(file=paste0(fig_dir, "fluhosp_los_did.pdf"), width=7, height=3)
ggplot(res_data, aes(x = Season, y = did)) + 
  geom_point(aes(col = Age, shape = effect), position = position_dodge(width=0.5), size = 2) +
  geom_linerange(aes(ymin = lb, ymax = ub, col = Age, shape = effect), 
                 position = position_dodge(width=0.5), width=0.2) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(limits = c(-6,1), breaks = seq(-6, 1, 1),
  labels = seq(-6, 1, 1)) +
  ylab("Difference-in-difference in\ndays of hospitalization") +
  scale_color_manual(values = cols) +
  scale_shape_manual("Type of effect", values = shapes) + 
  theme_complete_bw()
dev.off()


pdf(file=paste0(fig_dir, "fluhosp_los_outlier_did.pdf"), width=7, height=3)
ggplot(res_outlier_data, aes(x = Season, y = did)) + 
  geom_point(aes(col = Age, shape = effect), position = position_dodge(width=0.5), size = 2) +
  geom_linerange(aes(ymin = lb, ymax = ub, col = Age, shape = effect), 
                 position = position_dodge(width=0.5), width=0.2) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(limits = c(-8,1), breaks = seq(-8, 1, 1),
  labels = seq(-8, 1, 1)) +
  ylab("Difference-in-difference in\ndays of hospitalization") +
  scale_color_manual(values = cols) +
  scale_shape_manual("Type of effect", values = shapes) + 
  theme_complete_bw()
dev.off()

