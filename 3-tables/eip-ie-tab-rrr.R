##########################################
# Shoo the Flu evaluation
# Surveillance analysis

# Analysis of Shoo the Flu indirect effects
# on flu hospitalization

# Table of ratio of ratios
##########################################
rm(list=ls())
source(here::here("0-config.R"))

res = readRDS(paste0(res_dir, "flu_surv_ratios.RDS"))

rir = res %>% filter(parameter == "Ratio of incidence ratios")

# Subset to primary analysis
rir = rir %>% 
  filter(label == "Census data subset by zipcode (Primary)") %>%
  filter(subset=="fluseasCDPH_2_5") %>%
  select(-c(label, parameter))

# Clean estimate and CI
rir = rir %>% 
  mutate(ptest = pt.est.ci.f(estimate, lb, ub, decimals=2, scale=1)) %>%
  select(-c(estimate, se, lb, ub))

# Reshape long to wide
N = rir %>% select(agecat, pyears) 
N = N[!duplicated(N),]
N_row = data.frame(seas = as.character("N"), 
                   all = as.character(N$pyears[N$agecat=="all"]),
                   eld = as.character(N$pyears[N$agecat=="eld"]),
                   nonelem = as.character(N$pyears[N$agecat=="nonelem"]))

rir_wide = dcast(rir, seas ~ agecat )
rir_wide$seas = as.character(rir_wide$seas)

table = bind_rows(N_row, rir_wide)

# Reorder columns
table = table[,c("seas", "nonelem", "eld", "all")]

write.csv(table, file = paste0(tab_dir, "fluhosp_table_rir.csv"), row.names = FALSE)
