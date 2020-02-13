##########################################
# Shoo the Flu evaluation
# Surveillance analysis

# Analysis of Shoo the Flu indirect effects
# on flu hospitalization

# Table of relative scale difference-in-differences
##########################################
rm(list=ls())
source(here::here("0-config.R"))

res = readRDS(paste0(res_dir, "flu_surv_differences.RDS"))

data = res %>%
  filter(label=="Census data subset by zipcode (Primary)" &
           covariates=="agecat, sex, race" &
           parameter =="Relative difference-in-difference" & 
           subset == "fluseasCDPH_2_5")  %>%
  select(-c(label, parameter, subset))

# Clean estimate and CI
data = data %>% 
  mutate(ptest = pt.est.ci.f(estimate, lb, ub, decimals=0, scale=1)) %>%
  select(-c(estimate, se, lb, ub))

# Reshape long to wide
N = data %>% select(agecat, pyears) 
N = N[!duplicated(N),]
N_row = data.frame(seas = as.character("N"), 
                   all = as.character(N$pyears[N$agecat=="all"]),
                   eld = as.character(N$pyears[N$agecat=="eld"]),
                   nonelem = as.character(N$pyears[N$agecat=="nonelem"]))

data_wide = dcast(data, seas ~ agecat, value.var = "ptest")
data_wide$seas = as.character(data_wide$seas)

pval_wide = dcast(data, seas ~ agecat, value.var = "pval")
pval_wide$seas = as.character(pval_wide$seas)

table = bind_rows(N_row, data_wide)

# Reorder columns
table = table[,c("seas", "nonelem", "eld", "all")]
table_out = cbind(table$seas, 
                  table$nonelem, c(NA, pval_wide$nonelem),
                  table$eld, c(NA, pval_wide$eld),
                  table$all, c(NA, pval_wide$all))

write.csv(table_out, file = paste0(tab_dir, "fluhosp_table_rdid.csv"), row.names = FALSE)
