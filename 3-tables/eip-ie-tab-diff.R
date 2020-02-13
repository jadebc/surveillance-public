##########################################
# Shoo the Flu evaluation
# Surveillance analysis

# Analysis of Shoo the Flu indirect effects
# on flu hospitalization

# Table of difference-in-differences
##########################################
rm(list=ls())
source(here::here("0-config.R"))

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

did = res %>% filter(parameter == "Difference-in-difference")

# Subset to primary analysis
did = did %>% 
  filter(label == "Census data subset by zipcode (Primary)") %>%
  filter(subset=="fluseasCDPH_2_5") %>%
  select(-c(label, parameter))


# Calculate N_OUSD
did = did %>% mutate(N_ousd = as.numeric(N_data %>% 
                                           filter(dist=="OUSD") %>% 
                                           select(N)),
                     N_wcc = as.numeric(N_data %>% 
                                          filter(dist=="WCCUSD") %>% 
                                          select(N))) %>%
  mutate(tot_hosp = estimate *N_ousd ,
         tot_hosp_lb = lb *N_ousd ,
         tot_hosp_ub = ub *N_ousd )


# Clean estimate and CI
did = did %>% 
  mutate(ptest = pt.est.ci.f(estimate, lb, ub, decimals=0, scale=100000),
         ptest_tot = pt.est.ci.f(tot_hosp, tot_hosp_lb, tot_hosp_ub, decimals = 0, scale = 1)) %>%
  select(-c(estimate, se, lb, ub,
            tot_hosp, tot_hosp_lb, tot_hosp_ub))


# Reshape long to wide
N = did %>% select(agecat, pyears) 
N = N[!duplicated(N),]
N_row = data.frame(seas = as.character("N"), 
                   all = as.character(N$pyears[N$agecat=="all"]),
                   eld = as.character(N$pyears[N$agecat=="eld"]),
                   nonelem = as.character(N$pyears[N$agecat=="nonelem"]))

did_wide = dcast(did, seas ~ agecat, value.var = "ptest")
did_wide$seas = as.character(did_wide$seas)

pval_wide = dcast(did, seas ~ agecat , value.var = "pval")
pval_wide$seas = as.character(pval_wide$seas)
pval_wide = pval_wide %>% mutate(
  all = sprintf("%0.03f", all),
  eld = sprintf("%0.03f", eld),
  nonelem = sprintf("%0.03f", nonelem)
)

pval_wide = bind_rows(pval_wide, pval_wide)
colnames(pval_wide) = c("seas", "pval-all", "pval-eld", "pval-nonelem")

did_wide_tot = dcast(did, seas ~ agecat, value.var = "ptest_tot")
did_wide_tot$seas = as.character(did_wide_tot$seas)

table = bind_rows(N_row, did_wide, did_wide_tot)
table_out = cbind(table$seas, table$nonelem, c(NA,pval_wide$`pval-nonelem`),
                      table$eld, c(NA,pval_wide$`pval-eld`), 
                      table$all, c(NA,pval_wide$`pval-all`))


write.csv(table_out, file = paste0(tab_dir, "fluhosp_table_did.csv"), row.names = FALSE)
