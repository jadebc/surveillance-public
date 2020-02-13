##########################################
# Shoo the Flu evaluation
# Surveillance analysis

# Analysis of Shoo the Flu indirect effects
# on flu hospitalization

# Mean difference in length of hospitalization

# using data subset by zip code for numerator and
# denominator
##########################################
rm(list=ls())
source(here::here("0-config.R"))

#-----------------------------------------
# import data
#-----------------------------------------
# load CEIP length of stay dataset
los = readRDS(paste0(data_dir, "ceip-flu-los-clean.RDS"))

los = los %>% 
  mutate(
    tr = ifelse(dist=="OUSD", 1, 0),
    race = as.factor(race),
    sex = as.factor(sex),
    Ethnic = as.factor(Ethnic),
    prog = ifelse(flusesn>=1415, 1, 0)
    )

# drop the outlier in 2014-15
los_outlier = los
los = los %>% filter(los<100)

#-----------------------------------------
# function for mean diff in diff
#-----------------------------------------
get_mean_did = function(data, season, agecat){
 glmdata = data %>% filter(flusesn==season | prog == 0)
 fit = glm(los ~ tr + race + sex+ tr*prog, data = glmdata)
  
 did = summary(fit)$coef[which(rownames(summary(fit)$coef) == "tr:prog"),][1]
 se = summary(fit)$coef[which(rownames(summary(fit)$coef) == "tr:prog"),][2]
 lb = did - (se * qnorm(0.975))
 ub = did + (se * qnorm(0.975))
 res = data.frame(season = season, agecat = agecat, 
                  did = did, lb = lb, ub = ub)
 rownames(res)=NULL
 return(res)
}

#-----------------------------------------
# main analysis - drop outlier
#-----------------------------------------
seas_list = list(1415, 1516, 1617, 1718)
all_res_list = lapply(seas_list, function(x) get_mean_did(data = los, season = x, agecat="all"))
all_res_df = bind_rows(all_res_list)
all_res_df

nonelem = los %>% filter(nonelem==1)
nonelem_res_list = lapply(seas_list, function(x) get_mean_did(nonelem, season = x, agecat="nonelem"))
nonelem_res_df = bind_rows(nonelem_res_list)
nonelem_res_df

eld = los %>% filter(eld==1)
eld_res_list = lapply(seas_list, function(x) get_mean_did(data = eld, season = x, agecat="eld"))
eld_res_df = bind_rows(eld_res_list)
eld_res_df

all_los_res = bind_rows(all_res_df, nonelem_res_df, eld_res_df)

saveRDS(all_los_res, file=paste0(res_dir, "los_differences.RDS"))

#-----------------------------------------
# analysis - keep outlier
#-----------------------------------------
seas_list = list(1415, 1516, 1617, 1718)
all_res_list_outlier = lapply(seas_list, function(x) get_mean_did(data = los_outlier, season = x, agecat="all"))
all_res_df_outlier = bind_rows(all_res_list_outlier)
all_res_df_outlier

nonelem = los_outlier %>% filter(nonelem==1)
nonelem_res_list_outlier = lapply(seas_list, function(x) get_mean_did(nonelem, season = x, agecat="nonelem"))
nonelem_res_df_outlier = bind_rows(nonelem_res_list_outlier)
nonelem_res_df_outlier

eld = los_outlier %>% filter(eld==1)
eld_res_list_outlier = lapply(seas_list, function(x) get_mean_did(data = eld, season = x, agecat="eld"))
eld_res_df_outlier = bind_rows(eld_res_list_outlier)
eld_res_df_outlier

all_los_res_outlier = bind_rows(all_res_df_outlier, nonelem_res_df_outlier, eld_res_df_outlier)

saveRDS(all_los_res_outlier, file=paste0(res_dir, "los_differences_outlier.RDS"))

