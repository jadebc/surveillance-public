##########################################
# Shoo the Flu evaluation
# Surveillance analysis

# Analysis of Shoo the Flu indirect effects
# on flu hospitalization

# Additive DID estimation

# using data subset by zip code for numerator and
# denominator

# stratify by race
##########################################
rm(list=ls())
source(here::here("0-config.R"))

#-----------------------------------------
# import data
#-----------------------------------------
data_zip_race = readRDS(paste0(data_dir, "ceip-flu-data-age-sex-race-complete-zip.RDS"))

preseas_list = matrix(c(1112, 1213, 1314))
seas_list = matrix(c(1415, 1516, 1617, 1718))
race_list = as.list(names(table(data_zip_race$fluseasCDPH_2_5$all$race)))

#-----------------------------------------
# wrapper function that stratifies by race
# and obtains DID estimates
#-----------------------------------------
stratify_did_by_race = function(data, race_stratum){
  
  data_sub = data %>% filter(race == race_stratum)
  
  res = bind_rows(apply(seas_list, 1, function(x)
    fit_did_glm_seas(
      data = data_sub,
      season = x,
      preseas = preseas_list,
      outcome = "flucases",
      treatment = "tr",
      offset = "logN",
      covariates = c("agecat", "sex"),
      label = "Census data subset by zipcode"
    )))
  
  return(res)
}

#-----------------------------------------
# estimate parameters  - all ages
#-----------------------------------------
all_list = list()
for(j in 1:length(race_list)){
  all_list[[j]] = stratify_did_by_race(data = data_zip_race$fluseasCDPH_2_5$all, race_stratum = race_list[j])
  all_list[[j]] = all_list[[j]] %>% mutate(race = race_list[j])
}

all_df = bind_rows(all_list)

all_df = all_df %>% mutate(race = unlist(race))

#-----------------------------------------
# estimate parameters  - eld
#-----------------------------------------
eld_list = list()
for(j in 1:length(race_list)){
  eld_list[[j]] = stratify_did_by_race(data = data_zip_race$fluseasCDPH_2_5$eld, race_stratum = race_list[j])
  eld_list[[j]] = eld_list[[j]] %>% mutate(race = race_list[j])
}

eld_df = bind_rows(eld_list)

eld_df = eld_df %>% mutate(race = unlist(race))

#-----------------------------------------
# estimate parameters  - nonelem
#-----------------------------------------
nonelem_list = list()
for(j in 1:length(race_list)){
  nonelem_list[[j]] = stratify_did_by_race(data = data_zip_race$fluseasCDPH_2_5$nonelem, race_stratum = race_list[j])
  nonelem_list[[j]] = nonelem_list[[j]] %>% mutate(race = race_list[j])
}

nonelem_df = bind_rows(nonelem_list)

nonelem_df = nonelem_df %>% mutate(race = unlist(race))


#-----------------------------------------
# combine and save results
#-----------------------------------------
all_df = all_df %>% mutate(agecat = "all")
eld_df = eld_df %>% mutate(agecat = "eld")
nonelem_df = nonelem_df %>% mutate(agecat = "nonelem")

results = bind_rows(all_df, eld_df, nonelem_df)

saveRDS(results, file=paste0(res_dir, "flu_surv_differences_race.RDS"))

