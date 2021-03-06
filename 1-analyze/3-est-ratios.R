##########################################
# Shoo the Flu evaluation
# Surveillance analysis

# Analysis of Shoo the Flu indirect effects
# on flu hospitalization

# Parameter estimation
# using data subset by zip code for numerator and
# denominator
##########################################
rm(list=ls())
source(here::here("0-config.R"))

# primary analysis dataset
data_zip_race = readRDS(paste0(data_dir, "ceip-flu-data-age-sex-race-zip.RDS"))

# secondary / sensitivity analysis datasets
data_zip = readRDS(paste0(data_dir, "ceip-flu-data-age-sex-zip.RDS"))
data_zip_fpsens = readRDS(paste0(data_dir, "ceip-flu-data-fpsens-age-sex-race-zip.RDS"))
data_dist = readRDS(paste0(data_dir, "ceip-flu-data-age-sex-race-dist.RDS"))
data_sens = readRDS(paste0(data_dir, "ceip-flu-data-age-sex-race-zipsens.RDS"))
data_acs = readRDS(paste0(data_dir, "ceip-flu-data-age-acs.RDS"))

seas_list = matrix(c(1415, 1516, 1617, 1718))

#-----------------------------------------
# estimate parameters - no race

# Census data subset by zipcode
# adjust for age, sex
#-----------------------------------------
data_list = unlist(data_zip, recursive=FALSE)

res_list = list()
for(i in 1:length(data_list)) {
  res_list[[i]] = bind_rows(apply(seas_list, 1, function(x)
    fit_irr_seas(
      data = data_list[[i]],
      season = x,
      outcome = "flucases",
      treatment = "tr",
      offset = "logN",
      covariates = c("ageyrs", "sex"), 
      label = "Census data subset by zipcode - no race"
    )))
  
  res_list[[i]] = res_list[[i]] %>%
    mutate(agecat = strsplit(names(data_list)[i], "[.]")[[1]][2],
           subset = strsplit(names(data_list)[i], "[.]")[[1]][1])
}

res_zip = bind_rows(res_list) 

rm(data_list, res_list)

#-----------------------------------------
# estimate parameters

# Census data subset by zipcode
# adjust for age, sex, race
#-----------------------------------------
data_list = unlist(data_zip_race, recursive=FALSE)

res_list = list()
for(i in 1:length(data_list)) {
  res_list[[i]] = bind_rows(apply(seas_list, 1, function(x)
    fit_irr_seas(
      data = data_list[[i]],
      season = x,
      outcome = "flucases",
      treatment = "tr",
      offset = "logN",
      covariates = c("agecat", "sex", "race"), 
      label = "Census data subset by zipcode (Primary)"
    )))
  
  res_list[[i]] = res_list[[i]] %>%
    mutate(agecat = strsplit(names(data_list)[i], "[.]")[[1]][2],
           subset = strsplit(names(data_list)[i], "[.]")[[1]][1])
}

res_zip_race = bind_rows(res_list)

rm(data_list, res_list)

#-----------------------------------------
# estimate parameters

# Census data subset by zipcode and
# flu positive defined by most sensitive
# method
#-----------------------------------------
data_list = unlist(data_zip_fpsens, recursive=FALSE)

res_list = list()
for(i in 1:length(data_list)) {
  res_list[[i]] = bind_rows(apply(seas_list, 1, function(x)
    fit_irr_seas(
      data = data_list[[i]],
      season = x,
      outcome = "flucases",
      treatment = "tr",
      offset = "logN",
      covariates = c("agecat", "sex", "race"), 
      label = "Census data subset by zipcode, most sensitive flu test"
    )))
  
  res_list[[i]] = res_list[[i]] %>%
    mutate(agecat = strsplit(names(data_list)[i], "[.]")[[1]][2],
           subset = strsplit(names(data_list)[i], "[.]")[[1]][1])
}

res_zip_fpsens = bind_rows(res_list) 

rm(data_list, res_list)

#-----------------------------------------
# estimate parameters

# Census data subset by district
#-----------------------------------------
data_list = unlist(data_dist, recursive=FALSE)

res_list = list()
for(i in 1:length(data_list)) {
  res_list[[i]] = bind_rows(apply(seas_list, 1, function(x)
    fit_irr_seas(
      data = data_list[[i]],
      season = x,
      outcome = "flucases",
      treatment = "tr",
      offset = "logN",
      covariates = c("agecat", "sex", "race"), 
      label = "Census data subset by district boundaries"
    )))
  
  res_list[[i]] = res_list[[i]] %>%
    mutate(agecat = strsplit(names(data_list)[i], "[.]")[[1]][2],
           subset = strsplit(names(data_list)[i], "[.]")[[1]][1])
}

res_dist = bind_rows(res_list)

rm(data_list, res_list)

#-----------------------------------------
# estimate parameters

# Census data subset by zip code
# excluding zip codes on the school district boundaries
#-----------------------------------------
data_list = unlist(data_sens, recursive=FALSE)

res_list = list()
for(i in 1:length(data_list)) {
  res_list[[i]] = bind_rows(apply(seas_list, 1, function(x)
    fit_irr_seas(
      data = data_list[[i]],
      season = x,
      outcome = "flucases",
      treatment = "tr",
      offset = "logN",
      covariates = c("agecat", "sex", "race"), 
      label = "Census data subset by zip code, exclude boundary zips"
    )))
  
  res_list[[i]] = res_list[[i]] %>%
    mutate(agecat = strsplit(names(data_list)[i], "[.]")[[1]][2],
           subset = strsplit(names(data_list)[i], "[.]")[[1]][1])
}

res_sens = bind_rows(res_list) 

rm(data_list, res_list)


#-----------------------------------------
# estimate parameters

# ACS data subset by school district
#-----------------------------------------
data_list = unlist(data_acs, recursive=FALSE)

res_list = list()
for(i in 1:length(data_list)) {
  res_list[[i]] = bind_rows(apply(seas_list, 1, function(x)
    fit_irr_seas(
      data = data_list[[i]],
      season = x,
      outcome = "flucases",
      treatment = "tr",
      offset = "logN",
      covariates = "agecat", 
      label = "ACS data"
    )))
  
  res_list[[i]] = res_list[[i]] %>%
    mutate(agecat = strsplit(names(data_list)[i], "[.]")[[1]][2],
           subset = strsplit(names(data_list)[i], "[.]")[[1]][1])
}

res_acs = bind_rows(res_list)

rm(data_list, res_list)

#-----------------------------------------
# Bind together results
#-----------------------------------------
results = bind_rows(res_zip, 
                    res_zip_race,
                    res_zip_fpsens,
                    res_dist,
                    res_sens,
                    res_acs)

saveRDS(results, file=paste0(res_dir, "flu_surv_ratios.RDS"))
