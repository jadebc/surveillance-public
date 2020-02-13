##############################################
# Documentation: merge_flucount_pop
# Usage: merge_flucount_pop(flucount, pop, pop_type, season_list=NULL)
# Description: aggregate flu case counts using prep_flu, aggregate 
#              population counts using prep_census or prep_acs, then merge 
#              aggregated datasets to prepare for analysis

# Args/Options: 
# flucount:                data with flu case counts
#                          must include a variable called seas, 
#                          indicating the program year / flu season, 
#                          dist variable indicating OUSD vs. WCCUSD, 
#                          and any covariates used for grouping
# pop:                     census/ACS data; must include a variable called seas, 
#                          indicating the program year / flu season, 
#                          dist variable indicating OUSD vs. WCCUSD,
#                          and any additional grouping covariates
# pop_type:                either "census" indicating census data is to be used or
#                          "acs" indicating that acs data is to be used
# covariates:              vector of strings containing covariates to be included in dataset
# season_list:             vector of seasons to be included (NULL if using
#                          ACS data)
# Returns: a list of data frames including merged flu counts and population 
# for all ages, elderly, and non-elementary age groups

merge_flucount_pop = function(flucount, pop, pop_type, covariates, season_list=NULL, all_race_cats=FALSE, yname = "flupos"){
  
  #--------------------------------------------
  # check arguments for internal consistency
  #--------------------------------------------
  # assert that correct age variable is included in 
  # covariate list depending on whether race is included
  # (census race data is stratified by age category;
  # if no race, then age in whole years is available)
  if("race" %in% covariates & pop_type == "census")  assert_that("agecat" %in% covariates, 
                                                                 msg = "If race is included in census dataset, agecat variable must be included as well.")
  
  if(!"race" %in% covariates & pop_type == "census")  assert_that("ageyrs" %in% covariates, 
                                                                  msg = "If race is not included in census dataset, ageyrs variable must be included as well.")
  
  if(pop_type == "acs")  assert_that("agecat" == covariates, 
                                     msg = "If ACS dataset is used, agecat is the only possible covariate.")
  
  # assert that data contains the covariates that were specified
  if(!all_race_cats) assert_that(all(covariates %in% colnames(pop)), msg = "Some covariates not included in population data.")
  assert_that(all(covariates %in% colnames(flucount)), msg = "Some covariates not included in flu data.")

  #--------------------------------------------
  # summarise flu count data
  #--------------------------------------------
  flu_agg = prep_flu(flucount, 
                     pop = pop_type, 
                     yname = yname,
                     covariates = covariates, 
                     all_race_cats = all_race_cats)
  
  #--------------------------------------------
  # summarise population data
  #--------------------------------------------
  if(pop_type=="census" & !all_race_cats) pop_agg = prep_census(pop, season_list = season_list, covariates = covariates)
  if(pop_type=="census" & all_race_cats) pop_agg = prep_census(pop, season_list = season_list, covariates = c("agecat", "sex", "race_complete")) %>% rename(race = race_complete)
  if(pop_type=="acs") pop_agg = prep_acs(pop)
  
  pop_agg = pop_agg %>% mutate(dist = as.factor(dist))
  
  if("agecat" %in% covariates & pop_type == "census"){
    
    agecat_levels = c("Under 5", "5 to 9", "10 to 14", "15 to 17", "18 and 19", "20", "21",    
                      "22 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49",
                      "50 to 54", "55 to 59", "60 and 61", "62 to 64", "65 and 66", "67 to 69",
                      "70 to 74", "75 to 79", "80 to 84", "85 and over", "(Missing)" )
    
    flu_agg = flu_agg %>% mutate(agecat = factor(agecat, levels = agecat_levels))
    pop_agg = pop_agg %>% mutate(agecat = factor(agecat, levels = agecat_levels))
  } 
  
  if("ageyrs" %in% covariates & pop_type == "census" ){
     if(length(levels(flu_agg$ageyrs))!=length(levels(pop_agg$ageyrs))){
       flu_agg = flu_agg %>% mutate(ageyrs = as.character(ageyrs))
       pop_agg = pop_agg %>% mutate(ageyrs = as.character(ageyrs)) 
     }
  } 
  
  #--------------------------------------------
  # merge numerator and denominator
  #--------------------------------------------
  if(pop_type=="census"){
    flu.merge=full_join(pop_agg, flu_agg,
                        by = c("seas", "dist", covariates))
  } #DIFFERENT RACE CATEGORIES IF USING COMPELTE RACE VARIABLE 
  if(pop_type=="acs"){
    assert_that("agecat" %in% covariates)
    flu.merge=full_join(pop_agg, flu_agg,by=c("seas","dist","agecat"))
  }
  
  #--------------------------------------------
  # drop if missing N
  #--------------------------------------------
  print(paste0(nrow(flu.merge[is.na(flu.merge$N),]), " out of ",
               nrow(flu.merge)," rows dropped because N was missing for that stratum"))
  
  flu.merge = flu.merge %>% filter(!is.na(N))
  
  #--------------------------------------------
  # create log pop offset and treatment indicator
  #--------------------------------------------
  flu.merge = flu.merge %>% 
    mutate(logN = log(N),
           tr = ifelse(dist=="OUSD", 1, 0))
  
  #--------------------------------------------
  # subset to age specific groupings
  #--------------------------------------------
  group_vars_sym <- syms(c("seas", "dist", covariates))
  all = flu.merge %>% mutate(flucases = ifelse(is.na(flucases), 0, flucases)) 
  
  #--------------------------------------------
  # check for missing values
  #--------------------------------------------
  if( any(!complete.cases(all))) warning('Warning: missing values are present in data')
  
  #--------------------------------------------
  # organize ageyrs level and sort data
  #--------------------------------------------
  if("ageyrs" %in% covariates){
    ageyrs_levels = c("Under 1 year",
                      "1 year",
                      paste0(seq(2, 99, 1), " years"),
                      "100 to 104 years", "105 to 109 years",
                      "110 years and over")
    
    all = all %>% mutate(ageyrs = factor(ageyrs, levels = ageyrs_levels))
  }
  
  
  group_vars_sym <- syms(c("seas", "dist", covariates))
  all = all %>% arrange(!!!(group_vars_sym))
  
  #--------------------------------------------
  # subset to specific ages
  #--------------------------------------------
  all = all %>% 
    mutate(season = as.factor(case_when(
      seas == 0809 ~ "2008-09",
      seas == 0910 ~ "2009-10",
      seas == 1011 ~ "2010-11",
      seas == 1112 ~ "2011-12",
      seas == 1213 ~ "2012-13",
      seas == 1314 ~ "2013-14",
      seas == 1415 ~ "2014-15",
      seas == 1516 ~ "2015-16",
      seas == 1617 ~ "2016-17",
      seas == 1718 ~ "2017-18"
    )))
  
  if(pop_type=="census" & "agecat" %in% covariates){
    eld = all %>% 
      filter(agecat=="65 and 66" | 
               agecat=="67 to 69"  |
               agecat=="70 to 74"  |
               agecat=="75 to 79"  |
               agecat=="80 to 84"  |
               agecat=="85 and over") 
    
    nonelem = all %>% 
      filter(agecat!="5 to 9" & agecat!="10 to 14") 
  }
  
  if(pop_type=="census" & "ageyrs" %in% covariates){
    
    eld_ageyrs = c(paste0(seq(65, 99, 1), " years"),
                   "100 to 104 years", "105 to 109 years")
    eld = all %>% filter(ageyrs %in% eld_ageyrs)
    
    nonelem_ageyrs = c("Under 1 year", 
                       paste0(c(seq(1, 4, 1), seq(15, 64, 1)), " years"))
    nonelem = all %>% filter(ageyrs %in% nonelem_ageyrs) 
  }
  
  if(pop_type=="acs"){
    eld = all %>% 
      filter(agecat=="65-74 years" | 
               agecat=="75-84 years" |
               agecat=="85+ years") 
    
    nonelem = all %>% 
      filter(agecat!="5-9 years" & agecat!="10-14 years") 
  }
  
  #--------------------------------------------
  # return data list
  #--------------------------------------------
  merged = list(all = all, 
                eld = eld,
                nonelem = nonelem)  
  
  return(merged)
}  


##############################################
# Documentation: fit_irr_seas
# Usage: fit_irr_seas(data, season, outcome, treatment, offset, covariates, label)
# Description:  Fits log-linear poisson models to estimate adjusted incidence ratios
#               and ratios of incidence ratios
# Args/Options: 
# data:                    the dataset to include in the regression model 
# season:                  number indicating program year / flu seas, as numeric
# outcome:                 the outcome column name, as a string
# treatment:               the treatment column name, as a string   
# offset:                  log population offset column name, as a string
# covariates:              adjustment covariates, as a character vector
# label:                   description of dataset, as a string
# 
# Returns: a formatted data frame that includes: adjusted incidence ratio
# and ratio of incidence ratio estimates, the label, person-years, 
# the lower and upper bounds of a 95% CI, and the season

fit_irr_seas = function(data, season, outcome, treatment, offset, covariates, label){
  pre_seas_list <- c(1112, 1213, 1314)
  analysis_seas_list <-  c(pre_seas_list, season)
  
  fitdata_ir <- data[data$seas == season,]
  
  fitdata_rir <- data %>% filter(seas %in% analysis_seas_list) 
  
  fitdata_rir$time = ifelse(fitdata_rir$seas == season, 1, 0)
  
  cov_formula = paste(covariates, collapse = " + ")
  LHS = paste(outcome, "~")
  RHS_ir = paste0(treatment, " + ", cov_formula)
  RHS_rir = paste0(treatment, "*", "time")
  
  formula_ir <- as.formula(paste(LHS, RHS_ir))
  formula_rir <- as.formula(paste(LHS, RHS_rir))
  
  glm_fit_ir=glm(formula_ir, offset=fitdata_ir[[offset]], data=fitdata_ir,
                 family=poisson(link=log))
  glm_fit_rir=glm(formula_rir, offset=fitdata_rir[[offset]], data=fitdata_rir,
                  family=poisson(link=log))
  
  res_rir = format.glm(glm_fit_rir,coef=":", parameter = "ratio")
  res_ir = format.glm(glm_fit_ir,coef=treatment, parameter = "ratio")
  
  res_rir = res_rir %>% mutate(parameter = "Ratio of incidence ratios")
  res_ir = res_ir %>% mutate(parameter = "Incidence ratio")
  
  N_ir = fitdata_ir %>% summarise(N = sum(N))
  N_rir = fitdata_rir %>% summarise(N = sum(N))
  
  res_ir = res_ir %>% mutate(pyears = N_ir$N) 
  
  res_rir = res_rir %>% mutate(pyears = N_rir$N) 
  
  res = bind_rows(res_ir, res_rir) %>%
    mutate(label = label,
           covariates = paste(covariates, collapse=", "),
           seas = season) %>%
    select(label, covariates, seas, pyears, parameter, everything()) %>%
    arrange(label, covariates, seas, parameter, seas)
  
  return(res)
  
}


##############################################

# Documentation: get_did
# Usage: get_did(fit)
# Description: Fits a Poisson log-linear model and obtains the DID and 95% CI
#              for a program year subtracting rates in pre-program years 
# Args/Options: 
# fit: glm fit from a log-linear model with coefficients for treatment, pre/post
#      intervention, and an interaction term between the two and no other variables
# 
# Returns: a formatted data frame that includes: difference-in-difference estimate, 
# standard error, the lower and upper bounds of a 95% CI, and the season


get_did = function(fit){
  
  # Gradient 
  b0 = fit$coefficients[1]
  b1 = fit$coefficients[2]
  b2 = fit$coefficients[3]
  b3 = fit$coefficients[4]
  
  dfdb0 = (exp(b0 + b1 + b2 + b3) - exp(b0 + b1)) - 
    (exp(b0 + b2) - (exp(b0)))
  dfdb1 = exp(b0 + b1 + b2 + b3) - exp(b0 + b1)
  dfdb2 = exp(b0 + b1 + b2 + b3) - exp(b0 + b2) 
  dfdb3 = exp(b0 + b1 + b2 + b3) 
  
  grad <- c(dfdb0, dfdb1, dfdb2, dfdb3)
  names(grad) <- c("dfdb0", "dfdb1", "dfdb2", "dfdb3")
  grad
  
  # Estimate DID
  did = (exp(b0+b1+b2+b3) - exp(b0+b1)) - (exp(b0+b2) - exp(b0))
  names(did) = NULL
  
  # Variance-covariance matrix
  vb <- vcov(fit)
  
  # Variance of DID
  vF <- t(grad) %*% vb %*% grad
  
  se <- sqrt(vF)
  
  # 95% CI 
  lb = did - (qnorm(0.975)*se)
  ub = did + (qnorm(0.975)*se)

  # 2-sided p-value 
  t_stat = (did - 1) / se
  pval = 2 * pt(-abs(t_stat), df = nrow(fit$data) - 1)
    
  out = data.frame(estimate = did, se = se , lb = lb, ub = ub, pval = pval)
  
  return(out)
  
}


##############################################

# Documentation: get_rdid
# Usage: get_rdid(fit)
# Description: Fits a Poisson log-linear model and obtains the relative scale DID and 95% CI
#              for a program year subtracting rates in pre-program years 
# Args/Options: 
# fit: glm fit from a log-linear model with coefficients for treatment, pre/post
#      intervention, and an interaction term between the two and no other variables
# 
# Returns: a formatted data frame that includes: relative difference-in-difference estimate, 
# standard error, the lower and upper bounds of a 95% CI, and the season

# Relative scale DID is defined as  1 - (E[Y|X=1, T=1]-E[Y|X=1, T=0]) / (E[Y|X=0, T=1] - E[Y|X=0, T=0])


get_rdid = function(fit, parameter){
  
  # Gradient 
  b0 = fit$coefficients[1]
  b1 = fit$coefficients[2]
  b2 = fit$coefficients[3]
  b3 = fit$coefficients[4]
  
  dfdb0 = 0
  dfdb1 = -(exp(b1) * (exp(b2 + b3) - 1)) / ((exp(b2) -1)^2)
  dfdb2 = ((exp(b3)-1)*exp(b1+b2))/((exp(b2)-1)^2)
  dfdb3 = (exp(b1+b2+b3))/(1-exp(b2))
  
  grad <- c(dfdb0, dfdb1, dfdb2, dfdb3)
  names(grad) <- c("dfdb0", "dfdb1", "dfdb2", "dfdb3")
  grad
  
  # Estimate relative scale DID
  rr_numerator = exp(b0+b1+b2+b3) - exp(b0+b1)
  rr_denominator = exp(b0+b2) - exp(b0)
  
  # If the numerator and denominator are very close to 0, 
  # impute a value of 1
  if(rr_numerator > 0.00001 & rr_denominator > 0.00001){
    rr = rr_numerator / rr_denominator
  }else{
    rr = 1
  }
  
  rdid = (1 - rr) * 100 
  
  # Variance-covariance matrix
  vb <- vcov(fit)
  vb
  
  # Variance of DID
  vF <- t(grad) %*% vb %*% grad
  vF
  
  if(rr_numerator > 0.00001 & rr_denominator > 0.00001){
    se <- sqrt(vF)
  }else{
    se <- NA
  }
  
  # 95% CI 
  lb = log(rr) - (qnorm(0.975)*se)
  ub = log(rr) + (qnorm(0.975)*se)
  
  # 2-sided p-value 
  t_stat = log(rr) / se
  pval = 2 * pt(-abs(t_stat), df = nrow(fit$data) - 1)
  
  rdid = (1 - rr)*100
  rdid_lb = (1 - exp(ub))*100
  rdid_ub = (1 - exp(lb))*100
  
  if(parameter=="RDID") return(list = data.frame(estimate = rdid, se = se, 
                                                 lb = rdid_lb, ub = rdid_ub, pval = pval))
  if(parameter=="ratio") return(list = data.frame(estimate = rr, se = se, 
                                                  lb = exp(lb), ub = exp(ub), pval = pval))
  
}

##############################################

# Documentation: fit_did_glm_seas
# Usage: fit_did_glm_seas(data, season, outcome, treatment, offset, covariates, label)
# Description: Preprocesses data and then fits a Poisson log-linear model 
#              and obtains an adjusted mean difference, DID and 95% CI
#              for a program year subtracting rates in pre-program years 
# Args/Options: 
# data:                    the data; must include a variable called seas, 
#                          indicating the program year / flu season
# season:                  number indicating program year / flu seas, as numeric
# outcome:                 the outcome column name, as a string
# treatment:               the treatment column name, as a string   
# offset:                  log population offset column name, as a string
# covariates:              adjustment covariates, as a character vector
# label:                   description of dataset, as a string
# 
# Returns: a formatted data frame that includes: adjusted difference estimate, 
# difference-in-difference estimate, standard error, the lower and upper bounds 
# of a 95% CI, and the season and person-years

fit_did_glm_seas = function(data, season, preseas, outcome, treatment, offset, covariates, label, parameter = "DID"){
  pre_seas_list <- preseas
  analysis_seas_list <- c(pre_seas_list, season)
  
  fitdata_diff <- data[data$seas == season,]
  
  fitdata_did <- data %>% 
    filter(seas %in% analysis_seas_list)
  
  fitdata_did$time = ifelse(fitdata_did$seas == season, 1, 0)
  
  cov_formula = paste(covariates, collapse = " + ")
  LHS = paste(outcome, "~")
  RHS_diff = paste0(treatment, " + ", cov_formula)
  RHS_did = paste0(treatment, "*", "time")
  
  diff_formula <- as.formula(paste(LHS, RHS_diff))
  did_formula <- as.formula(paste(LHS, RHS_did))
  
  glm_diff=glm(diff_formula,offset=fitdata_diff[[offset]],data=fitdata_diff,
               family=poisson(link=log))
  glm_did=glm(did_formula,offset=fitdata_did[[offset]],data=fitdata_did,
              family=poisson(link=log))
  
  if(parameter == "DID") res_diff = format.glm(glm_diff, coefname = treatment, parameter = "difference")
  if(parameter == "RDID") res_diff = get_rdid(glm_did, parameter = "ratio")
  
  if(parameter == "DID") res_did = get_did(glm_did)
  if(parameter == "RDID") res_did = get_rdid(glm_did, parameter = "RDID")
  
  if(parameter == "DID") res_diff = res_diff %>% mutate(parameter = "Mean difference")
  if(parameter == "DID") res_did = res_did %>% mutate(parameter = "Difference-in-difference")
  
  if(parameter == "RDID") res_diff = res_diff %>% mutate(parameter = "Ratio adjusted for pre-intervention")
  if(parameter == "RDID") res_did = res_did %>% mutate(parameter = "Relative difference-in-difference")
  
  N_diff = fitdata_diff %>% summarise(pyears = sum(N))
  N_did = fitdata_did %>% summarise(pyears = sum(N))
  
  res_diff = res_diff %>% mutate(pyears = N_diff$pyears) 
  res_did = res_did %>% mutate(pyears = N_did$pyears) 
  
  res = bind_rows(res_diff, res_did) %>% 
    mutate(seas = season,
           label = label,
           covariates = paste(covariates, collapse=", ")) %>%
    select(label, covariates, seas, pyears, everything()) %>%
    arrange(label, covariates, seas, parameter)
  
  return(res)
  
}