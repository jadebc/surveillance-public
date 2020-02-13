##########################################
# Shoo the Flu evaluation
# Surveillance analysis

# Analysis of Shoo the Flu indirect effects
# on flu hospitalization

# Base functions
##########################################


# function to extract point estimates from Poisson regression
format.glm=function(x, coefname, parameter){
  res=coef(summary(x))
  row=grep(coefname,rownames(res))
  rr=res[row,1]
  se=res[row,2]
  lb=rr-(qnorm(0.975)*se)
  ub=rr+(qnorm(0.975)*se)
  pval = res[row,4]
  out=data.frame(estimate=rr,se=se,lb=lb,ub=ub, pval=pval)
  
  if(parameter=="ratio")  out=exp(out)

  return(out)
}

# format point estimate and ci
pt.est.ci.f=function(est, lb, ub, decimals,scale){
  a=sprintf(paste("%0.0",decimals,"f",sep=""),est*scale)
  b=sprintf(paste("%0.0",decimals,"f",sep=""),lb*scale)
  c=sprintf(paste("%0.0",decimals,"f",sep=""),ub*scale)
  return(paste(a," (",b,", ",c,")",sep=""))
}



#-----------------------------------------------
# function to remove leftover minus sign
# can't figure out why this is occuring with dplyr
#-----------------------------------------------
removedash=function(x){
  if(length(grep("-",x))>0){
    return(gsub("- ","",x))
  }
}



#-----------------------------------------------
# clean census label for years
# this function finds the last dash in a string
# and subsets to the number of years after the dash
# and removes the word "years"
#-----------------------------------------------
getyears=function(x){
  pos=gregexpr("-",x)
  lastdash=pos[[1]][length(pos[[1]])]
  len=nchar(as.character(x))
  years=substr(x,lastdash+2,len)
  years=gsub(" years","",years)
  years=gsub(" year","",years)
  years=gsub("Under 1","0",years)
  return(removedash(years))
}


#-----------------------------------------------
# clean census label for sex
# this function extracts the sex from the label
#-----------------------------------------------
getsex=function(x){
  # x=as.character(x)
  colon=gregexpr(":",x)[[1]][1]
  sex=substr(x,1,colon-1)
  return(sex)
}

# function to reformat clean race-specific census files
cens.race=function(data,race,ousd.zip,wcc.zip){
  
  # make age label key
  age.key=data.frame(age=t(data[1,]),lab=colnames(data))
  colnames(age.key)[1]="age"
  
  # total rows to drop
  col.drop=colnames(data)[which(as.character(unlist(data[1,])) %in% c("Total:","Male:","Female:"))]
  
  # subset to rows with ousd/wcc zip codes and keep label row
  # drop columns with totals by sex
  # drop label row
  data.out = data %>% 
    filter( GEO.id2 %in% c(ousd.zip,wcc.zip)) %>%
    mutate(dist=ifelse(GEO.id2 %in% ousd.zip,"OUSD","WCCUSD")) %>%
    select(-one_of(c("GEO.id","GEO.display.label",col.drop)))
  
  # reshape data to long format   
  data.l = data.out %>%
    gather(lab,N,-dist,-GEO.id2) %>%
    rename(zip=GEO.id2) 
  
  # add back age labels, clean age and sex, add race
  data.la = data.l %>%
    group_by(dist,zip) %>%
    left_join(age.key, by = "lab") %>%
    mutate(sex =getsex(age),race=race) %>%
    mutate(age=getyears(age)) %>%
    select(-lab)
  
  data.la$sex[data.la$sex=="Fema"]="Female"
  
  return(data.la)
}



##############################################
# Documentation: prep_flu
# Usage: prep_flu(data, pop)
# Description: aggregate CEIP flu case counts by race, 
#              age, sex, season, and district depending
#              on which population denominator (census vs.
#              acs) is used
# Args/Options: 
# data:                    data with flu case counts
#                          must include a variable called seas, 
#                          indicating the program year / flu season, 
#                          dist variable indicating OUSD vs. WCCUSD, 
#                          and any covariates used for grouping
# pop:                     either "census" indicating census data is to be used or
#                          "acs" indicating that acs data is to be used
 
# Returns: a dataset with total flu cases within season, dist, age, race, and sex
# if census data is used and within season, dist, age if ACS is used

prep_flu = function(data, pop, covariates, yname = "flupos", all_race_cats=FALSE){
  
  if(pop=="census" & "race" %in% covariates){
    if(!all_race_cats){
      # drop if race is unknown since that category is
      # not in the census 
      print(paste("Number of observations from flu dataset dropped due to unknown race:", 
                  nrow(data[data$race=="Unknown",]), "out of", nrow(data)))
      data = data %>% filter(race!="Unknown")
      
      # combine less common race categories
      if(!all_race_cats){
        data = data %>% mutate(race = case_when(
          race == "American Indian/Alaska Native" ~ "Other", 
          race == "Two or more races" ~ "Other",
          race == "Hispanic" ~ "Other",
          TRUE ~ race))
      }
    }
  }
  
  if(pop=="acs"){
    data = data %>%
      mutate(agecat2 = case_when(
        agecat == "Under 5"                                    ~ "Under 5 years",
        agecat == "5 to 9"                                     ~ "5-9 years",
        agecat == "10 to 14"                                   ~ "10-14 years",
        agecat == "15 to 17" | agecat == "18 and 19"           ~ "15-19 years",
        agecat == "20" | agecat == "21" | agecat == "22 to 24" ~ "20-24 years",
        agecat == "25 to 29" | agecat == "30 to 34"            ~ "25-34 years",
        agecat == "35 to 39" | agecat == "40 to 44"            ~ "35-44 years",
        agecat == "45 to 49" | agecat == "50 to 54"            ~ "45-54 years",
        agecat == "55 to 59"                                   ~ "55-59 years",
        agecat == "60 and 61" | agecat == "62 to 64"           ~ "60-64 years",
        agecat == "65 and 66" | agecat == "67 to 69" | agecat == "70 to 74" ~ "65-74 years",
        agecat == "75 to 79" | agecat == "80 to 84"            ~ "75-84 years",
        agecat == "85 and over"                                ~ "85+ years"
      )) %>%
    select(-agecat) %>%
    rename(agecat = agecat2) %>%
    mutate(agecat = factor(agecat, 
           levels = c("Under 5 years", "5-9 years", "10-14 years",
                      "15-19 years", "20-24 years", "25-34 years",  
                      "35-44 years", "45-54 years", "55-59 years",
                      "60-64 years", "65-74 years", "75-84 years", 
                      "85+ years")))
    
    data = data %>% mutate(agecat = fct_explicit_na(agecat, na_level = "(Missing)"))
    
  }
  
  if("ageyrs" %in% covariates){
    data = data %>% mutate(
      ageyrs = case_when(
        ageyrs == 0 ~ "Under 1 year",
        ageyrs == 1 ~ "1 year",
        ageyrs >1 & ageyrs <=99 ~ paste0(ageyrs, " years"),
        ageyrs >=100 & ageyrs<=104 ~ "100 to 104 years",
        ageyrs >=105 & ageyrs<=109 ~ "105 to 109 years",
        ageyrs >=110 ~ "110 years and over",
        TRUE ~ "Missing"
      )
    ) %>%
    mutate(ageyrs = as.factor(ageyrs))
  }
  
  # drop if age is missing since can't merge with census
  if("agecat" %in% covariates & nrow(data[data$agecat=="(Missing)",])>0){
    print(paste("Number of observations from flu dataset dropped due to missing age:", 
                nrow(data[data$agecat=="(Missing)",]), "out of", nrow(data)))
    data = data %>% filter(agecat!="(Missing)")
  }
  if("ageyrs" %in% covariates & nrow(data[data$ageyrs=="Missing",])>0){
    print(paste("Number of observations from flu dataset dropped due to missing age:", 
                nrow(data[data$ageyrs=="Missing",]), "out of", nrow(data)))
    data = data %>% filter(ageyrs!="Missing")
  }
  
  if(!all_race_cats){
    group_vars_sym <- syms(c("flusesn", "dist", covariates))

    data = data %>% group_by(!!!(group_vars_sym))

    data = data %>%
      summarise(flucases = sum(!!sym(yname), na.rm=TRUE)) %>%
      rename(seas = flusesn) %>% 
      ungroup() %>% 
      mutate(dist = as.factor(dist)) 
  
  }
  
  if(all_race_cats){

    data_race = data %>%
      group_by(flusesn, dist, agecat, sex, race) %>%
      summarise(flucases = sum(!!sym(yname), na.rm=TRUE)) %>%
      rename(seas = flusesn) %>% 
      ungroup() %>% 
      mutate(dist = as.factor(dist)) 
    
    data_hispanic = data %>%
      group_by(flusesn, dist, agecat, sex) %>%
      filter(Ethnic==1) %>%
      summarise(flucases = sum(!!sym(yname), na.rm=TRUE)) %>%
      rename(seas = flusesn) %>% 
      ungroup() %>% 
      mutate(dist = as.factor(dist),
             race = "Hispanic") %>%
      select(seas, dist, agecat, sex, race, flucases)
    
    data = bind_rows(data_race, data_hispanic)
    
  }
  
  # filter out anyone with missing values
  # since they will be dropped in the merge
  # with the census data
  if(paste(nrow(data[!complete.cases(data),]))>0){
    print(paste(nrow(data[!complete.cases(data),]), "rows in the flu dataset dropped due to missingness"))
  }
  data = data[complete.cases(data),]

  # define covariates as factors
  data[covariates] <- lapply(data[covariates], factor) 
  
  # rename sex factor labels
  if("sex" %in% covariates){
    data = data %>% 
      mutate(sex = recode_factor(sex, "Female" = "female",
                                 "Male" = "male"))
  }
  
  return(data)
}

##############################################
# Documentation: prep_census
# Usage: prep_census(data, season_list)
# Description: aggregate census population counts by race, 
#              age, sex, season, and district 
# Args/Options: 
# data:                    the data; must include a variable called seas, 
#                          indicating the program year / flu season, 
#                          dist variable indicating OUSD vs. WCCUSD, 
#                          and race, agecat, and sex
# covariates:              vector of strings containing covariates to be included in dataset
# season_list:             vector of seasons to be included

# Returns: a dataset with population count within season, dist, age, race, and sex
# Note: The same population is used for each season based on
# the 2010 census

prep_census = function(data, covariates, season_list, all_race_cats=FALSE){
  
  if("race" %in% covariates & !all_race_cats){
    # combine less common race categories
    data = data %>% mutate(race = ifelse(race == "American Indian/Alaska Native" | 
                                           race == "Two or more races", "Other", race))
    
  }
  
  covs_sym <- syms(covariates)
    
    dist_cov_sum = data %>% 
      group_by(!!!(covs_sym))
    
    dist_cov_sum = suppressMessages(mutate_if(dist_cov_sum, is.factor, as.character)) 
    
    dist_cov_sum = dist_cov_sum %>%
      summarise(OUSD = sum(as.numeric(OUSD)), 
                WCCUSD = sum(as.numeric(WCCUSD)))

  # convert wide to long
  cens.long = melt(dist_cov_sum, id.vars=covariates, 
                   value.name = "N")
  colnames(cens.long)[which(colnames(cens.long)=="variable")] = "dist"

  # template
  nseas = length(season_list)
  cens_seas = list()
  
  for(i in 1:nseas){
    cens_seas[[i]] = cens.long
    cens_seas[[i]] = cens_seas[[i]] %>% mutate(seas = season_list[i])
  }
  
  template = bind_rows(cens_seas)
  
  template[covariates] <- lapply(template[covariates], factor) 
  
  # rename sex factor labels
  if("sex" %in% covariates){
    template = template %>%
      mutate(sex = recode_factor(sex, "Female" = "female",
                    "Male" = "male"))
  }
  
  # drop if N = 0
  template = template %>% filter(N>0)
  
  return(template)
}

##############################################
# Documentation: prep_acs
# Usage: prep_acs(data)
# Description: aggregate census population counts by 
#              age, season, and district 
# Args/Options: 
# data:                    ACS data; must include a variable called seas, 
#                          indicating the program year / flu season, 
#                          dist variable indicating OUSD vs. WCCUSD

# Returns: a dataset with population count within season, dist, age category

prep_acs = function(data){
  # convert from wide to long
  acs.l = melt(data, id.vars=c("agecat", "year"))
  colnames(acs.l) = c("agecat", "year", "dist", "N")
  
  # drop 2010-11 data 
  acs.l = acs.l %>% filter(year!=2010 & year!=2011)
  
  # assigning season; using the most recent year for the season
  # (eg, use 2014 pop for 2013-14)
  acs.l = acs.l %>%
    mutate(seas = case_when(
      year == 2012 ~ 1112,
      year == 2013 ~ 1213,
      year == 2014 ~ 1314,
      year == 2015 ~ 1415,
      year == 2016 ~ 1516,
      year == 2017 ~ 1617,
      year == 2018 ~ 1718
    )) %>% 
    select(-year) %>%
    arrange()
  
  return(acs.l)
}





#----------------------------------------
# clean label in census dataset with age in 
# single years or in categories
# extract the text after the last dash
# in the string and save it as a vector

# x = vector of "Id" variable in census dataset
# returns cleaned label with age in years
#----------------------------------------
clean_age = function(x){
  dash_loc_list = gregexpr("-", x)
  
  x_out = matrix(NA, nrow = length(x), 1)
  for(i in 1:length(dash_loc_list)){
    dash_loc_list_element1 = dash_loc_list[[i]][length(dash_loc_list[[i]])]
    if(dash_loc_list_element1>0){
      x_out[i,1] = substr(x[i], dash_loc_list_element1+2, nchar(as.character(x[i])))
    }else{
      x_out[i,1] = ""
    }
    
  }
  
  return(x_out)
  
}


#----------------------------------------
# wrapper function that cleans both
# age and sex and filters to ages 
# in a list of characters

# removes "years" from age cat because
# this function is used with race data 
# which uses age categories

# inputs: 
# metadata: meta data from census
# agelist: character string of age values to filter on
#----------------------------------------
clean_age_sex = function(metadata, agelist){
  
  out = metadata %>% 
    mutate(agecat = clean_age(Id),
           sex = clean_sex(Id)) %>%
    mutate(agecat = gsub(" years", "", agecat)) %>%
    filter(agecat %in% agecats) 
  
  # check that there is one row for each age and sex 
  assert_that(max(data.frame(table(out$agecat, out$sex))$Freq)==1)
  
  out = out %>% mutate(label = as.character(GEO.id)) %>%
    select(c(label, agecat, sex))
  
  return(out)
  
}

#----------------------------------------
# function that subsets census data to zip
# codes of interest and reformats data, then
# applies selected meta data labels

# inputs: 
# census_data: raw census data file
# meta_data: cleaned metadata file corresponding to census file (cleaned by clean_age_sex)
# zip_name: character string for name of column with zip code in raw census data
# ousd_zips: vector of OUSD zip codes
# wcc_zips: vector of WCC zip codes
#----------------------------------------
process_census = function(census_data, meta_data, zip_name, ousd_zips, wcc_zips){
  
  # create vector of labels to keep
  column_names = as.character(meta_data$label)
  assert_that(length(column_names)>0)
  
  # subset to zip code, age and sex columns
  cens = census_data %>% select(c(zip_name, column_names))
  
  # clean zip code
  zip_name_sym = sym(zip_name)
  cens = cens %>% rename(zip = !!(zip_name_sym))
  
  # subset by zip code
  cens = cens %>% 
    filter(zip %in% c(ousd_zips, wcc_zips)) %>%
    mutate(dist = case_when(
      zip %in% ousd_zips ~ "OUSD",
      zip %in% wcc_zips ~ "WCCUSD"
    ))
  
  print("Zip codes not included in census data:")
  print(paste(ousd_zips[!ousd_zips %in% names(table(cens$zip[cens$dist=="OUSD"]))],
              wcc_zips[!wcc_zips %in% names(table(cens$zip[cens$dist=="WCCUSD"]))]))
  
  # https://www.zipmap.net/California/Alameda_County/Oakland.htm
  
  #----------------------------------------
  # aggregate within district
  #----------------------------------------
  cens.dist = cens %>% select(-zip) 
  
  # convert factor to numeric
  for(i in 1:(ncol(cens.dist)-1)){
    cens.dist[,i] = as.numeric(as.character(cens.dist[,i]))
  }
  
  cens.dist = cens.dist %>%
    group_by(dist) %>%
    summarise_all(sum)
  
  cens.dist.l = t(cens.dist)
  colnames(cens.dist.l) = c("OUSD", "WCCUSD")
  cens.dist.l = cens.dist.l[-1,]
  cens.dist.l = as.data.frame(cens.dist.l)
  
  #----------------------------------------
  # merge in labels from metadata
  #----------------------------------------
  cens.dist.l$label = rownames(cens.dist.l)
  rownames(cens.dist.l) = NULL
  
  meta_label = meta_data %>% mutate(label = as.character(label))
  
  cens.clean = full_join(cens.dist.l, meta_label, by = c("label"))
  
  # drop label
  cens.clean = cens.clean %>% select(-label)
  
  return(cens.clean)
}



#----------------------------------------
# create sex variable for census dataset with age in 
# single years
# extract the text after the last dash
# in the string and save it as a vector

# x = vector of "Id" variable in census dataset
# returns cleaned label with sex 
#----------------------------------------
clean_sex = function(x){
  
  df = data.frame(x = as.character(x))
  
  df = df %>%
    mutate(
      male = str_detect(x, "Male"),
      female = str_detect(x, "Female"),
      both = str_detect(x, "Both sexes")
    ) %>%
    mutate(multicheck = male + female + both)
  
  assert_that(max(df$multicheck)==1, msg = "More than one sex label present")
  
  df = df %>%
    mutate(sex = case_when(
      male ~ "male",
      female ~ "female",
      both ~ "both"
    )) 
  
  return(df$sex)
}


#----------------------------------------
# subset data by a variable

# data: to be subset
# subset: character string for name of variable
# to subset by if that variable = 1

# if subset is "All", return full data
#----------------------------------------
subset_data = function(data, subset){
  if(subset!= "All"){
    data = data %>% filter(!!sym(subset) == 1)
    return(data)
  }else{
    return(data)
  }
}
