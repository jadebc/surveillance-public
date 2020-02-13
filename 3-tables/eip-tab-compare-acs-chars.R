##########################################
# Shoo the Flu evaluation
# Surveillance analysis

# Import and clean ACS 3-year 2013 data
# Subset by zip code
# To compare demographics by zip code
##########################################
rm(list=ls())
source(here::here("0-config.R"))

# Formula for converting MOEs to confidence interval of proportion
# https://www.census.gov/content/dam/Census/library/publications/2018/acs/acs_general_handbook_2018_ch08.pdf

prop_ci = function(n, N, moe_n, moe_N){
  if(is.factor(n)) n = as.numeric(as.character(n))
  if(is.factor(N)) N = as.numeric(as.character(N))
  if(is.factor(moe_n)) moe_n = as.numeric(as.character(moe_n))
  if(is.factor(moe_N)) moe_N = as.numeric(as.character(moe_N))
  
  p = n/N
  moe_p = (1/N)*sqrt(moe_n^2 - (p^2 * moe_N^2))
  se = (moe_p / 1.9645)
  lb = p - se*qnorm(0.975)
  ub = p + se*qnorm(0.975)
  return(list(est = p*100, lb = lb*100, ub = ub*100))
}


process_percent = function(data, label){
  data %>%
    mutate(est = as.numeric(as.character(est)),
           moe = as.numeric(as.character(moe))) %>%
    mutate(lb = est - (moe/1.645)*qnorm(0.975),
           ub = est + (moe/1.645)*qnorm(0.975)) %>%
    mutate(category  = label) %>%
    select(category, dist, est, lb, ub)
}

#----------------------------------------
# Educational attainment
#----------------------------------------
edu=read.csv("~/Dropbox/Flu/Surveillance/Census/ACS demographics/3-year district subset/education/ACS_13_3YR_S1501_with_ann.csv",header=TRUE)

# these estimates are read in as percentages with a 90% CI MOE

# keep columns with relevant data
edu.cols=c("GEO.display.label",
           "HC01_EST_VC02","HC01_MOE_VC02",
           "HC01_EST_VC03","HC01_MOE_VC03",
           "HC01_EST_VC04","HC01_MOE_VC04",
           "HC01_EST_VC05","HC01_MOE_VC05")

edu_sub_cols = edu[,colnames(edu) %in% edu.cols]
edu_sub_rows = edu_sub_cols %>% filter(`GEO.display.label` %in% 
        c("Oakland Unified School District, California",
          "West Contra Costa Unified School District, California"))

edu_clean = edu_sub_rows %>% 
  mutate(dist = ifelse(`GEO.display.label` == "Oakland Unified School District, California",
                       "OUSD", "WCCUSD")) %>%
  rename(
    est_less_hs        = HC01_EST_VC02,
    moe_less_hs        = HC01_MOE_VC02,
    est_hs             = HC01_EST_VC03,
    moe_hs             = HC01_MOE_VC03,
    est_some_coll      = HC01_EST_VC04,
    moe_some_coll      = HC01_MOE_VC04,
    est_coll           = HC01_EST_VC05,
    moe_coll           = HC01_MOE_VC05
  ) %>% 
  select(-`GEO.display.label`) 


p_less_hs = edu_clean %>% 
  rename(est = est_less_hs, 
         moe = moe_less_hs) %>%
  process_percent(label = "Less than high school") 

p_hs = edu_clean %>% 
  rename(est = est_hs, 
         moe = moe_hs) %>%
  process_percent(label = "High school graduate") 

p_some_coll = edu_clean %>% 
  rename(est = est_some_coll, 
         moe = moe_some_coll) %>%
  process_percent(label = "Some college or Associate's") 

p_coll = edu_clean %>% 
  rename(est = est_coll, 
         moe = moe_coll) %>%
  process_percent(label = "Bachelor's degree or higher") 


edu = bind_rows(p_less_hs, p_hs, p_some_coll, p_coll) %>%
  select(dist, category, everything())


#----------------------------------------
# Median income
#----------------------------------------
medinc=read.csv("~/Dropbox/Flu/Surveillance/Census/ACS demographics/3-year district subset/income/ACS_13_3YR_S1903_with_ann.csv",header=TRUE)

# keep columns with relevant data
medinc.cols=c("GEO.display.label","HC02_EST_VC02","HC02_MOE_VC02")

medinc_cols = medinc[,colnames(medinc) %in% medinc.cols]

medinc_rows = medinc_cols %>% filter(`GEO.display.label` %in% 
         c("Oakland Unified School District, California",
           "West Contra Costa Unified School District, California"))
medinc_clean = medinc_rows %>% 
  mutate(dist = ifelse(`GEO.display.label` == "Oakland Unified School District, California",
                       "OUSD", "WCCUSD")) %>%
  rename(
    est_medinc      = HC02_EST_VC02,
    moe_medinc      = HC02_MOE_VC02
    
  ) %>% mutate(
    est_medinc = as.numeric(as.character(est_medinc)),
    moe_medinc = as.numeric(as.character(moe_medinc))
  ) %>%
  select(-`GEO.display.label`) 

# get 95% CI
medinc_clean = medinc_clean %>% 
  rename(est = est_medinc) %>% 
  mutate(
         lb = est - (moe_medinc / 1.645)*qnorm(0.975),
         ub = est + (moe_medinc / 1.645)*qnorm(0.975),
         category = "Median household income ($)" 
     ) %>%
  select(dist, category, est, lb, ub)

#----------------------------------------
# School enrollment
#----------------------------------------
school=read.csv("~/Dropbox/Flu/Surveillance/Census/ACS demographics/3-year district subset/school enrollment/ACS_13_3YR_S1401_with_ann.csv",header=TRUE)

# keep columns with relevant data
school.cols=c("GEO.display.label",
              "HC02_EST_VC04","HC02_MOE_VC04",
              "HC03_EST_VC04","HC03_MOE_VC04",
              "HC02_EST_VC05","HC02_MOE_VC05",
              "HC03_EST_VC05","HC03_MOE_VC05",
              "HC02_EST_VC06","HC02_MOE_VC06",
              "HC03_EST_VC06","HC03_MOE_VC06")

school_cols = school[,colnames(school) %in% school.cols]

school_rows = school_cols %>% filter(`GEO.display.label` %in% 
              c("Oakland Unified School District, California",
              "West Contra Costa Unified School District, California")) %>% 
  mutate(dist = ifelse(`GEO.display.label` == "Oakland Unified School District, California",
                       "OUSD", "WCCUSD")) %>%
  rename(
    est_pub_k      = HC02_EST_VC04,
    moe_pub_k      = HC02_MOE_VC04,
    est_priv_k     = HC03_EST_VC04,
    moe_priv_k     = HC03_MOE_VC04,
    est_pub_1_4    = HC02_EST_VC05,
    moe_pub_1_4    = HC02_MOE_VC05,
    est_priv_1_4   = HC03_EST_VC05,
    moe_priv_1_4   = HC03_MOE_VC05,
    est_pub_5_8    = HC02_EST_VC06,
    moe_pub_5_8    = HC02_MOE_VC06,
    est_priv_5_8   = HC03_EST_VC06,
    moe_priv_5_8   = HC03_MOE_VC06
    
  ) %>%
  select(-`GEO.display.label`) 


pub_k = school_rows %>% 
  rename(est = est_pub_k, 
         moe = moe_pub_k) %>%
  process_percent(label = "Public kindergarten") 

priv_k = school_rows %>% 
  rename(est = est_priv_k, 
         moe = moe_priv_k) %>%
  process_percent("Private kindergarten")

pub1_4 = school_rows %>% 
  rename(est = est_pub_1_4, 
         moe = moe_pub_1_4) %>%
  process_percent("Public grades 1-4") 

priv1_4 = school_rows %>% 
  rename(est = est_priv_1_4, 
         moe = moe_priv_1_4) %>%
  process_percent("Private grades 1-4") 

pub5_8 = school_rows %>% 
  rename(est = est_pub_5_8, 
         moe = moe_pub_5_8) %>%
  process_percent( "Public grades 5-8")

priv5_8 = school_rows %>% 
  rename(est = est_priv_5_8, 
         moe = moe_priv_5_8) %>%
  process_percent("Private grades 5-8")

enroll = bind_rows(pub_k, priv_k, 
                   pub1_4, priv1_4,
                   pub5_8, priv5_8)

#----------------------------------------
# Poverty level
#----------------------------------------
poverty=read.csv("~/Dropbox/Flu/Surveillance/Census/ACS demographics/3-year district subset/poverty/ACS_13_3YR_S1701_with_ann.csv",header=TRUE)

# keep columns with relevant data
poverty.cols=c("GEO.display.label","HC03_EST_VC01","HC03_MOE_VC01")

poverty_cols = poverty[,colnames(poverty) %in% poverty.cols]

poverty_rows = poverty_cols %>% filter(`GEO.display.label` %in% 
                  c("Oakland Unified School District, California",
                    "West Contra Costa Unified School District, California"))

poverty_clean = poverty_rows %>% 
  mutate(dist = ifelse(`GEO.display.label` == "Oakland Unified School District, California", "OUSD", "WCCUSD")) %>%
  rename(
    est_pov         = HC03_EST_VC01,
    moe_pov         = HC03_MOE_VC01
  ) %>% 
  select(-`GEO.display.label`) 


p_pov = poverty_clean %>% 
  rename(est = est_pov, 
         moe = moe_pov) %>%
  process_percent(label = "Households below the poverty level (%)") 


#----------------------------------------
# race
#----------------------------------------
race=read.csv("~/Dropbox/Flu/Surveillance/Census/ACS demographics/3-year district subset/race/ACS_13_3YR_B02001_with_ann.csv",header=TRUE)

# keep columns with relevant data
race.cols=c("GEO.display.label", 
            "HD01_VD01", "HD02_VD01", 
            "HD01_VD02", "HD02_VD02", 
            "HD01_VD03", "HD02_VD03", 
            "HD01_VD04", "HD02_VD04", 
            "HD01_VD05", "HD02_VD05", 
            "HD01_VD06", "HD02_VD06", 
            "HD01_VD07", "HD02_VD07", 
            "HD01_VD08", "HD02_VD08")

race_cols = race[,colnames(race) %in% race.cols]

race_rows = race_cols %>% filter(`GEO.display.label` %in% 
                                         c("Oakland Unified School District, California",
                                           "West Contra Costa Unified School District, California"))  %>% 
  mutate(dist = ifelse(`GEO.display.label` == "Oakland Unified School District, California",
                       "OUSD", "WCCUSD")) %>%
  rename(
    est_total      = HD01_VD01,
    moe_total      = HD02_VD01,
    est_white      = HD01_VD02,
    moe_white      = HD02_VD02,
    est_black      = HD01_VD03,
    moe_black      = HD02_VD03,
    est_natamer    = HD01_VD04,
    moe_natamer    = HD02_VD04,
    est_asian      = HD01_VD05,
    moe_asian      = HD02_VD05,
    est_hawaii     = HD01_VD06,
    moe_hawaii     = HD02_VD06,
    est_other      = HD01_VD07,
    moe_other      = HD02_VD07,
    est_twoplus    = HD01_VD08,
    moe_twoplus    = HD02_VD08
    
  ) %>%
  select(-`GEO.display.label`) 

p_white = bind_rows(prop_ci(
  n = race_rows$est_white, 
  N = race_rows$est_total, 
  moe_n = race_rows$moe_white,
  moe_N = race_rows$moe_total
)) %>% mutate(dist = c("OUSD", "WCCUSD"),
              category = "White")

p_black = bind_rows(prop_ci(
  n = race_rows$est_black, 
  N = race_rows$est_total, 
  moe_n = race_rows$moe_black,
  moe_N = race_rows$moe_total
)) %>% mutate(dist = c("OUSD", "WCCUSD"),
              category = "Black or African American")

p_natamer = bind_rows(prop_ci(
  n = race_rows$est_natamer, 
  N = race_rows$est_total, 
  moe_n = race_rows$moe_natamer,
  moe_N = race_rows$moe_total
)) %>% mutate(dist = c("OUSD", "WCCUSD"),
              category = "American Indian and Alaska Native")

p_asian = bind_rows(prop_ci(
  n = race_rows$est_asian, 
  N = race_rows$est_total, 
  moe_n = race_rows$moe_asian,
  moe_N = race_rows$moe_total
)) %>% mutate(dist = c("OUSD", "WCCUSD"),
              category = "Asian")

p_hawaii = bind_rows(prop_ci(
  n = race_rows$est_hawaii, 
  N = race_rows$est_total, 
  moe_n = race_rows$moe_hawaii,
  moe_N = race_rows$moe_total
)) %>% mutate(dist = c("OUSD", "WCCUSD"),
              category = "Native Hawaiian and Other Pacific Islander")

p_other = bind_rows(prop_ci(
  n = race_rows$est_other, 
  N = race_rows$est_total, 
  moe_n = race_rows$moe_other,
  moe_N = race_rows$moe_total
)) %>% mutate(dist = c("OUSD", "WCCUSD"),
              category = "Other race")

p_twoplus = bind_rows(prop_ci(
  n = race_rows$est_twoplus, 
  N = race_rows$est_total, 
  moe_n = race_rows$moe_twoplus,
  moe_N = race_rows$moe_total
)) %>% mutate(dist = c("OUSD", "WCCUSD"),
              category = "Two or more races")

race_clean = bind_rows(p_white, p_black, p_asian, 
                       p_hawaii, p_other, p_twoplus) %>%
  select(dist, category, everything())


#----------------------------------------
# ethnicity
#----------------------------------------
ethnicity=read.csv("~/Dropbox/Flu/Surveillance/Census/ACS demographics/3-year district subset/ethnicity/ACS_13_3YR_B03002_with_ann.csv",header=TRUE)

# keep columns with relevant data
eth.cols=c("GEO.display.label", 
           "HD01_VD01", "HD02_VD01",
           "HD01_VD12", "HD02_VD12")

eth_cols = ethnicity[,colnames(ethnicity) %in% eth.cols]

eth_rows = eth_cols %>% filter(`GEO.display.label` %in% 
                                   c("Oakland Unified School District, California",
                                     "West Contra Costa Unified School District, California"))  %>% 
  mutate(dist = ifelse(`GEO.display.label` == "Oakland Unified School District, California",
                       "OUSD", "WCCUSD")) %>%
  rename(
    est_total      = HD01_VD01,
    moe_total      = HD02_VD01,
    est_hisp       = HD01_VD12,
    moe_hisp       = HD02_VD12
    
  ) %>%
  select(-`GEO.display.label`) 

p_eth = bind_rows(prop_ci(
  n = eth_rows$est_hisp, 
  N = eth_rows$est_total, 
  moe_n = eth_rows$moe_hisp,
  moe_N = eth_rows$moe_total
)) %>% mutate(dist = c("OUSD", "WCCUSD"),
              category = "Hispanic or Latino ethnicity")  %>%
  select(dist, category, everything())


#----------------------------------------
# Combine all data
#----------------------------------------
all=rbind(edu, medinc_clean, p_pov, enroll, race_clean, p_eth)

all = all %>% mutate(
  est_f = paste0(sprintf("%0.0f", est), " (", 
                sprintf("%0.0f", lb), ", ",
                sprintf("%0.0f", ub), ")")
  ) %>% 
  select(dist, category, est_f)

# convert long to wide
out = spread(all, key = dist, value = est_f)

out = out %>% mutate(
  category = factor(category, levels = c(
    "Median household income ($)",
    "Households below the poverty level (%)",
    "Less than high school",
    "High school graduate",
    "Some college or Associate's",
    "Bachelor's degree or higher",
    "Public kindergarten",
    "Private kindergarten",
    "Public grades 1-4",
    "Private grades 1-4",
    "Public grades 5-8",
    "Private grades 5-8",
    "White",
    "Black or African American",
    "Asian",
    "Other race",
    "American Indian and Alaska Native",
    "Native Hawaiian and Other Pacific Islander",
    "Two or more races",
    "Hispanic or Latino ethnicity"
  ))
)  %>% arrange(category)

saveRDS(out,file=paste0(tab_dir, "table1.RDS"))
write.csv(out,file=paste0(tab_dir, "table1.csv"))
