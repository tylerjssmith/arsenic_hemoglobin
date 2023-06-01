################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Drinking Water Arsenic and Hemoglobin -- Subsets

# Tyler Smith
# May 29, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Subset on Singleton Live Births ##########################################
df_slb <- df %>%
  filter(LIVEBIRTH == 1) %>%
  filter(SINGLETON == 1)

##### Restrict to Complete Covariates ##########################################
# Check Missingness
df_slb %>% 
  select(ln_wAs,ln_wFe,AGE,PARITY,EDUCATION,medSEMUAC,PEHCIGAR) %>%
  sapply(function(x) sum(is.na(x)))

# Singleton Live Births
df_slb <- df_slb %>%
  filter(!is.na(AGE))

##### Indicate Selection #######################################################
# Singleton Live Births
df_slb <- df_slb %>%
  mutate(VISIT1 = ifelse(!is.na(SEHEMO),  1, 0)) %>%
  mutate(VISIT2 = ifelse(!is.na(SVXHEMO), 1, 0)) %>%
  mutate(VISIT3 = ifelse(!is.na(SMHEMO),  1, 0)) %>%
  mutate(VISIT4 = ifelse(!is.na(SM3HEMO), 1, 0))

df_slb %>% count(VISIT1)
df_slb %>% count(VISIT2)
df_slb %>% count(VISIT3)
df_slb %>% count(VISIT4)

##### Scale Exposure Variables #################################################
# Singleton Live Births
df_slb <- df_slb %>%
  mutate(ln_wAs_IQR = ln_wAs / IQR(ln_wAs)) %>%
  mutate(ln_wFe_IQR = ln_wFe / IQR(ln_wFe))

# Check Correlations
with(df_slb, cor(ln_wAs_IQR, ln_wFe_IQR))

##### Indicate >7 Days Postpartum ##############################################
df_slb <- df_slb %>%
  mutate(SMDAYSPP7 = 
      ifelse(VISIT3 == 1 & SMDAYSPP > 7, 1, 
      ifelse(VISIT3 == 1 & SMDAYSPP <= 7, 0, NA))
  )

df_slb %>% count(SMDAYSPP7)

