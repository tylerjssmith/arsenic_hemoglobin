################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Drinking Water Arsenic and Hemoglobin -- Models

# Tyler Smith
# January 11, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(car)

##### Scale Exposure Variables #################################################
df <- df %>%
  mutate(ln_wAs_IQR = ln_wAs / IQR(ln_wAs)) %>%
  mutate(ln_wFe_IQR = ln_wFe / IQR(ln_wFe))

##### Visit 1 ##################################################################
summary(model_v1_cru_as <- lm(SEHEMO ~ ln_wAs_IQR, 
  data = subset(df, VISIT1 == 1)))
summary(model_v1_cru_fe <- lm(SEHEMO ~ ln_wFe_IQR, 
  data = subset(df, VISIT1 == 1)))
summary(model_v1_adj    <- lm(SEHEMO ~ ln_wAs_IQR + ln_wFe_IQR + 
    AGE + SEGSTAGE + PARITY + EDUCATION + LSI + medSEMUAC + PEHCIGAR, 
  data = subset(df, VISIT1 == 1)))

nobs(model_v1_cru_as)
nobs(model_v1_cru_fe)
nobs(model_v1_adj)

##### Visit 2 ##################################################################
summary(model_v2_cru_as <- lm(SVXHEMO ~ ln_wAs_IQR, 
  data = subset(df, VISIT2 == 1)))
summary(model_v2_cru_fe <- lm(SVXHEMO ~ ln_wFe_IQR, 
  data = subset(df, VISIT2 == 1)))
summary(model_v2_adj    <- lm(SVXHEMO ~ ln_wAs_IQR + ln_wFe_IQR + 
    AGE + SVXGSTAGE + PARITY + EDUCATION + LSI + medSEMUAC + PEHCIGAR, 
  data = subset(df, VISIT2 == 1)))

nobs(model_v2_cru_as)
nobs(model_v2_cru_fe)
nobs(model_v2_adj)

##### Visit 3 ##################################################################
summary(model_v3_cru_as <- lm(SMHEMO ~ ln_wAs_IQR, 
  data = subset(df, VISIT3 == 1)))
summary(model_v3_cru_fe <- lm(SMHEMO ~ ln_wFe_IQR, 
  data = subset(df, VISIT3 == 1)))
summary(model_v3_adj    <- lm(SMHEMO ~ ln_wAs_IQR + ln_wFe_IQR + 
    AGE + PARITY + EDUCATION + LSI + medSEMUAC + PEHCIGAR, 
  data = subset(df, VISIT3 == 1)))

nobs(model_v3_cru_as)
nobs(model_v3_cru_fe)
nobs(model_v3_adj)

##### Visit 4 ##################################################################
summary(model_v4_cru_as <- lm(SM3HEMO ~ ln_wAs_IQR, 
  data = subset(df, VISIT4 == 1)))
summary(model_v4_cru_fe <- lm(SM3HEMO ~ ln_wFe_IQR, 
  data = subset(df, VISIT4 == 1)))
summary(model_v4_adj    <- lm(SM3HEMO ~ ln_wAs_IQR + ln_wFe_IQR + 
    AGE + PARITY + EDUCATION + LSI + medSEMUAC + PEHCIGAR, 
  data = subset(df, VISIT4 == 1)))

nobs(model_v4_cru_as)
nobs(model_v4_cru_fe)
nobs(model_v4_adj)

##### Check Model Assumptions ##################################################
# Residual Plots
residualPlots(model_v1_adj)
residualPlots(model_v2_adj)
residualPlots(model_v3_adj)
residualPlots(model_v4_adj)

# Multicollinearity
vif(model_v1_adj)
vif(model_v2_adj)
vif(model_v3_adj)
vif(model_v4_adj)






