################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Drinking Water Arsenic and Hemoglobin -- Variable Importance

# Tyler Smith
# January 24, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Visit 1 ##################################################################
summary(lm(SEHEMO ~ ln_wAs_IQR, 
  data = subset(df, VISIT1 == 1)))

summary(lm(SEHEMO ~ ln_wAs_IQR + AGE + SEGSTAGE + PARITY + EDUCATION + LSI + medSEMUAC + PEHCIGAR, 
  data = subset(df, VISIT1 == 1)))

summary(lm(SEHEMO ~ ln_wAs_IQR + ln_wFe_IQR + AGE + SEGSTAGE + PARITY + EDUCATION + LSI + medSEMUAC + PEHCIGAR, 
  data = subset(df, VISIT1 == 1)))

##### Visit 2 ##################################################################
summary(lm(SVXHEMO ~ ln_wAs_IQR, 
  data = subset(df, VISIT2 == 1)))

summary(lm(SVXHEMO ~ ln_wAs_IQR + AGE + SVXGSTAGE + PARITY + EDUCATION + LSI + medSEMUAC + PEHCIGAR, 
  data = subset(df, VISIT2 == 1)))

summary(lm(SVXHEMO ~ ln_wAs_IQR + ln_wFe_IQR + AGE + SVXGSTAGE + PARITY + EDUCATION + LSI + medSEMUAC + PEHCIGAR, 
  data = subset(df, VISIT2 == 1)))

##### Visit 3 ##################################################################
summary(lm(SMHEMO ~ ln_wAs_IQR, 
  data = subset(df, VISIT3 == 1)))

summary(lm(SMHEMO ~ ln_wAs_IQR + AGE + PARITY + EDUCATION + LSI + medSEMUAC + PEHCIGAR, 
  data = subset(df, VISIT3 == 1)))

summary(lm(SMHEMO ~ ln_wAs_IQR + ln_wFe_IQR + AGE + PARITY + EDUCATION + LSI + medSEMUAC + PEHCIGAR, 
  data = subset(df, VISIT3 == 1)))

##### Visit 4 ##################################################################
summary(lm(SM3HEMO ~ ln_wAs_IQR, 
  data = subset(df, VISIT4 == 1)))

summary(lm(SM3HEMO ~ ln_wAs_IQR + AGE + PARITY + EDUCATION + LSI + medSEMUAC + PEHCIGAR, 
  data = subset(df, VISIT4 == 1)))

summary(lm(SM3HEMO ~ ln_wAs_IQR + ln_wFe_IQR + AGE + PARITY + EDUCATION + LSI + medSEMUAC + PEHCIGAR, 
  data = subset(df, VISIT4 == 1)))

