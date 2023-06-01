################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Drinking Water Arsenic and Hemoglobin -- Explore

# Tyler Smith
# May 29, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(table1)

##### Labels and Units #########################################################
# Singleton Live Births
# (Time Fixed)
label(df_slb$AGE)        <- "Age"
label(df_slb$PARITY)     <- "Parity"
label(df_slb$EDUCATION)  <- "Education"
label(df_slb$LSI)        <- "Living Standards Index"
label(df_slb$medSEMUAC)  <- "Mid-upper Arm Circumference"
label(df_slb$PEHCIGAR)   <- "Husband Smokes"
label(df_slb$wAs)        <- "Drinking Water Arsenic"
label(df_slb$wFe)        <- "Drinking Water Iron"

units(df_slb$AGE)        <- "years"
units(df_slb$medSEMUAC)  <- "cm"
units(df_slb$wAs)        <- "µg/L"
units(df_slb$wFe)        <- "µg/L"

# (Time Varying)
label(df_slb$SEHEMO)     <- "Hemoglobin at Visit"
label(df_slb$SVXHEMO)    <- "Hemoglobin at Visit"
label(df_slb$SMHEMO)     <- "Hemoglobin at Visit"
label(df_slb$SM3HEMO)    <- "Hemoglobin at Visit"

label(df_slb$SEGSTAGE)   <- "Gestational Age at Visit"
label(df_slb$SVXGSTAGE)  <- "Gestational Age at Visit"
label(df_slb$SMDAYSPP)   <- "Days Postpartum at Visit"
label(df_slb$SM3DAYSPP)  <- "Days Postpartum at Visit"

units(df_slb$SEHEMO)     <- "g/dL"
units(df_slb$SVXHEMO)    <- "g/dL"
units(df_slb$SMHEMO)     <- "g/dL"
units(df_slb$SM3HEMO)    <- "g/dL"

units(df_slb$SEGSTAGE)   <- "weeks"
units(df_slb$SVXGSTAGE)  <- "weeks"
units(df_slb$SMDAYSPP)   <- "days"
units(df_slb$SM3DAYSPP)  <- "days"

##### Generate Tables ##########################################################
# Singleton Live Births
table1(~ AGE + PARITY + EDUCATION + LSI + medSEMUAC + PEHCIGAR + wAs + wFe + SEGSTAGE + SEHEMO, 
  data = df_slb %>% filter(VISIT1 == 1), overall = TRUE, 
  render.continuous = c("Median (IQR)" = "MEDIAN (Q1, Q3)"))

table1(~ AGE + PARITY + EDUCATION + LSI + medSEMUAC + PEHCIGAR + wAs + wFe + SVXGSTAGE + SVXHEMO, 
  data = df_slb %>% filter(VISIT2 == 1), overall = TRUE, 
  render.continuous = c("Median (IQR)" = "MEDIAN (Q1, Q3)"))

table1(~ AGE + PARITY + EDUCATION + LSI + medSEMUAC + PEHCIGAR + wAs + wFe + SMDAYSPP + SMHEMO, 
  data = df_slb %>% filter(VISIT3 == 1), overall = TRUE, 
  render.continuous = c("Median (IQR)" = "MEDIAN (Q1, Q3)"))

table1(~ AGE + PARITY + EDUCATION + LSI + medSEMUAC + PEHCIGAR + wAs + wFe + SM3DAYSPP + SM3HEMO, 
  data = df_slb %>% filter(VISIT4 == 1), overall = TRUE, 
  render.continuous = c("Median (IQR)" = "MEDIAN (Q1, Q3)"))

##### Summary Statistics #######################################################
# Table 1
df_slb %>%
  summarise(
    median = median(wAs),
    q1 = quantile(wAs, 0.25),
    q3 = quantile(wAs, 0.75)
  )

df_slb %>%
  summarise(
    median = median(wFe),
    q1 = quantile(wFe, 0.25),
    q3 = quantile(wFe, 0.75)
  )

# Table 2
df_slb %>%
  filter(VISIT1 == 1) %>%
  summarise(
    n = n(),
    median = median(SEHEMO),
    q1 = quantile(SEHEMO, 0.25),
    q3 = quantile(SEHEMO, 0.75)
  )

df_slb %>%
  filter(VISIT2 == 1) %>%
  summarise(
    n = n(),
    median = median(SVXHEMO),
    q1 = quantile(SVXHEMO, 0.25),
    q3 = quantile(SVXHEMO, 0.75)
  )

df_slb %>%
  filter(VISIT3 == 1) %>%
  summarise(
    n = n(),
    median = median(SMHEMO),
    q1 = quantile(SMHEMO, 0.25),
    q3 = quantile(SMHEMO, 0.75)
  )

df_slb %>%
  filter(VISIT4 == 1) %>%
  summarise(
    n = n(),
    median = median(SM3HEMO),
    q1 = quantile(SM3HEMO, 0.25),
    q3 = quantile(SM3HEMO, 0.75)
  )

# Table 3
df_slb %>%
  filter(VISIT3 == 1) %>%
  group_by(SMDAYSPP7) %>%
  summarise(
    n = n(),
    median = median(SMHEMO),
    q1 = quantile(SMHEMO, 0.25),
    q3 = quantile(SMHEMO, 0.75)
  )



