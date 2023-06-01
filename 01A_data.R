################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Drinking Water Arsenic and Hemoglobin -- Data

# Tyler Smith
# May 29, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(lubridate)

# Set Working Directory
setwd("~/Johns Hopkins/PAIR Data - Documents/Data/Current/")

##### Read Data ################################################################
pregtrak <- read_csv("j7pregtrak/pair_pregtrak_2022_0309.csv")
kidtrak  <- read_csv("j7kidtrak/pair_kidtrak_2022_0310.csv")
water_pe <- read_csv("assay_water_metals/pair_watermetals_pef_2022_1030.csv")
water_vx <- read_csv("assay_water_metals/pair_watermetals_vaxf_2022_1030.csv")
pefsst   <- read_csv("pefsst/pair_pefsst_2022_0310.csv")
vaxfsst  <- read_csv("vaxfsst/pair_vaxfsst_2022_0310.csv")
mdab     <- read_csv("mdab/pair_mdab_2022_0310.csv")
m3mopsst <- read_csv("m3mopsst/pair_m3mopsst_2022_0310.csv")
parity   <- read_csv("pair_reprohistory/pair_reprohistory_2022_0328.csv")
ses      <- read_csv("ses/pair_ses_2022_0310.csv")
pef      <- read_csv("pef/pair_pef_2022_0310.csv") 
ferritin <- read_csv("assay_ocm/pair_ocm_2023_0328.csv")

##### Select Variables #########################################################
# J7PREGTRAK
pregtrak <- pregtrak %>%
  filter(PEF == 1 & PEFSST == 1) %>%
  select(
    UID,
    DOBYY,
    BGLMPWK
  )

# J7KIDTRAK
kidtrak <- kidtrak %>%
  select(
    UID = MOMUID, 
    CHILDUID, 
    CHILDDOB
  )

# Drinking Water Elements
water_pe <- water_pe %>%
  select(
    UID, 
    PE_wMetals_As, 
    PE_wMetals_Fe
  )

water_pe <- water_pe %>%
  mutate(UID = as.numeric(UID))

water_vx <- water_vx %>%
  select(
    UID, 
    VX_wMetals_As, 
    VX_wMetals_Fe
  )

water_vx <- water_vx %>%
  mutate(UID = as.numeric(UID))

# PEFSST
pefsst <- pefsst %>%
  select(
    UID,
    SESTATUS,
    SEDATE,
    SEWKINT, 
    SEHEMO, 
    SEFETABS,
    medSEMUAC
  )

# VAXFSST
vaxfsst <- vaxfsst %>%
  select(
    UID,
    SVXSTATUS,
    SVXDATE,
    SVXWKINT,
    SVXHEMO,
    SVXFETABS
  )

# MDAB
mdab <- mdab %>%
  select(
    UID,
    SMSTATUS,
    SMDATE,
    SMWKINT,
    SMHEMO,
    SMFETABS
  )

# M3MOPSST
m3mopsst <- m3mopsst %>%
  select(
    UID,
    SM3STATUS,
    SM3DATE,
    SM3WKINT,
    SM3HEMO,
    SM3FETABS
  )

# Parity
parity <- parity %>%
  select(
    UID,
    PARITY = FDPSR_PARITY
  )

# SES
ses <- ses %>%
  select(
    UID,
    EDUCATION = wehclass_mc2,
    LSI = lsi
  )

# PEF
pef <- pef %>%
  select(
    UID,
    PEHCIGAR
  )

# Plasma Ferritin
ferritin <- ferritin %>%
  select(UID, SEFER)

ferritin <- ferritin %>%
  mutate(UID = as.numeric(UID))

##### Join Data ################################################################
df <- left_join(pregtrak, kidtrak, by = "UID")
df <- left_join(df, water_pe, by = "UID")
df <- left_join(df, water_vx, by = "UID")
df <- left_join(df, pefsst, by = "UID")
df <- left_join(df, vaxfsst, by = "UID")
df <- left_join(df, mdab, by = "UID")
df <- left_join(df, m3mopsst, by = "UID")
df <- left_join(df, parity, by = "UID")
df <- left_join(df, ses, by = "UID")
df <- left_join(df, pef, by = "UID")
df <- left_join(df, ferritin, by = "UID")

df %>% head()

# Remove Data Objects
rm(list = ls()[!grepl("df",ls())])

##### Limit to 1 Row/Woman #####################################################
# Indicate Live Birth
df <- df %>%
  mutate(LIVEBIRTH = ifelse(!is.na(CHILDUID), 1, 0))

# Indicate Singleton Live Birth
df <- df %>%
  group_by(UID) %>%
  mutate(
    SINGLETON = 
      ifelse(LIVEBIRTH == 1 & n() == 1, 1,
      ifelse(LIVEBIRTH == 1 & n() != 1, 0,
      ifelse(LIVEBIRTH != 1, NA, NA)))) %>%
  ungroup()
    
df %>%
  group_by(LIVEBIRTH) %>%
  count(SINGLETON)

# Reduce to 1 Row/Pregnant Woman
df <- df %>%
  group_by(UID) %>%
  arrange(UID, CHILDDOB) %>%
  slice_head() %>%
  ungroup()

df %>% head()
df %>% nrow()

##### Prepare Hemoglobin #######################################################
# Convert to Numeric
df %>%
  select(UID, ends_with("HEMO")) %>%
  pivot_longer(cols = -UID) %>%
  na.omit() %>%
  mutate(value = as.numeric(value)) %>%
  filter(is.na(value))

df %>%
  select(UID, ends_with("HEMO")) %>%
  filter(UID == 756671 | UID == 846363)

df <- df %>%
  mutate(across(ends_with("HEMO"), ~ as.numeric(.x)))

# Standardize Missing Values
df %>%
  select(ends_with("HEMO")) %>%
  pivot_longer(cols = everything()) %>%
  group_by(name) %>%
  count(value == 99.9)

df <- df %>%
  mutate(SMHEMO  = ifelse(SMHEMO  == 99.9, NA, SMHEMO)) %>%
  mutate(SM3HEMO = ifelse(SM3HEMO == 99.9, NA, SM3HEMO))

# Set Erroneous Hemoglobin Value to Missing
df %>%
  select(ends_with("HEMO")) %>%
  pivot_longer(cols = everything()) %>%
  filter(value > 16)

df %>%
  select(ends_with("HEMO")) %>%
  pivot_longer(cols = everything()) %>%
  filter(value > 15)

df <- df %>%
  mutate(SEHEMO = ifelse(SEHEMO > 16, NA, SEHEMO))

# Drop if Given Iron Supplements
df %>% count(SEFETABS)
df %>% count(SVXFETABS)
df %>% count(SMFETABS)
df %>% count(SM3FETABS)

# (Iron Supplements at Visit 1/PEF)
sefetabs_uid <- df %>%
  filter(SEFETABS == 1) %>%
  pull(UID)

df <- df %>%
  mutate(across(c(SVXHEMO,SMHEMO,SM3HEMO), ~ ifelse(UID == sefetabs_uid, NA, .x)))

df %>% count(is.na(SVXHEMO))

# (Iron Supplements at Visit 3/MDAB)
smfetabs_uid <- df %>%
  filter(SMFETABS == 1) %>%
  pull(UID)

df <- df %>%
  mutate(across(c(SM3HEMO), ~ ifelse(UID %in% smfetabs_uid, NA, .x)))

df %>% count(is.na(SM3HEMO))

rm(list = c("sefetabs_uid","smfetabs_uid"))

##### Prepare Drinking Water Elements ##########################################
# (Use Visit 2 Values if Visit 1 Values Missing or Erroneous)
df <- df %>%
  mutate(wAs = ifelse(is.na(PE_wMetals_As), VX_wMetals_As, PE_wMetals_As)) %>%
  mutate(wFe = ifelse(is.na(PE_wMetals_Fe), VX_wMetals_Fe, PE_wMetals_Fe)) %>%
  mutate(wFe = ifelse(wFe > 284000, VX_wMetals_Fe, wFe))

df %>% count(is.na(wAs))
df %>% count(is.na(wFe))

df <- df %>%
  mutate(ln_wAs = log(wAs)) %>%
  mutate(ln_wFe = log(wFe))

##### Age ######################################################################
# Derive Age
df <- df %>%
  mutate(AGE = year(SEDATE) - DOBYY)

# Check Missingness
df %>%
  count(is.na(AGE)) %>%
  mutate(pr = n / sum(n) * 100)

# Check Range
df %>%
  summarise(
    n = sum(!is.na(AGE)),
    min = min(AGE, na.rm = TRUE),
    max = max(AGE, na.rm = TRUE)
  )

# Plot Distribution
df %>%
  filter(!is.na(AGE)) %>%
  ggplot(aes(x = AGE)) +
  geom_density() +
  scale_x_continuous(breaks = seq(10,50,5), limits = c(10,45)) +
  scale_y_continuous(limits = c(0.00,0.08)) +
  labs(
    title = "Maternal Age at Visit 1",
    x = "Maternal Age (years)",
    y = "Density") +
  theme_bw()

##### Gestational Age (Visits 1-2) #############################################
# Derive Gestational Age
df <- df %>%
  mutate(SEGSTAGE  = SEWKINT  - BGLMPWK) %>%
  mutate(SVXGSTAGE = SVXWKINT - BGLMPWK)

# Check Range
# (Visit 1)
df %>%
  count(SEGSTAGE) %>%
  mutate(pr = n / sum(n))

# (Visit 2)
df %>%
  count(SVXGSTAGE) %>%
  mutate(pr = n / sum(n))

##### Days Postpartum (Visits 3-4) #############################################
# Derive Days Postpartum
df <- df %>%
  mutate(SMDAYSPP  = as.numeric(SMDATE  - CHILDDOB)) %>%
  mutate(SM3DAYSPP = as.numeric(SM3DATE - CHILDDOB))

# Plot Distributions
df %>%
  select(SMDAYSPP, SM3DAYSPP) %>%
  pivot_longer(cols = everything()) %>%
  mutate(name = factor(name, levels = c("SMDAYSPP","SM3DAYSPP"), 
    labels = c("Visit 3","Visit 4"))) %>%
  ggplot(aes(x = value, y = fct_rev(name), group = name)) +
  geom_jitter(alpha = 0.4, height = 0.2) +
  scale_x_continuous(breaks = seq(0,180,30)) +
  labs(
    title = "Days Postpartum at Visits 3-4",
    x = "Days Postpartum",
    y = "Visit") +
  theme_bw()

##### Parity ###################################################################
# Categorize Parity
df <- df %>%
  mutate(PARITY = ifelse(PARITY > 2, 2, PARITY))

# Define Factors
df <- df %>%
  mutate(PARITY = factor(PARITY,
    levels = c(0,1,2),
    labels = c("Nulliparous","Primiparous","Multiparous")
  ))

# Check Distribution
df %>%
  count(PARITY) %>%
  mutate(pr = n / sum(n) * 100)

##### Education ################################################################
# Categorize Education
df <- df %>%
  mutate(EDUCATION = ifelse(EDUCATION > 2, 2, EDUCATION))

# Define Factors
df <- df %>%
  mutate(EDUCATION = factor(EDUCATION,
    levels = c(0,1,2),
    labels = c("None","Class 1-9","Class ≥10")
  ))

# Check Distribution
df %>%
  count(EDUCATION) %>%
  mutate(pr = n / sum(n) * 100)

##### Living Standards Index ###################################################
# Plot Distribution
df %>%
  ggplot(aes(x = LSI)) +
  geom_density() +
  scale_y_continuous(limits = c(0,0.5)) +
  labs(
    title = "Living Standards Index",
    x = "Living Standards Index",
    y = "Density") +
  theme_bw()

##### Mid-upper Arm Circumference ##############################################
# Plot Distribution
df %>%
  ggplot(aes(x = medSEMUAC)) +
  geom_density() +
  scale_x_continuous(limits = c(10,40), breaks = seq(10,40,5)) +
  labs(
    title = "Mid-upper Arm Circumference at Visit 1",
    x = "Mid-upper Arm Circumference (cm)",
    y = "Density") +
  theme_bw()

# Plot MUAC by LSI
df %>%
  ggplot(aes(x = LSI, y = medSEMUAC)) +
  geom_point(alpha = 0.4) +
  geom_smooth() +
  scale_x_continuous(limits = c(-2,3)) +
  scale_y_continuous(limits = c(10,40)) +
  labs(
    x = "Living Standards Index",
    y = "Mid-upper Arm Circumference (cm)") +
  theme_bw()

##### Husband's Smoking ########################################################
# Check Distribution
df %>%
  count(PEHCIGAR) %>%
  mutate(pr = n / sum(n) * 100)

# Define Factors
df <- df %>%
  mutate(PEHCIGAR = factor(PEHCIGAR,
    levels = c(0,1),
    labels = c("No","Yes")
  ))

##### Plasma Ferritin ##########################################################
# Transform Plasma Ferritin
df <- df %>%
  mutate(ln_SEFER = log(SEFER))

# Plot Distribution
df %>%
  ggplot(aes(x = ln_SEFER)) +
  geom_density() +
  scale_x_continuous(limits = c(1,7), breaks = seq(1,7,1)) +
  scale_y_continuous(limits = c(0.0,0.6)) +
  labs(
    title = "Plasma Ferritin at Visit 1",
    x = "Plasma Ferritin (ln ng/mL)",
    y = "Density") +
  theme_bw()

##### Prepare Data Set #########################################################
# Select Variables
df <- df %>%
  select(
    # Identifiers and Selection
    UID, LIVEBIRTH, SINGLETON,
    
    # Hemoglobin
    ends_with("HEMO"), ends_with("FETABS"),
    
    # Drinking Water Elements
    contains("wAs"), contains("wFe"),
    
    # Other Covariates
    AGE, SEGSTAGE, SVXGSTAGE, SMDAYSPP, SM3DAYSPP, PARITY, EDUCATION, 
    LSI, medSEMUAC, PEHCIGAR, contains("SEFER")
  )

df %>% head()


