################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Drinking Water Arsenic and Hemoglobin -- Explore

# Tyler Smith
# January 11, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Summarize Gestational Ages Visits 1-2 ####################################
# Visit 1
df %>%
  filter(VISIT1 == 1) %>%
  summarise(
    n = n(),
    median = median(SEGSTAGE),
    q1 = quantile(SEGSTAGE, 0.25),
    q3 = quantile(SEGSTAGE, 0.75)
  )

# Visit 2
df %>%
  filter(VISIT2 == 1) %>%
  summarise(
    n = n(),
    median = median(SVXGSTAGE),
    q1 = quantile(SVXGSTAGE, 0.25),
    q3 = quantile(SVXGSTAGE, 0.75)
  )

##### Obtain Sample Sizes ######################################################
# Singleton Live Births
df %>% filter(LIVEBIRTH == 1 & SINGLETON == 1) %>% nrow()

# Singleton Live Births and Complete Data
df %>% filter(VISIT1 == 1) %>% nrow()
df %>% filter(VISIT2 == 1) %>% nrow()
df %>% filter(VISIT3 == 1) %>% nrow()
df %>% filter(VISIT4 == 1) %>% nrow()

##### Summarize Drinking Water Elements ########################################
# Drinking Water Arsenic (w-As)
df %>%
  filter(VISIT1 == 1) %>%
  summarise(
    med = median(wAs),
    q1  = quantile(wAs, 0.25),
    q3  = quantile(wAs, 0.75)
  )

# Drinking Water Iron (w-Fe)
df %>%
  filter(VISIT1 == 1) %>%
  summarise(
    med = median(wFe),
    q1  = quantile(wFe, 0.25),
    q3  = quantile(wFe, 0.75)
  )

# Pearson Correlation
df %>%
  filter(VISIT1 == 1) %>%
  select(wAs, wFe) %>%
  cor(method = "pearson")

# Spearman's Correlation
df %>%
  filter(VISIT1 == 1) %>%
  select(wAs, wFe) %>%
  cor(method = "spearman")

##### Hemoglobin ###############################################################
# Visit 1
df %>%
  filter(VISIT1 == 1) %>%
  summarise(
    med = median(SEHEMO),
    q1  = quantile(SEHEMO, 0.25),
    q3  = quantile(SEHEMO, 0.75)
  )

# Visit 2
df %>%
  filter(VISIT2 == 1) %>%
  summarise(
    med = median(SVXHEMO),
    q1  = quantile(SVXHEMO, 0.25),
    q3  = quantile(SVXHEMO, 0.75)
  )

# Visit 3
df %>%
  filter(VISIT3 == 1) %>%
  summarise(
    med = median(SMHEMO),
    q1  = quantile(SMHEMO, 0.25),
    q3  = quantile(SMHEMO, 0.75)
  )

# Visit 4
df %>%
  filter(VISIT4 == 1) %>%
  summarise(
    med = median(SM3HEMO),
    q1  = quantile(SM3HEMO, 0.25),
    q3  = quantile(SM3HEMO, 0.75)
  )



