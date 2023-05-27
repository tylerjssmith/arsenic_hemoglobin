################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Drinking Water Arsenic and Hemoglobin -- Sensitivity Analysis

# Tyler Smith
# January 24, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(broom)
library(patchwork)
library(car)

##### Prepare Data #############################################################
# Subset Data
tmp_df <- df %>%
  filter(VISIT1 == 1) %>%
  mutate(VISIT4 = factor(VISIT4, levels = c(0,1), labels = c("No","Yes")))

##### Compare Baseline Values ##################################################
# Drinking Water Arsenic
(fig_as <- tmp_df %>%
  ggplot(aes(x = factor(VISIT4), y = ln_wAs, group = VISIT4)) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq(-20,20,1)) +
  labs(
    title = "Drinking Water Arsenic",
    x = "Included at Visit 4?",
    y = "Drinking Water Arsenic (ln µg/L)") +
  th)

tidy(lm(ln_wAs ~ VISIT4, data = tmp_df), 
  conf.int = TRUE)

tidy(glm(VISIT4 ~ ln_wAs_IQR, data = tmp_df, family = "binomial"), 
  conf.int = TRUE, exponentiate = TRUE)

# Drinking Water Iron
(fig_fe <- tmp_df %>%
  ggplot(aes(x = VISIT4, y = ln_wFe, group = VISIT4)) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq(-20,20,1)) +
  labs(
    title = "Drinking Water Iron",
    x = "Included at Visit 4?",
    y = "Drinking Water Iron (ln µg/L)") +
  th)

tidy(lm(ln_wFe ~ VISIT4, data = tmp_df), 
  conf.int = TRUE)

tidy(glm(VISIT4 ~ ln_wFe_IQR, data = tmp_df, family = "binomial"), 
  conf.int = TRUE, exponentiate = TRUE)

# Hemoglobin
(fig_hb <- tmp_df %>%
  ggplot(aes(x = VISIT4, y = SEHEMO, group = VISIT4)) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq(-20,20,1)) +
  labs(
    title = "Hemoglobin",
    x = "Included at Visit 4?",
    y = "Hemoglobin (g/dL)") +
  th)

tidy(lm(SEHEMO ~ VISIT4, data = tmp_df), 
  conf.int = TRUE)

tidy(glm(VISIT4 ~ SEHEMO, data = tmp_df, family = "binomial"), 
  conf.int = TRUE, exponentiate = TRUE)

# (Exclude Two Influential Observations)
tidy(lm(SEHEMO ~ VISIT4, data = subset(tmp_df, SEHEMO >= 8)), conf.int = TRUE)

tidy(glm(VISIT4 ~ SEHEMO, data = subset(tmp_df, SEHEMO >= 8), family = "binomial"), 
  conf.int = TRUE, exponentiate = TRUE)

# Combine Plots
fig_as + fig_fe + fig_hb +
  plot_annotation(title = "Drinking Water Elements and Hemoglobin at Visit 1")

##### Compare Baseline Association #############################################
model_v1_adj_v4no  <- lm(SEHEMO ~ ln_wAs_IQR + ln_wFe_IQR + AGE + SEGSTAGE + 
    PARITY + EDUCATION + LSI + medSEMUAC + PEHCIGAR, 
  data = subset(tmp_df, VISIT4 == "No"))
model_v1_adj_v4yes <- lm(SEHEMO ~ ln_wAs_IQR + ln_wFe_IQR + AGE + SEGSTAGE + 
    PARITY + EDUCATION + LSI + medSEMUAC + PEHCIGAR, 
  data = subset(tmp_df, VISIT4 == "Yes"))

tidy(model_v1_adj_v4no, conf.int = TRUE)
tidy(model_v1_adj_v4yes, conf.int = TRUE)

nobs(model_v1_adj_v4no)
nobs(model_v1_adj_v4yes)



