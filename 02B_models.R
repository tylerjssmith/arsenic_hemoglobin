################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Drinking Water Arsenic and Hemoglobin -- Models

# Tyler Smith
# May 29, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(broom)
library(sandwich)

##### Function: Tidy Model Output with Selection Weights #######################
tidier_robust <- function(model, visit, set, weights = "Unweighted", stratum = "All") {
  require(sandwich)
  
  out <- tibble(
    term = names(coef(model)),
    estimate = coef(model),
    std.error = sqrt(diag(sandwich(model))),
    visit = visit,
    set = set,
    weights = weights,
    stratum = stratum
  )
  
  out <- out %>%
    filter(grepl("wAs", term) | grepl("wFe", term)) %>%
    mutate(conf.low  = estimate - 1.96 * std.error) %>%
    mutate(conf.high = estimate + 1.96 * std.error) %>%
    select(stratum, visit, term, set, weights, estimate, 
      conf.low, conf.high)
  
  return(out)
}

##### Calculate Selection Weights: Singleton Live Births #######################
# Models: Marginal Probabilities
fit_mpoe_v2 <- glm(VISIT2 ~ 1, data = df_slb, family = binomial)
fit_mpoe_v3 <- glm(VISIT3 ~ 1, data = df_slb, family = binomial)
fit_mpoe_v4 <- glm(VISIT4 ~ 1, data = df_slb, family = binomial)

# Models: Conditional Probabilities
fit_cpoe_v2 <- glm(VISIT2 ~ ln_wAs_IQR + ln_wFe_IQR + AGE + SEGSTAGE + PARITY + 
    EDUCATION + LSI + medSEMUAC + PEHCIGAR, data = df_slb, family = binomial)
fit_cpoe_v3 <- glm(VISIT3 ~ ln_wAs_IQR + ln_wFe_IQR + AGE + SEGSTAGE + PARITY + 
    EDUCATION + LSI + medSEMUAC + PEHCIGAR, data = df_slb, family = binomial)
fit_cpoe_v4 <- glm(VISIT4 ~ ln_wAs_IQR + ln_wFe_IQR + AGE + SEGSTAGE + PARITY + 
    EDUCATION + LSI + medSEMUAC + PEHCIGAR, data = df_slb, family = binomial)

tidy(fit_cpoe_v2, conf.int = TRUE, exponentiate = TRUE)
tidy(fit_cpoe_v3, conf.int = TRUE, exponentiate = TRUE)
tidy(fit_cpoe_v4, conf.int = TRUE, exponentiate = TRUE)

# Calculate Stabilized Weights
df_slb$wt_v2 <- fitted(fit_mpoe_v2) / fitted(fit_cpoe_v2)
df_slb$wt_v3 <- fitted(fit_mpoe_v3) / fitted(fit_cpoe_v3)
df_slb$wt_v4 <- fitted(fit_mpoe_v4) / fitted(fit_cpoe_v4)

# Check Weights
sum(df_slb$wt_v2)
sum(df_slb$wt_v3)
sum(df_slb$wt_v4)

boxplot(df_slb$wt_v2)
boxplot(df_slb$wt_v3)
boxplot(df_slb$wt_v4)

with(df_slb, scatter.smooth(ln_wAs_IQR, wt_v2))
with(df_slb, scatter.smooth(ln_wAs_IQR, wt_v3))
with(df_slb, scatter.smooth(ln_wAs_IQR, wt_v4))

with(df_slb, scatter.smooth(ln_wFe_IQR, wt_v2))
with(df_slb, scatter.smooth(ln_wFe_IQR, wt_v3))
with(df_slb, scatter.smooth(ln_wFe_IQR, wt_v4))

# Set Weights to 0 for Non-Selected Participants
df_slb$wt_v2 <- ifelse(df_slb$VISIT2 == 1, df_slb$wt_v2, 0)
df_slb$wt_v3 <- ifelse(df_slb$VISIT3 == 1, df_slb$wt_v3, 0)
df_slb$wt_v4 <- ifelse(df_slb$VISIT4 == 1, df_slb$wt_v4, 0)

##### Fit Models: Singleton Live Births ########################################
# Visit 1: Unweighted
summary(fit_slb_v1_wAs <- lm(SEHEMO ~ ln_wAs_IQR, 
  data = subset(df_slb, VISIT1 == 1)))
summary(fit_slb_v1_wFe <- lm(SEHEMO ~ ln_wFe_IQR, 
  data = subset(df_slb, VISIT1 == 1)))
summary(fit_slb_v1_adj <- lm(SEHEMO ~ ln_wAs_IQR + ln_wFe_IQR + AGE + SEGSTAGE + 
    PARITY + EDUCATION + LSI + medSEMUAC + PEHCIGAR, 
  data = subset(df_slb, VISIT1 == 1)))

nobs(fit_slb_v1_wAs)
nobs(fit_slb_v1_wFe)
nobs(fit_slb_v1_adj)

# Visit 2: Weighted
summary(fit_slb_v2_wAs_wt <- lm(SVXHEMO ~ ln_wAs_IQR,
  data = subset(df_slb, VISIT2 == 1), weights = wt_v2))
summary(fit_slb_v2_wFe_wt <- lm(SVXHEMO ~ ln_wFe_IQR,
  data = subset(df_slb, VISIT2 == 1), weights = wt_v2))
summary(fit_slb_v2_adj_wt <- lm(SVXHEMO ~ ln_wAs_IQR + ln_wFe_IQR + AGE + 
    SVXGSTAGE + PARITY + EDUCATION + LSI + medSEMUAC + PEHCIGAR, 
  data = subset(df_slb, VISIT2 == 1), weights = wt_v2))

nobs(fit_slb_v2_wAs_wt)
nobs(fit_slb_v2_wFe_wt)
nobs(fit_slb_v2_adj_wt)

sum(df_slb$wt_v2)

# Visit 3: Weighted
summary(fit_slb_v3_wAs_wt <- lm(SMHEMO ~ ln_wAs_IQR,
  data = subset(df_slb, VISIT3 == 1), weights = wt_v3))
summary(fit_slb_v3_wFe_wt <- lm(SMHEMO ~ ln_wFe_IQR,
  data = subset(df_slb, VISIT3 == 1), weights = wt_v3))
summary(fit_slb_v3_adj_wt <- lm(SMHEMO ~ ln_wAs_IQR + ln_wFe_IQR + AGE + 
    PARITY + EDUCATION + LSI + medSEMUAC + PEHCIGAR, 
  data = subset(df_slb, VISIT3 == 1), weights = wt_v3))

nobs(fit_slb_v3_wAs_wt)
nobs(fit_slb_v3_wFe_wt)
nobs(fit_slb_v3_adj_wt)

sum(df_slb$wt_v3)

# Visit 4: Weighted
summary(fit_slb_v4_wAs_wt <- lm(SM3HEMO ~ ln_wAs_IQR,
  data = subset(df_slb, VISIT4 == 1), weights = wt_v4))
summary(fit_slb_v4_wFe_wt <- lm(SM3HEMO ~ ln_wFe_IQR,
  data = subset(df_slb, VISIT4 == 1), weights = wt_v4))
summary(fit_slb_v4_adj_wt <- lm(SM3HEMO ~ ln_wAs_IQR + ln_wFe_IQR + AGE + 
    PARITY + EDUCATION + LSI + medSEMUAC + PEHCIGAR, 
  data = subset(df_slb, VISIT4 == 1), weights = wt_v4))

nobs(fit_slb_v4_wAs_wt)
nobs(fit_slb_v4_wFe_wt)
nobs(fit_slb_v4_adj_wt)

sum(df_slb$wt_v4)

##### Fit Models: Singleton Live Births at Visit 3 by Days Postpartum ##########
# ≤7 Days Postpartum: Weighted
summary(fit_slb_v3_wAs_wt_pp0 <- lm(SMHEMO ~ ln_wAs_IQR,
  data = subset(df_slb, VISIT3 == 1 & SMDAYSPP7 == 0), weights = wt_v3))
summary(fit_slb_v3_wFe_wt_pp0 <- lm(SMHEMO ~ ln_wFe_IQR,
  data = subset(df_slb, VISIT3 == 1 & SMDAYSPP7 == 0), weights = wt_v3))
summary(fit_slb_v3_adj_wt_pp0 <- lm(SMHEMO ~ ln_wAs_IQR + ln_wFe_IQR + AGE + 
    PARITY + EDUCATION + LSI + medSEMUAC + PEHCIGAR, 
  data = subset(df_slb, VISIT3 == 1 & SMDAYSPP7 == 0), weights = wt_v3))

# >7 Days Postpartum: Weighted
summary(fit_slb_v3_wAs_wt_pp1 <- lm(SMHEMO ~ ln_wAs_IQR,
  data = subset(df_slb, VISIT3 == 1 & SMDAYSPP7 == 1), weights = wt_v3))
summary(fit_slb_v3_wFe_wt_pp1 <- lm(SMHEMO ~ ln_wFe_IQR,
  data = subset(df_slb, VISIT3 == 1 & SMDAYSPP7 == 1), weights = wt_v3))
summary(fit_slb_v3_adj_wt_pp1 <- lm(SMHEMO ~ ln_wAs_IQR + ln_wFe_IQR + AGE + 
    PARITY + EDUCATION + LSI + medSEMUAC + PEHCIGAR, 
  data = subset(df_slb, VISIT3 == 1 & SMDAYSPP7 == 1), weights = wt_v3))

nobs(fit_slb_v3_wAs_wt_pp0)
nobs(fit_slb_v3_wFe_wt_pp0)
nobs(fit_slb_v3_adj_wt_pp0)

nobs(fit_slb_v3_wAs_wt_pp1)
nobs(fit_slb_v3_wFe_wt_pp1)
nobs(fit_slb_v3_adj_wt_pp1)

sum(df_slb$wt_v3[df_slb$VISIT3 == 1 & df_slb$SMDAYSPP7 == 0])
sum(df_slb$wt_v3[df_slb$VISIT3 == 1 & df_slb$SMDAYSPP7 == 1])

##### Compile Estimates: Singleton Live Births #################################
# Visits 1-4
est_slb_wt <- rbind(
  # Visit 1
  tidier_robust(fit_slb_v1_wAs, visit = "Visit 1", set = "Unadjusted", 
    weights = "Unweighted", stratum = "All"),
  tidier_robust(fit_slb_v1_wFe, visit = "Visit 1", set = "Unadjusted", 
    weights = "Unweighted", stratum = "All"),
  tidier_robust(fit_slb_v1_adj, visit = "Visit 1", set = "Adjusted", 
    weights = "Unweighted", stratum = "All"),
  
  # Visit 2
  tidier_robust(fit_slb_v2_wAs_wt, visit = "Visit 2", set = "Unadjusted", 
    weights = "Weighted", stratum = "All"),
  tidier_robust(fit_slb_v2_wFe_wt, visit = "Visit 2", set = "Unadjusted", 
    weights = "Weighted", stratum = "All"),
  tidier_robust(fit_slb_v2_adj_wt, visit = "Visit 2", set = "Adjusted", 
    weights = "Weighted", stratum = "All"),

  # Visit 3
  tidier_robust(fit_slb_v3_wAs_wt, visit = "Visit 3", set = "Unadjusted", 
    weights = "Weighted", stratum = "All"),
  tidier_robust(fit_slb_v3_wFe_wt, visit = "Visit 3", set = "Unadjusted", 
    weights = "Weighted", stratum = "All"),
  tidier_robust(fit_slb_v3_adj_wt, visit = "Visit 3", set = "Adjusted", 
    weights = "Weighted", stratum = "All"),

  # Visit 4
  tidier_robust(fit_slb_v4_wAs_wt, visit = "Visit 4", set = "Unadjusted", 
    weights = "Weighted", stratum = "All"),
  tidier_robust(fit_slb_v4_wFe_wt, visit = "Visit 4", set = "Unadjusted", 
    weights = "Weighted", stratum = "All"),
  tidier_robust(fit_slb_v4_adj_wt, visit = "Visit 4", set = "Adjusted", 
    weights = "Weighted", stratum = "All")
)

# Format Estimates
est_slb_wt <- est_slb_wt %>%
  mutate(visit = factor(visit,
    levels = c("Visit 1","Visit 2","Visit 3","Visit 4")
  ))

est_slb_wt <- est_slb_wt %>%
  mutate(term = factor(term,
    levels = c("ln_wAs_IQR","ln_wFe_IQR"),
    labels = c("wAs","wFe")
  ))

est_slb_wt <- est_slb_wt %>%
  mutate(set = factor(set,
    levels = c("Unadjusted","Adjusted")
  ))

est_slb_wt %>% head()

# Visit 3 by Days Postpartum
est_slb_wt_v3 <- rbind(
  # ≤7 Days Postpartum
  tidier_robust(fit_slb_v3_wAs_wt_pp0, visit = "Visit 3", set = "Unadjusted", 
    weights = "Weighted", stratum = "≤7 Days Postpartum"),
  tidier_robust(fit_slb_v3_wFe_wt_pp0, visit = "Visit 3", set = "Unadjusted", 
    weights = "Weighted", stratum = "≤7 Days Postpartum"),
  tidier_robust(fit_slb_v3_adj_wt_pp0, visit = "Visit 3", set = "Adjusted", 
    weights = "Weighted", stratum = "≤7 Days Postpartum"),
  
  # >7 Days Postpartum
  tidier_robust(fit_slb_v3_wAs_wt_pp1, visit = "Visit 3", set = "Unadjusted", 
    weights = "Weighted", stratum = ">7 Days Postpartum"),
  tidier_robust(fit_slb_v3_wFe_wt_pp1, visit = "Visit 3", set = "Unadjusted", 
    weights = "Weighted", stratum = ">7 Days Postpartum"),
  tidier_robust(fit_slb_v3_adj_wt_pp1, visit = "Visit 3", set = "Adjusted", 
    weights = "Weighted", stratum = ">7 Days Postpartum")
)

est_slb_wt_v3 %>% head()

# Format Estimates
est_slb_wt_v3 <- est_slb_wt_v3 %>%
  mutate(stratum = factor(stratum,
    levels = c("≤7 Days Postpartum",">7 Days Postpartum")
  ))

est_slb_wt_v3 <- est_slb_wt_v3 %>%
  mutate(term = factor(term,
    levels = c("ln_wAs_IQR","ln_wFe_IQR"),
    labels = c("wAs","wFe")
  ))

est_slb_wt_v3 <- est_slb_wt_v3 %>%
  mutate(set = factor(set,
    levels = c("Unadjusted","Adjusted")
  ))

est_slb_wt_v3 %>% head()

##### Plot Estimates ###########################################################
# Define Plot Theme
th <- theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 36, color = "black"),
    axis.title = element_text(size = 36, color = "black"),
    axis.text = element_text(size = 36, color = "black"),
    legend.position = "right",
    legend.title = element_text(size = 36, color = "black"),
    legend.text = element_text(size = 36, color = "black")
  )

# Generate Figures
(fig1 <- est_slb_wt %>%
  ggplot(aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high, 
    color = set)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(width = 0, position = position_dodge(width = 0.5), 
    linewidth = 2) +
  geom_point(position = position_dodge(width = 0.5), size = 9) +
  scale_y_continuous(breaks = seq(-1,1,0.2)) +
  facet_wrap(. ~ visit, nrow = 1) +
  labs(
    x = NULL,
    y = "Expected Difference\nin Hemoglobin (g/dL)",
    color = "Model") +
  th)

(fig2 <- est_slb_wt_v3 %>%
  ggplot(aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high, 
    color = set)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(width = 0, position = position_dodge(width = 0.5), 
    linewidth = 2) +
  geom_point(position = position_dodge(width = 0.5), size = 9) +
  scale_y_continuous(breaks = seq(-1,1,0.2)) +
  facet_wrap(. ~ stratum, nrow = 1) +
  labs(
    x = NULL,
    y = "Expected Difference\nin Hemoglobin (g/dL)",
    color = "Model") +
  th)

# Export Figures
ggsave(
  plot = fig1,
  filename = "~/Desktop/research/manuscripts/smith_etal_pair_hemoglobin/drafts/sper2023/fig1.jpg",
  device = "jpeg",
  width = 23,
  height = 6,
  units = "in",
  dpi = 400
)

ggsave(
  plot = fig2,
  filename = "~/Desktop/research/manuscripts/smith_etal_pair_hemoglobin/drafts/sper2023/fig2.jpg",
  device = "jpeg",
  width = 18,
  height = 7,
  units = "in",
  dpi = 400
)



