################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Drinking Water Arsenic and Hemoglobin -- Figures

# Tyler Smith
# January 11, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(broom)

# Define Plot Theme
th <- theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

##### Figure 1: Unadjusted Estimates ###########################################
df_fig1 <- rbind(
  tidy(model_v1_cru_as, conf.int = TRUE)[2,] %>% mutate(VISIT = 1, VAR = "As"),
  tidy(model_v1_cru_fe, conf.int = TRUE)[2,] %>% mutate(VISIT = 1, VAR = "Fe"),
  tidy(model_v2_cru_as, conf.int = TRUE)[2,] %>% mutate(VISIT = 2, VAR = "As"),
  tidy(model_v2_cru_fe, conf.int = TRUE)[2,] %>% mutate(VISIT = 2, VAR = "Fe"),
  tidy(model_v3_cru_as, conf.int = TRUE)[2,] %>% mutate(VISIT = 3, VAR = "As"),
  tidy(model_v3_cru_fe, conf.int = TRUE)[2,] %>% mutate(VISIT = 3, VAR = "Fe"),
  tidy(model_v4_cru_as, conf.int = TRUE)[2,] %>% mutate(VISIT = 4, VAR = "As"),
  tidy(model_v4_cru_fe, conf.int = TRUE)[2,] %>% mutate(VISIT = 4, VAR = "Fe")
)

df_fig1 <- df_fig1 %>%
  mutate(VISIT = factor(VISIT, 
    levels = c(1,2,3,4), 
    labels = c("Visit 1","Visit 2","Visit 3","Visit 4")))

df_fig1 %>% head()

df_fig1 %>%
  filter(term == "ln_wAs_IQR") %>%
  select(term, estimate, conf.low, conf.high) %>%
  mutate(across(-term, ~ round(.x, 2)))

(fig1 <- df_fig1 %>%
  ggplot(aes(x = VAR, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(width = 0.05) +
  geom_point() +
  scale_y_continuous(breaks = seq(-10,10,0.2), limits = c(-0.4,1.0)) +
  facet_wrap(. ~ VISIT, nrow = 1) +
  labs(
    title = "Unadjusted Mean Differences per IQR-unit Difference in Drinking Water Arsenic or Iron",
    x = NULL,
    y = "Unadjusted Mean Difference (g/dL)\n(95% Confidence Interval)") +
  th)

##### Figure 2: Adjusted Estimates #############################################
df_fig2 <- rbind(
  tidy(model_v1_adj, conf.int = TRUE)[2,] %>% mutate(VISIT = 1, VAR = "As"),
  tidy(model_v1_adj, conf.int = TRUE)[3,] %>% mutate(VISIT = 1, VAR = "Fe"),
  tidy(model_v2_adj, conf.int = TRUE)[2,] %>% mutate(VISIT = 2, VAR = "As"),
  tidy(model_v2_adj, conf.int = TRUE)[3,] %>% mutate(VISIT = 2, VAR = "Fe"),
  tidy(model_v3_adj, conf.int = TRUE)[2,] %>% mutate(VISIT = 3, VAR = "As"),
  tidy(model_v3_adj, conf.int = TRUE)[3,] %>% mutate(VISIT = 3, VAR = "Fe"),
  tidy(model_v4_adj, conf.int = TRUE)[2,] %>% mutate(VISIT = 4, VAR = "As"),
  tidy(model_v4_adj, conf.int = TRUE)[3,] %>% mutate(VISIT = 4, VAR = "Fe")
)

df_fig2 <- df_fig2 %>%
  mutate(VISIT = factor(VISIT, 
    levels = c(1,2,3,4), 
    labels = c("Visit 1","Visit 2","Visit 3","Visit 4")))

df_fig2 %>% head()

df_fig2 %>%
  filter(term == "ln_wAs_IQR") %>%
  select(term, estimate, conf.low, conf.high) %>%
  mutate(across(-term, ~ round(.x, 2)))

(fig2 <- df_fig2 %>%
  ggplot(aes(x = VAR, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(width = 0.05) +
  geom_point() +
  scale_y_continuous(breaks = seq(-10,10,0.2), limits = c(-0.4,1.0)) +
  facet_wrap(. ~ VISIT, nrow = 1) +
  labs(
    title = "Adjusted Mean Differences per IQR-unit Difference in Drinking Water Arsenic or Iron",
    x = NULL,
    y = "Adjusted Mean Difference (g/dL)\n(95% Confidence Interval)") +
  th)

##### Figure S1: Drinking Water Arsenic and Iron ###############################
(figS1 <- df %>%
  filter(VISIT1 == 1) %>%
  ggplot(aes(x = ln_wAs, y = ln_wFe)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm") +
  geom_smooth(method = "gam", color = "red", linetype = "dashed") +
  labs(
    title = "Drinking Water Arsenic and Iron at Visit 1",
    x = "Drinking Water Arsenic (ln µg/L)",
    y = "Drinking Water Iron (ln µg/L)",
    caption = "Note: Red line is a generalized additive model\nwith cubic splines and shrinkage.") +
  theme_bw())

##### Figure S2: Maternal Hemoglobin ###########################################
# Prepare Data for Multi-panel Plots
tmp <- df %>%
  filter(LIVEBIRTH == 1) %>%
  select(ln_wAs, ln_wFe, ends_with("HEMO"), starts_with("VISIT")) %>%
  pivot_longer(ends_with("HEMO"))

tmp <- tmp %>%
  filter(
    (VISIT1 == 1 & name == "SEHEMO")  |
    (VISIT2 == 1 & name == "SVXHEMO") |
    (VISIT3 == 1 & name == "SMHEMO")  |
    (VISIT4 == 1 & name == "SM3HEMO")
  )
      
tmp <- tmp %>%
  mutate(name = factor(name, 
    levels = c("SEHEMO","SVXHEMO","SMHEMO","SM3HEMO"),
    labels = c("Visit 1","Visit 2","Visit 3","Visit 4")))

# Maternal Hemoglobin
(figS2 <- tmp %>%
  ggplot(aes(x = value, fill = factor(name))) +
  geom_density(alpha = 0.4) +
  scale_x_continuous(limits = c(5,16), breaks = seq(5,16,1)) +
  scale_y_continuous(limits = c(0,0.5)) +
  labs(
    title = "Maternal Hemoglobin by Visit",
    x = "Hemoglobin (g/dL)",
    y = "Density",
    fill = "Visit") +
  theme_bw())

##### Figures S3-S4: Check Linearity ###########################################
# Drinking Water Arsenic
(figS3 <- tmp %>%
  ggplot(aes(x = ln_wAs, y = value)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm") +
  geom_smooth(method = "gam", color = "red", linetype = "dashed") +
  scale_y_continuous(limits = c(5,16), breaks = seq(0,20,2)) +
  facet_wrap(. ~ name, nrow = 1) +
  labs(
    title = "Hemoglobin by Drinking Water Arsenic",
    x = "Drinking Water Arsenic (ln µg/L)",
    y = "Hemoglobin (g/dL)",
    caption = "Note: Blue line is a linear model.\nRed line is a generalized additive model with cubic splines.") +
  th)

# Drinking Water Iron
(figS4 <- tmp %>%
  ggplot(aes(x = ln_wFe, y = value)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm") +
  geom_smooth(method = "gam", color = "red", linetype = "dashed") +
  scale_y_continuous(limits = c(5,16), breaks = seq(0,20,2)) +
  facet_wrap(. ~ name, nrow = 1) +
  labs(
    title = "Hemoglobin by Drinking Water Iron",
    x = "Drinking Water Iron (ln µg/L)",
    y = "Hemoglobin (g/dL)",
    caption = "Note: Blue line is a linear model.\nRed line is a generalized additive model with cubic splines.") +
  th)

##### Export Plots #############################################################
# Set Working Directory
setwd("~/Desktop/research/manuscripts/smith_etal_pair_hemoglobin/tables_figures/")

# Figure 1
ggsave(
  plot = fig1,
  filename = "fig1.jpg",
  device = "jpeg",
  height = 3,
  width = 9,
  units = "in",
  dpi = 300
)

# Figure 2
ggsave(
  plot = fig2,
  filename = "fig2.jpg",
  device = "jpeg",
  height = 3,
  width = 9,
  units = "in",
  dpi = 300
)

# Figure S1
ggsave(
  plot = figS1,
  filename = "figS1.jpg",
  device = "jpeg",
  height = 6,
  width = 6,
  units = "in",
  dpi = 300
)

# Figure S2
ggsave(
  plot = figS2,
  filename = "figS2.jpg",
  device = "jpeg",
  height = 6,
  width = 8,
  units = "in",
  dpi = 300
)

# Figure S3
ggsave(
  plot = figS3,
  filename = "figS3.jpg",
  device = "jpeg",
  height = 6,
  width = 9,
  units = "in",
  dpi = 300
)

# Figure S4
ggsave(
  plot = figS4,
  filename = "figS4.jpg",
  device = "jpeg",
  height = 6,
  width = 9,
  units = "in",
  dpi = 300
)






