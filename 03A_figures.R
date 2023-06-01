################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Drinking Water Arsenic and Hemoglobin -- Figures

# Tyler Smith
# May 29, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

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
  scale_color_manual(values = c("#d9d9d9","#4575c1")) +
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
  scale_color_manual(values = c("#d9d9d9","#4575c1")) +
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