#-------------------------------------------------------------------------------
#
# Electoral predictors - full & 30 day empty model comparison
#
#-------------------------------------------------------------------------------

# Libraries ---------------------------------------------------------------

{
  library(ggplot2)
  library(dplyr)
}


# Data --------------------------------------------------------------------

# election-level election day bias estimates
b0_summary <- readRDS("~/results_vis/us_senate_predictable/bias_senate1990_2022_empty.RDS")
b0_30_summary <- readRDS("~/results_vis/us_senate_predictable/bias_senate1990_2022_empty30.RDS")

# election-level excess sd estimates
ex_sd_summary <- readRDS("~/results_vis/us_senate_predictable/excess_sd_senate1990_2022_empty.RDS")
ex_sd_30_summary <- readRDS("~/results_vis/us_senate_predictable/excess_sd_senate1990_2022_empty30.RDS")


# Plots -------------------------------------------------------------------

# election day bias
b0_summary_complete30 <- rbind(b0_summary %>% 
                                 mutate(period = "Complete"),
                               b0_30_summary %>% 
                                 mutate(period = "30 days"))

b0_complete_30_plot <- ggplot(b0_summary_complete30) +
  geom_point(aes(y = mean*100, x = state_abb, color = period), position = position_dodge(width = 0.7)) +
  geom_linerange(aes(x = state_abb,  ymin = `2.5%`*100, ymax = `97.5%`*100, 
                   color = period), position = position_dodge(width = 0.7)) +
  facet_wrap(~cycle, ncol = 1, strip.position = "right") +
  theme_bw() +
  labs(x = "State", y = "Election day bias (%)", color = "Time period") +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(angle = 45),
        legend.position = "bottom") +
  geom_hline(yintercept = 0, color = "lightgrey", linetype = "dashed")  +
  scale_color_manual(values = c("darkgrey", "black"))
  
ggsave(filename = "~/results_vis/us_senate_predictable/plots/b0_complete_30_plot.png",
       plot = b0_complete_30_plot,
       width = 15, height = 18, bg="#ffffff")

# excess sd
ex_sd_summary_complete30 <- rbind(ex_sd_summary %>% 
                                 mutate(period = "Complete"),
                               ex_sd_30_summary %>% 
                                 mutate(period = "30 days"))

ex_sd_complete_30_plot <- ggplot(ex_sd_summary_complete30) +
  geom_point(aes(y = mean*100, x = state_abb, color = period), position = position_dodge(width = 0.7)) +
  geom_linerange(aes(x = state_abb,  ymin = `2.5%`*100, ymax = `97.5%`*100, 
                     color = period), position = position_dodge(width = 0.7)) +
  facet_wrap(~cycle, ncol = 1, strip.position = "right") +
  theme_bw() +
  labs(x = "State", y = "Excess SD (%)", color = "Time period") +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(angle = 45),
        legend.position = "bottom") +
  scale_color_manual(values = c("darkgrey", "black"))

ggsave(filename = "~/results_vis/us_senate_predictable/plots/ex_sd_complete_30_plot.png",
       plot = ex_sd_complete_30_plot,
       width = 15, height = 18, bg="#ffffff")




