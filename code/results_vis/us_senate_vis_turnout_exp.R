#-------------------------------------------------------------------------------
#
# Electoral predictors - 
#   visualization turnout & pc expenditures
#
# Data: US Senate pre-election polls 1990 - 2022
#
#-------------------------------------------------------------------------------


# Libraries ---------------------------------------------------------------

{
  library(ggplot2)
  library(dplyr)
  library(cowplot)
  library(grid)
  library(gridExtra)
  library(ggrepel)
  library(usdata)
}


# Data --------------------------------------------------------------------

ex_sd_turnout <- readRDS("~/results_vis/us_senate_predictable/us_senate_ex_sd_turnout.RDS")
exp_ex_sd_turnout <- readRDS("~/results_vis/us_senate_predictable/us_senate_exp_ex_sd_turnout.RDS")
ex_sd_exp <- readRDS("~/results_vis/us_senate_predictable/us_senate_ex_sd_exp.RDS")
exp_ex_sd_exp <- readRDS("~/results_vis/us_senate_predictable/us_senate_exp_ex_sd_exp.RDS")


# specify jitter position+
pos <- position_jitter(height = 0.4, width = 0, seed = 2022)


# Preparation -------------------------------------------------------------

ex_sd_exp <- ex_sd_exp %>% 
  mutate(log_total = log(exp_pc_dem + exp_pc_rep),
         state_year_short = paste0(state2abbr(state), " ", cycle),
         mean_ex_sd = mean(mean),
         sd_ex_sd = sd(mean), 
         mean_log_total = mean(log_total),
         sd_log_total = sd(log_total),
         add_label = if_else(mean > mean_ex_sd + (2*sd_ex_sd) |
                               mean < mean_ex_sd - (2*sd_ex_sd) |
                               log_total > mean_log_total + (2*sd_log_total) |
                               log_total < mean_log_total - (2*sd_log_total), 1, 0))

exp_ex_sd_exp <- exp_ex_sd_exp %>% 
  mutate(log_total = log(exp_pc_dem + exp_pc_rep))

ex_sd_turnout <- ex_sd_turnout %>% 
  mutate(state_year_short = paste0(state2abbr(state), " ", cycle),
         mean_ex_sd = mean(mean),
         sd_ex_sd = sd(mean), 
         mean_to = mean(turnout_vep),
         sd_to = sd(turnout_vep),
         add_label = if_else(mean > mean_ex_sd + (2*sd_ex_sd) |
                               mean < mean_ex_sd - (2*sd_ex_sd) |
                               turnout_vep > mean_to + (2*sd_to) |
                               turnout_vep < mean_to - (2*sd_to), 1, 0)) 


# Plots -------------------------------------------------------------------

# excess SD turnout plot
turnout_edb_plot <- ggplot() +
  geom_line(data = exp_ex_sd_turnout, aes(x = turnout_vep*100, y = mean*100), 
            color = "darkgrey")  +
  geom_ribbon(data = exp_ex_sd_turnout, aes(x = turnout_vep*100, ymin = `2.5%`*100, ymax = `97.5%`*100), 
              fill = "darkgrey", alpha = 0.5) +
  geom_point(data = ex_sd_turnout, aes(x = turnout_vep*100, y = mean*100)) +
  geom_segment(data = ex_sd_turnout, aes(x = turnout_vep*100, xend = turnout_vep*100, 
                   y = `2.5%`*100, yend = `97.5%`*100)) +
  theme_bw() +
  labs(x = "Turnout (%)", y = "")  +
  theme(text = element_text(size = 16),
        plot.margin=grid::unit(c(2,6,0,2), "mm"))

# excess SD total pc expenditures plot
exp_edb_plot <- ggplot() +
  geom_line(data = exp_ex_sd_exp, aes(x = log_total, y = mean*100), 
            color = "darkgrey")  +
  geom_ribbon(data = exp_ex_sd_exp, aes(x = log_total, ymin = `2.5%`*100, ymax = `97.5%`*100), 
              fill = "darkgrey", alpha = 0.5) +
  geom_point(data = ex_sd_exp, aes(x = log_total, y = mean*100)) +
  geom_segment(data = ex_sd_exp, aes(x = log_total, xend = log_total, 
                                         y = `2.5%`*100, yend = `97.5%`*100)) +
  theme_bw() +
  labs(x = "log(PC Expenditures)", y = "")  +
  theme(text = element_text(size = 16),
        plot.margin=grid::unit(c(2,6,0,2), "mm"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# arrange plots
turnout_exp_plot <- plot_grid(turnout_edb_plot, exp_edb_plot, ncol = 2,
                              rel_widths = c(1.05, 1), 
                              labels = "AUTO", label_x = 0.96) +
  draw_label("Excess SD (%)", x=  0, y=0.55, vjust= 1.5, angle=90)

# save
ggsave(filename = '~/results_vis/us_senate_predictable/plots/ex_sd_turnout_exp.png', 
       plot = turnout_exp_plot, 
       width = 14, height = 4, bg='#ffffff') 
ggsave(filename = '~/results_vis/us_senate_predictable/plots/ex_sd_turnout_exp.eps', 
       plot = turnout_exp_plot, 
       width = 14, height = 4, bg='#ffffff', device=cairo_ps) 

rm(turnout_edb_plot, exp_edb_plot, turnout_exp_plot)

# excess SD turnout plot by cycle
turnout_cycle_edb_plot <- ggplot() +
  geom_point(data = ex_sd_turnout, aes(x = turnout_vep*100, y = mean*100)) +
  geom_segment(data = ex_sd_turnout, 
               aes(x = turnout_vep*100, xend = turnout_vep*100, 
                   y = `2.5%`*100, yend = `97.5%`*100)) +
  theme_bw() +
  labs(x = "Turnout (%)", y = "Excess SD (%)")  +
  theme(text = element_text(size = 16),
        plot.margin=grid::unit(c(2,6,0,2), "mm")) +
  facet_wrap(~cycle, ncol = 4)

# save
ggsave(filename = '~/results_vis/us_senate_predictable/plots/ex_sd_turnout_cycle.png', 
       plot = turnout_cycle_edb_plot, 
       width = 14, height = 8, bg='#ffffff') 
ggsave(filename = '~/results_vis/us_senate_predictable/plots/ex_sd_turnout_cycle.eps', 
       plot = turnout_cycle_edb_plot, 
       width = 14, height = 8, bg='#ffffff') 

# excess SD exp plot by cycle
exp_cycle_edb_plot <- ggplot() +
  geom_point(data = ex_sd_exp, aes(x = log_total, y = mean*100)) +
  geom_segment(data = ex_sd_exp, aes(x = log_total, xend = log_total, 
                                     y = `2.5%`*100, yend = `97.5%`*100)) +
  theme_bw() +
  labs(x = "log(PC Expenditures)", y = "Excess SD (%)")  +
  theme(text = element_text(size = 16),
        plot.margin=grid::unit(c(2,6,0,2), "mm")) +
  facet_wrap(~cycle, ncol = 4)

# save
ggsave(filename = '~/results_vis/us_senate_predictable/plots/ex_sd_exp_cycle.png', 
       plot = exp_cycle_edb_plot, 
       width = 14, height = 8, bg='#ffffff') 
ggsave(filename = '~/results_vis/us_senate_predictable/plots/ex_sd_exp_cycle.eps', 
       plot = exp_cycle_edb_plot, 
       width = 14, height = 8, bg='#ffffff') 


# excess SD labeld turnout plot
turnout_ex_sd_label_plot <- ggplot() +
  geom_line(data = exp_ex_sd_turnout, aes(x = turnout_vep*100, y = mean*100), 
            color = "darkgrey")  +
  geom_ribbon(data = exp_ex_sd_turnout, 
              aes(x = turnout_vep*100, ymin = `2.5%`*100, ymax = `97.5%`*100), 
              fill = "darkgrey", alpha = 0.5) +
  geom_point(data = ex_sd_turnout, aes(x = turnout_vep*100, y = mean*100)) +
  geom_segment(data = ex_sd_turnout, 
               aes(x = turnout_vep*100, xend = turnout_vep*100, 
                   y = `2.5%`*100, yend = `97.5%`*100)) +
  geom_label_repel(data = ex_sd_turnout %>% 
                     filter(mean > mean_ex_sd + (2*sd_ex_sd)), 
                   aes(x = turnout_vep*100, y = mean*100, 
                       label = ifelse(add_label == 1,state_year_short, "")),
                   nudge_y = 0.7, color = "grey",
                   nudge_x = 2.1) +
  geom_label_repel(data = ex_sd_turnout %>% 
                     filter(mean < mean_ex_sd - (2*sd_ex_sd)), 
                   aes(x = turnout_vep*100, y = mean*100, 
                       label = ifelse(add_label == 1,state_year_short, "")),
                   nudge_y = -1, color = "grey",
                   nudge_x = 1.5) +
  geom_label_repel(data = ex_sd_turnout %>% 
                     filter(turnout_vep > mean_to + (2*sd_to) & 
                              !(mean < mean_ex_sd - (2*sd_ex_sd)) & 
                              !(mean > mean_ex_sd + (2*sd_ex_sd))), 
                   aes(x = turnout_vep*100, y = mean*100, 
                       label = ifelse(add_label == 1, state_year_short, "")),
                   nudge_y = 0.0, color = "grey",
                   nudge_x = 7) +
  geom_label_repel(data = ex_sd_turnout %>% 
                     filter(turnout_vep < mean_to - (2*sd_to) & 
                              !(mean < mean_ex_sd - (2*sd_ex_sd)) & 
                              !(mean > mean_ex_sd + (2*sd_ex_sd))), 
                   aes(x = turnout_vep*100, y = mean*100, 
                       label = ifelse(add_label == 1,state_year_short, "")),
                   nudge_y = 0.0, color = "grey",
                   nudge_x = -8) +
  theme_bw() +
  labs(x = "Turnout (%)", y = "Excess SD (%)")  +
  theme(text = element_text(size = 16),
        plot.margin=grid::unit(c(2,6,0,2), "mm"))

ggsave(filename = '~/results_vis/us_senate_predictable/plots/turnout_ex_sd_label_plot.png', 
       plot = turnout_ex_sd_label_plot, 
       width = 14, height = 8, bg='#ffffff') 


# excess SD labeld total pc expenditures plot
exp_ex_sd_label_plot <- ggplot() +
  geom_line(data = exp_ex_sd_exp, aes(x = log_total, y = mean*100), 
            color = "darkgrey")  +
  geom_ribbon(data = exp_ex_sd_exp, 
              aes(x = log_total, ymin = `2.5%`*100, ymax = `97.5%`*100), 
              fill = "darkgrey", alpha = 0.5) +
  geom_point(data = ex_sd_exp, aes(x = log_total, y = mean*100)) +
  geom_segment(data = ex_sd_exp, aes(x = log_total, xend = log_total, 
                                     y = `2.5%`*100, yend = `97.5%`*100)) +
  geom_label_repel(data = ex_sd_exp %>% 
                     filter(mean > mean_ex_sd + (2*sd_ex_sd)), 
                   aes(x = log_total, y = mean*100, 
                       label = ifelse(add_label == 1,state_year_short, "")),
                   nudge_y = 0.9, color = "grey",
                   nudge_x = 0.3) +
  geom_label_repel(data = ex_sd_exp %>% 
                     filter(mean < mean_ex_sd - (2*sd_ex_sd)), 
                   aes(x = log_total, y = mean*100, 
                       label = ifelse(add_label == 1,state_year_short, "")),
                   nudge_y = -1, color = "grey",
                   nudge_x = 0.15) +
  geom_label_repel(data = ex_sd_exp %>% 
                     filter(log_total > mean_log_total + (2*sd_log_total) & 
                              !(mean < mean_ex_sd - (2*sd_ex_sd)) & 
                              !(mean > mean_ex_sd + (2*sd_ex_sd))), 
                   aes(x = log_total, y = mean*100, 
                       label = ifelse(add_label == 1,state_year_short, "")),
                   nudge_y = 0.0, color = "grey",
                   nudge_x = 0.6) +
  geom_label_repel(data = ex_sd_exp %>% 
                     filter(log_total < mean_log_total - (2*sd_log_total) & 
                              !(mean < mean_ex_sd - (2*sd_ex_sd)) & 
                              !(mean > mean_ex_sd + (2*sd_ex_sd))), 
                   aes(x = log_total, y = mean*100, 
                       label = ifelse(add_label == 1,state_year_short, "")),
                   nudge_y = -0.3, color = "grey",
                   nudge_x = -0.4) +
  theme_bw() +
  labs(x = "log(PC Expenditures)", y = "Excess SD (%)")  +
  theme(text = element_text(size = 16),
        plot.margin=grid::unit(c(2,6,0,2), "mm"))

ggsave(filename = '~/results_vis/us_senate_predictable/plots/exp_ex_sd_label_plot.png', 
       plot = exp_ex_sd_label_plot, 
       width = 14, height = 8, bg='#ffffff') 

