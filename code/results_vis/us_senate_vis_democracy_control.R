#-------------------------------------------------------------------------------
#
# Electoral predictors - 
#   visualization state democracy score & state control
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
  library(usdata)
  library(stringi)
  library(usdata)
  library(ggrepel)
}


# Data --------------------------------------------------------------------

b0_democracy <- readRDS("~/results_vis/us_senate_predictable/us_senate_b0_democracy.RDS")
exp_b0_democracy <- readRDS("~/results_vis/us_senate_predictable/us_senate_exp_b0_democracy.RDS")
avg_exp_b0_democracy <- readRDS("~/results_vis/us_senate_predictable/us_senate_avg_exp_b0_democracy.RDS")
b0_control <- readRDS("~/results_vis/us_senate_predictable/us_senate_b0_control.RDS")
avg_b0_control <- readRDS("~/results_vis/us_senate_predictable/us_senate_avg_b0_control.RDS")


# specify jitter position+
pos <- position_jitter(height = 0.4, width = 0, seed = 2022)


# Preparation -------------------------------------------------------------

# edb democracy score
b0_democracy <- b0_democracy %>% 
  mutate(mean_b0 = mean(mean),
         sd_b0 = sd(mean), 
         mean_democracy = mean(democracy_mcmc),
         sd_democracy = sd(democracy_mcmc),
         add_label = if_else(mean > mean_b0 + (2*sd_b0) |
                               mean < mean_b0 - (2*sd_b0) |
                               democracy_mcmc > mean_democracy + (2*sd_democracy) |
                               democracy_mcmc < mean_democracy - (2*sd_democracy), 1, 0),
         state_year_short = paste0(state2abbr(state), " ", cycle))

b0_control <- b0_control %>% 
  mutate(control = case_when(control_dem == 1 & control_rep == 0 ~ "Dem. state control", 
                             control_dem == 0 & control_rep == 1 ~ "Rep. state control",
                             control_dem == 0 & control_rep == 0 ~ "No state control") %>% 
           factor(levels = c("Dem. state control", "Rep. state control",  
                             "No state control")),
         state_year_abb = paste0(state2abbr(state), stri_sub(cycle, -2, -1)),
         state_year_short = paste0(state2abbr(state), " ", cycle), 
         mean_b0 = mean(mean),
         sd_b0 = sd(mean), 
         add_label = if_else(mean > mean_b0 + (2*sd_b0) |
                               mean < mean_b0 - (2*sd_b0) , 1, 0),
         preclearance = if_else(state %in% c("Alabama", "Alaska", "Arizona", 
                                             "Georgia", "Louisiana", 
                                             "Mississippi", "South Carolina", 
                                             "Texas",  "Virginia"),
                                 "Preclearance", "Non-preclearance"))

avg_b0_control <- avg_b0_control %>% 
  mutate(control = case_when(feature == "control_dem" ~ "Dem. state control",
                             feature == "control_rep" ~ "Rep. state control",
                             feature == "control_neither" ~ "No state control") %>% 
           factor(levels = c("Dem. state control", "Rep. state control",  
                             "No state control")),
         estimate = "Expected")


# Plots -------------------------------------------------------------------

# edb State Democracy Index plot
democracy_edb_plot <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey") +
  geom_line(data = avg_exp_b0_democracy, aes(x = democracy_mcmc, y = mean*100), 
            color = "darkgrey")  +
  geom_ribbon(data = avg_exp_b0_democracy, 
              aes(x = democracy_mcmc, ymin = `2.5%`*100, ymax = `97.5%`*100), 
              fill = "darkgrey", alpha = 0.5) +
  geom_point(data = b0_democracy, aes(x = democracy_mcmc, y = mean*100)) +
  geom_segment(data = b0_democracy, 
               aes(x = democracy_mcmc, xend = democracy_mcmc, 
                   y = `2.5%`*100, yend = `97.5%`*100)) +
  theme_bw() +
  labs(x = "State Democracy Index", y = "Election-day bias (%)")  +
  theme(text = element_text(size = 16),
        plot.margin=grid::unit(c(2,10,0,8), "mm"))

# edb by state control
control_edb_plot <- ggplot(b0_control) +
  geom_segment(aes(y = as.factor(cycle), yend = as.factor(cycle), 
                   x = `2.5%`*100, xend = `97.5%`*100,
                   color = control),
               position = position_jitter(seed = 2022)) +
  geom_point(data = b0_control,
             aes(y = as.factor(cycle),x = mean*100, color = control), 
             position = position_jitter(seed = 2022)) +
  theme_bw() +
  labs(y = "", x = "")  +
  theme(text = element_text(size = 16), 
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "NULL",
        plot.margin=grid::unit(c(2,10,0,2), "mm"),
        axis.title.y = element_text(angle = 360, vjust = 0.5)) +
  facet_wrap(~control, ncol = 4) +
  scale_color_manual(values = c("#0015BC","#DE0100", "darkgrey")) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkgrey") +
  xlim(-20, 14) +
  scale_y_discrete(expand = c(0.08, 0.08), limits = rev) 

control_avg_bias_plot <- ggplot(avg_b0_control) +
  geom_point(aes(y = estimate, x = mean*100, color = control)) +
  geom_segment(aes(y = estimate, yend = estimate, x = `2.5%`*100, 
                   xend = `97.5%`*100, color = control)) +
  theme_bw() +
  facet_wrap(~control, ncol = 4)  +
  scale_color_manual(values = c("#0015BC","#DE0100", "darkgrey"))  +
  theme(text = element_text(size = 16), 
        axis.text.x = element_text(size = 12),
        legend.position = "NULL",
        strip.text.x = element_blank(),
        plot.margin=grid::unit(c(2,10,0,2), "mm"),
        axis.title.y = element_text(angle = 360, vjust = 0.5))  +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkgrey") +
  labs(y = "", x = "Election-day bias (%)")  +
  xlim(-20, 14)


control_plot <- plot_grid(control_edb_plot, NULL, control_avg_bias_plot, 
                      ncol = 1, align = "v",  rel_heights = c(4, -0.25, 1))

# combine plots
democracy_control_plot <- plot_grid(democracy_edb_plot, control_plot, ncol = 1, 
                                  rel_heights = c(0.6,1), labels = "AUTO",
                                  label_x = 0.98)

# save plots
ggsave(filename = '~/results_vis/us_senate_predictable/plots/b0_democarcy_control.png', 
       plot = democracy_control_plot, 
       width = 14, height = 8, bg='#ffffff') 
ggsave(filename = '~/results_vis/us_senate_predictable/plots/b0_democarcy_control.eps', 
       plot = democracy_control_plot, 
       width = 14, height = 8, bg='#ffffff', device=cairo_ps) 

# edb vs. democracy index plot by cycle
democracy_cycle_edb_plot <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey") +
  geom_point(data = b0_democracy, aes(x = democracy_mcmc, y = mean*100)) +
  geom_segment(data = b0_democracy, 
               aes(x = democracy_mcmc, xend = democracy_mcmc, 
                   y = `2.5%`*100, yend = `97.5%`*100)) +
  theme_bw() +
  labs(x = "State Democracy Index", y = "Election day bias (%)")  +
  theme(text = element_text(size = 16),
        plot.margin=grid::unit(c(2,6,0,2), "mm")) +
  facet_wrap(~cycle, ncol = 4)

# save
ggsave(filename = '~/results_vis/us_senate_predictable/plots/edb_democracy_cycle.png', 
       plot = democracy_cycle_edb_plot, 
       width = 14, height = 5, bg='#ffffff') 
ggsave(filename = '~/results_vis/us_senate_predictable/plots/edb_democracy_cycle.eps', 
       plot = democracy_cycle_edb_plot, 
       width = 14, height = 5, bg='#ffffff') 

# edb democracy index labeld plot
democracy_edb_labeled_plot <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey") +
  geom_line(data = avg_exp_b0_democracy, aes(x = democracy_mcmc, y = mean*100), 
            color = "grey")  +
  geom_ribbon(data = avg_exp_b0_democracy, 
              aes(x = democracy_mcmc, ymin = `2.5%`*100, ymax = `97.5%`*100), 
              alpha = 0.3, fill = "grey") +
  geom_point(data = b0_democracy, aes(x = democracy_mcmc, y = mean*100)) +
  geom_segment(data = b0_democracy, 
               aes(x = democracy_mcmc, xend = democracy_mcmc,  
                   y = `2.5%`*100, yend = `97.5%`*100)) +
  geom_label_repel(data = b0_democracy %>% 
                     filter(mean > mean_b0 + (1.96*sd_b0)), 
                   aes(x = democracy_mcmc, y = mean*100, 
                       label = ifelse(add_label == 1, state_year_short, "")),
                   nudge_y = 2, color = "grey",
                   nudge_x = 0.2) +
  geom_label_repel(data = b0_democracy %>% 
                     filter(mean < mean_b0 - (1.96*sd_b0)), 
                   aes(x = democracy_mcmc, y = mean*100, 
                       label = ifelse(add_label == 1,state_year_short, "")),
                   nudge_y = -3, color = "grey",
                   nudge_x = 0.1) +
  geom_label_repel(data = b0_democracy %>% 
                     filter(democracy_mcmc > mean_democracy + (1.96*sd_democracy) & 
                              !(mean < mean_b0 - (1.96*sd_b0)) & 
                              !(mean > mean_b0 + (1.96*sd_b0))), 
                   aes(x = democracy_mcmc, y = mean*100, 
                       label = ifelse(add_label == 1,state_year_short, "")),
                   nudge_y = 10, color = "grey",
                   nudge_x = 0.1) +
  geom_label_repel(data = b0_democracy %>% 
                     filter(democracy_mcmc < mean_democracy - (1.96*sd_democracy) & 
                              !(mean < mean_b0 - (1.96*sd_b0)) & 
                              !(mean > mean_b0 + (1.96*sd_b0))), 
                   aes(x = democracy_mcmc, y = mean*100, 
                       label = ifelse(add_label == 1,state_year_short, "")),
                   nudge_y = 0.0, color = "grey",
                   nudge_x = -0.4) +  
  theme_bw() +
  labs(x = "State Democracy Index", y = "Election-day bias (%)")  +
  theme(text = element_text(size = 16),
        plot.margin=grid::unit(c(2,10,0,2), "mm"),
        legend.position = c(0.95, 0.2),
        legend.background=element_blank()) 

ggsave(filename = '~/results_vis/us_senate_predictable/plots/democracy_edb_labeled_plot.png', 
       plot = democracy_edb_labeled_plot, 
       width = 14, height = 8, bg='#ffffff') 

# edb by preclearnce & state control

control_preclearance_plot <- ggplot(b0_control) +
  geom_pointrange(aes(x = cycle, y = mean*100, 
                      ymin = `2.5%`*100, ymax = `97.5%`*100,
                      color = as.factor(control_rep)),
                  size = 0.3,
                  position = position_jitter(width = 0.7, height = 0)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey") +
  geom_vline(xintercept = 2013, linetype = "dashed") +
  scale_x_continuous(breaks = seq(1990, 2022, by = 2)) +
  scale_color_manual(labels = c("Non-Republican", "Republican"),
                     values=c('darkgrey', 'black'),
                     name = "State Control") +
  facet_wrap(~preclearance) +
  labs(x = "",
       y = "Election-day bias (%)") +
  theme_bw() +
  theme(legend.position="bottom",
        text = element_text(size = 16),
        axis.text.x = element_text(angle = 45, vjust = 0.5))

ggsave(filename = '~/results_vis/us_senate_predictable/plots/control_preclearance_plot.png', 
       plot = control_preclearance_plot, 
       width = 12, height = 6, bg='#ffffff') 

# labeld edb by state control
b0_control <- b0_control %>% 
  mutate(state_year_short = if_else(mean > (mean_b0 + (2*sd_b0)) |
                                      mean < (mean_b0 - (2*sd_b0)),
                                    state_year_short, ""))

control_edb_label_plot <- ggplot(b0_control) +
  geom_segment(aes(y = as.factor(cycle), yend = as.factor(cycle),
                   x = `2.5%`*100, xend = `97.5%`*100,
                   color = control),
               position = pos) +
  geom_point(data = b0_control,
             aes(y = as.factor(cycle), x = mean*100, color = control), 
             position = pos) +
  geom_label_repel(data = b0_control, 
                   aes(x = mean*100, y = as.factor(cycle), 
                       label = state_year_short), color = "grey",
                   position = pos,
                   min.segment.length = 0) +
  theme_bw() +
  labs(x = "Election day bias (%)", y = "Cycle")  +
  theme(text = element_text(size = 16),
        legend.position = "NULL",
        plot.margin=grid::unit(c(2,10,0,2), "mm"),
        axis.title.y = element_text(angle = 90, vjust = 0.5)) +
  facet_wrap(~control, ncol = 3) +
  scale_color_manual(values = c("#0015BC","#DE0100", "darkgrey")) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkgrey") +
  xlim(-20, 14) +
  scale_y_discrete(expand = c(0.08, 0.08), limits = rev)  

ggsave(filename = '~/results_vis/us_senate_predictable/plots/control_edb_label_plot.png', 
       plot = control_edb_label_plot, 
       width = 14, height = 8, bg='#ffffff') 


