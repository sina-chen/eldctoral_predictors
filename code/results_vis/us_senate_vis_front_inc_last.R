#-------------------------------------------------------------------------------
#
# Electoral predictors - 
#   visualization front runner & incumbency
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
  library(ggrepel)
  library(usdata)
}


# Data --------------------------------------------------------------------

b0_front <- readRDS("~/results_vis/us_senate_predictable/us_senate_b0_front.RDS")
avg_bias_front <- readRDS("~/results_vis/us_senate_predictable/us_senate_avg_b0_front.RDS")
b0_inc <- readRDS("~/results_vis/us_senate_predictable/us_senate_b0_inc.RDS")
avg_bias_inc <- readRDS("~/results_vis/us_senate_predictable/us_senate_avg_b0_inc.RDS")
b0_last <- readRDS("~/results_vis/us_senate_predictable/us_senate_b0_last.RDS")
exp_b0_last <- readRDS("~/results_vis/us_senate_predictable/us_senate_exp_b0_last.RDS")
avg_exp_b0_last <- readRDS("~/results_vis/us_senate_predictable/us_senate_avg_exp_b0_last.RDS")

# specify jitter position
pos <- position_jitter(height = 0.4, width = 0, seed = 2022)


# Preparation -------------------------------------------------------------

b0_front <- b0_front %>% 
  mutate(front = case_when(front_dem == 1 & front_rep == 0 ~ "Dem. frontrunner", 
                           front_dem == 0 & front_rep == 1 ~ "Rep. frontrunner",
                           front_dem == 0 & front_rep == 0 ~ "No frontrunner") %>% 
           factor(levels = c("Dem. frontrunner", "Rep. frontrunner",  
                             "No frontrunner")),
         state_year_abb = paste0(state2abbr(state), stri_sub(cycle, -2, -1)),
         state_year_short = paste0(state2abbr(state), " ", cycle),
         mean_b0 = mean(mean),
         sd_b0 = sd(mean), 
         add_label = if_else(mean > mean_b0 + (2*sd_b0) |
                               mean < mean_b0 - (2*sd_b0) , 1, 0))

avg_bias_front <- avg_bias_front %>% 
  mutate(front = case_when(feature == "front_dem" ~ "Dem. frontrunner",
                           feature == "front_rep" ~ "Rep. frontrunner",
                           feature == "front_neither" ~ "No frontrunner") %>% 
           factor(levels = c("Dem. frontrunner", "Rep. frontrunner",  
                             "No frontrunner")),
         estimate = "Expected")
            
b0_inc <- b0_inc %>% 
  mutate(inc = case_when(inc_dem == 1 & inc_rep == 0 ~ "Dem. incumbent", 
                         inc_dem == 0 & inc_rep == 1 ~ "Rep. incumbent",
                         inc_dem == 0 & inc_rep == 0 ~ "No incumbent") %>% 
           factor(levels = c("Dem. incumbent", "Rep. incumbent",  
                             "No incumbent")),
         state_year_abb = paste0(state2abbr(state), stri_sub(cycle, -2, -1)),
         state_year_short = paste0(state2abbr(state), " ", cycle), 
         mean_b0 = mean(mean),
         sd_b0 = sd(mean), 
         add_label = if_else(mean > mean_b0 + (2*sd_b0) |
                               mean < mean_b0 - (2*sd_b0) , 1, 0))

b0_last <- b0_last %>% 
  mutate(state_year_short = paste0(state2abbr(state), " ", cycle),
         mean_b0 = mean(mean),
         sd_b0 = sd(mean), 
         mean_last = mean(last_poll_margin),
         sd_last = sd(last_poll_margin),
         add_label = if_else(mean > sd_b0 + (2*sd_b0) |
                               mean < sd_b0 - (2*sd_b0) |
                               last_poll_margin > mean_last + (2*sd_last) |
                               last_poll_margin < mean_last - (2*sd_last), 1, 0)) 

avg_bias_inc <- avg_bias_inc %>% 
  mutate(inc = case_when(feature == "inc_dem" ~ "Dem. incumbent",
                         feature == "inc_rep" ~ "Rep. incumbent",
                         feature == "inc_neither" ~ "No incumbent") %>% 
           factor(levels = c("Dem. incumbent", "Rep. incumbent",  
                             "No incumbent")),
         estimate = "Expected")


# Plots -------------------------------------------------------------------

# edb by frontrunner
front_edb_plot <- ggplot(b0_front) +
  geom_segment(aes(y = as.factor(cycle), yend = as.factor(cycle), 
                   x = `2.5%`*100, xend = `97.5%`*100,
                   color = front),
               position = position_jitter(seed = 2022)) +
  geom_point(data = b0_front,
             aes(y = as.factor(cycle), x = mean*100, color = front), 
             position = position_jitter(seed = 2022)) +
  theme_bw() +
  labs(x = "", y = "")  +
  theme(text = element_text(size = 16), 
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "NULL",
        plot.margin=grid::unit(c(2,10,0,2), "mm"),
        axis.title.y = element_text(angle = 90, vjust = 0.5)) +
  facet_wrap(~front, ncol = 4) +
  scale_color_manual(values = c("#0015BC","#DE0100", "darkgrey")) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkgrey") +
  xlim(-20, 14) +
  scale_y_discrete(expand = c(0.08, 0.08), limits = rev) 
  
front_avg_bias_plot <- ggplot(avg_bias_front) +
  geom_point(aes(y = estimate, x = mean*100, color = front)) +
  geom_segment(aes(y = estimate, yend = estimate, x = `2.5%`*100, 
                   xend = `97.5%`*100,
                   color = front)) +
  theme_bw() +
  facet_wrap(~front, ncol = 4)  +
  scale_color_manual(values = c("#0015BC","#DE0100", "darkgrey"))  +
  theme(text = element_text(size = 16), 
        axis.text.x = element_text(size = 12),
        legend.position = "NULL",
        strip.text.x = element_blank(),
        plot.margin=grid::unit(c(2,10,0,2), "mm"),
        axis.title.y = element_text(angle = 90, vjust = 2))  +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkgrey") +
  labs(x = "Election-day bias (%)", y = "")  +
  xlim(-20, 14)


front_plot <- plot_grid(front_edb_plot, NULL, front_avg_bias_plot, 
                      ncol = 1, align = "v",  rel_heights = c(4,-0.25, 1))


# edb by incumbency
inc_edb_plot <- ggplot(b0_inc) +
  geom_segment(aes(y = as.factor(cycle), yend = as.factor(cycle), 
                   x = `2.5%`*100, xend = `97.5%`*100,
                   color = inc),
               position = position_jitter(seed = 2022)) +
  geom_point(data = b0_inc,
             aes(y = as.factor(cycle), x = mean*100, color = inc), 
             position = position_jitter(seed = 2022)) +
  theme_bw() +
  labs(x = "", y = "")  +
  theme(text = element_text(size = 16), 
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "NULL",
        plot.margin=grid::unit(c(2,10,0,2), "mm"),
        axis.title.y = element_text(angle = 90, vjust = 0.5)) +
  facet_wrap(~inc, ncol = 4) +
  scale_color_manual(values = c("#0015BC","#DE0100", "darkgrey")) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkgrey") +
  xlim(-20, 14) +
  scale_y_discrete(expand = c(0.08, 0.08), limits = rev) 

inc_avg_bias_plot <- ggplot(avg_bias_inc) +
  geom_point(aes(y = estimate, x = mean*100, color = inc)) +
  geom_segment(aes(y = estimate, yend = estimate, x = `2.5%`*100, 
                   xend = `97.5%`*100, color = inc)) +
  theme_bw() +
  facet_wrap(~inc, ncol = 4)  +
  scale_color_manual(values = c("#0015BC","#DE0100", "darkgrey"))  +
  theme(text = element_text(size = 16), 
        axis.text.x = element_text(size = 12),
        legend.position = "NULL",
        strip.text.x = element_blank(),
        plot.margin=grid::unit(c(2,10,0,2), "mm"),
        axis.title.y = element_text(angle = 90, vjust = 2))  +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkgrey") +
  labs(x = "Election-day bias (%)", y ="")  +
  xlim(-20, 14)

inc_plot <- plot_grid(inc_edb_plot, NULL, inc_avg_bias_plot, 
                      ncol = 1, align = "v",  rel_heights = c(4, -0.25, 1))


# margin last poll plot
last_plot <- ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkgrey") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey") +
  geom_line(data = avg_exp_b0_last, aes(x = last_poll_margin*100, y = mean*100), 
            color = "darkgrey")  +
  geom_ribbon(data = avg_exp_b0_last, 
              aes(x = last_poll_margin*100, ymin = `2.5%`*100, 
                  ymax = `97.5%`*100), 
              fill = "darkgrey", alpha = 0.5) +
  geom_point(data = b0_last, aes(x = last_poll_margin*100, y = mean*100)) +
  geom_segment(data = b0_last, 
               aes(x = last_poll_margin*100, xend = last_poll_margin*100,
                   y = `2.5%`*100, yend = `97.5%`*100)) +
  theme_bw() +
  labs(x = "Margin last poll, Rep.-Dem. (%)", y = "Election-day bias (%)") +
  scale_x_continuous(breaks = c(seq(-60,60,20))) +
  theme(text = element_text(size = 16),
        plot.margin=grid::unit(c(2,10,0,12), "mm"))

front_inc_last_plot <- plot_grid(front_plot, inc_plot, last_plot, ncol = 1, 
                            rel_heights = c(1,1,0.7), labels = "AUTO",
                            label_x = 0.97,  axis = "tblr")

# save
ggsave(filename = '~/results_vis/us_senate_predictable/plots/b0_front_inc_last.png', 
       plot = front_inc_last_plot, 
       width = 14, height = 12, bg='#ffffff') 

ggsave(filename = '~/results_vis/us_senate_predictable/plots/b0_front_inc_last.eps', 
       plot = front_inc_last_plot, 
       width = 14, height = 12, bg='#ffffff', device=cairo_ps) 

# edb vs. margin last poll plot by cycle
last_cycle_edb_plot <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey") +
  geom_point(data = b0_last, aes(x = last_poll_margin*100, y = mean*100)) +
  geom_segment(data = b0_last, aes(x = last_poll_margin*100, 
                                   xend = last_poll_margin*100, 
                                   y = `2.5%`*100, yend = `97.5%`*100)) +
  theme_bw() +
  labs(x = "Margin last poll, Rep.-Dem. (%)", y = "Election day bias (%)")  +
  theme(text = element_text(size = 16),
        plot.margin=grid::unit(c(2,6,0,2), "mm")) +
  facet_wrap(~cycle, ncol = 4)

# save
ggsave(filename = '~/results_vis/us_senate_predictable/plots/edb_last_cycle.png', 
       plot = last_cycle_edb_plot, 
       width = 14, height = 8, bg='#ffffff') 
ggsave(filename = '~/results_vis/us_senate_predictable/plots/edb_last_cycle.eps', 
       plot = last_cycle_edb_plot, 
       width = 14, height = 8, bg='#ffffff') 

pos <- position_jitter(height = 0.4, width = 0, seed = 2022)

# labeld edb by frontrunner
b0_front <- b0_front %>% 
  mutate(state_year_short = if_else(mean > mean_b0 + (2*sd_b0) |
                                      mean < mean_b0 - (2*sd_b0),
                                    state_year_short, ""))

front_edb_label_plot <- ggplot(b0_front) +
  geom_segment(aes(y = as.factor(cycle), yend = as.factor(cycle), 
                   x = `2.5%`*100, xend = `97.5%`*100,
                   color = front),
               position = pos) +
  geom_point(data = b0_front,
             aes(y = as.factor(cycle), x = mean*100, color = front), 
             position = pos) +
  geom_label_repel(data = b0_front, 
                   aes(x = mean*100, y = as.factor(cycle), 
                       label = state_year_short), color = "grey",
                   position = pos, min.segment.length = 0) +
  theme_bw() +
  labs(x = "Election day bias (%)", y = "Cycle")  +
  theme(text = element_text(size = 16), 
        legend.position = "NULL",
        plot.margin=grid::unit(c(2,10,0,2), "mm"),
        axis.title.y = element_text(angle = 90, vjust = 0.5)) +
  facet_wrap(~front, ncol = 4) +
  scale_color_manual(values = c("#0015BC","#DE0100", "darkgrey")) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkgrey") +
  xlim(-20, 14) +
  scale_y_discrete(expand = c(0.08, 0.08), limits = rev)  

ggsave(filename = '~/results_vis/us_senate_predictable/plots/front_edb_label_plot.png', 
       plot = front_edb_label_plot, 
       width = 14, height = 8, bg='#ffffff') 


# labeld edb by incumbency
b0_inc <- b0_inc %>% 
  mutate(state_year_short = if_else(mean > (mean_b0 + (2*sd_b0)) |
                                      mean < (mean_b0 - (2*sd_b0)),
                                    state_year_short, ""))

inc_edb_label_plot <- ggplot(b0_inc) +
  geom_segment(aes(y = as.factor(cycle), yend = as.factor(cycle), 
                   x = `2.5%`*100, xend = `97.5%`*100,
                   color = inc),
               position = pos) +
  geom_point(data = b0_inc,
             aes(y = as.factor(cycle), x = mean*100, color = inc), 
             position = pos) +
  geom_label_repel(data = b0_inc, 
                   aes(x = mean*100, y = as.factor(cycle), 
                       label = state_year_short), color = "grey",
                   position = pos, max.overlaps = 18,
                  min.segment.length = 0) +
  theme_bw() +
  labs(x = "Election day bias (%)", y = "Cycle")  +
  theme(text = element_text(size = 16), 
        legend.position = "NULL",
        plot.margin=grid::unit(c(2,10,0,2), "mm"),
        axis.title.y = element_text(angle = 90, vjust = 0.5)) +
  facet_wrap(~inc, ncol = 3) +
  scale_color_manual(values = c("#0015BC","#DE0100", "darkgrey")) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkgrey") +
  xlim(-20, 14) +
  scale_y_discrete(expand = c(0.08, 0.08), limits = rev) 

ggsave(filename = '~/results_vis/us_senate_predictable/plots/inc_edb_label_plot.png', 
       plot = inc_edb_label_plot, 
       width = 14, height = 8, bg='#ffffff') 

# edb labeld margin last poll plot
last_b0_label_plot <- ggplot() +
  geom_line(data = avg_exp_b0_last, aes(x = last_poll_margin*100, y = mean*100), 
            color = "darkgrey")  +
  geom_ribbon(data = avg_exp_b0_last, 
              aes(x = last_poll_margin*100, ymin = `2.5%`*100, 
                  ymax = `97.5%`*100), 
              fill = "darkgrey", alpha = 0.5) +
  geom_point(data = b0_last, aes(x = last_poll_margin*100, y = mean*100)) +
  geom_segment(data = b0_last, aes(x = last_poll_margin*100, 
                                   xend = last_poll_margin*100, 
                                   y = `2.5%`*100, yend = `97.5%`*100)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  geom_label_repel(data = b0_last %>% 
                     filter(mean > mean_b0 + (2*sd_b0)), 
                   aes(x = last_poll_margin*100, y = mean*100, 
                       label = ifelse(add_label == 1,state_year_short, "")),
                   nudge_y = 3, color = "grey",
                   nudge_x = 10) +
  geom_label_repel(data = b0_last %>% 
                     filter(mean < mean_b0 - (2*sd_b0)), 
                   aes(x = last_poll_margin*100, y = mean*100, 
                       label = ifelse(add_label == 1, state_year_short, "")),
                   nudge_y = -1, color = "grey",
                   nudge_x = 10) +
  geom_label_repel(data = b0_last %>% 
                     filter(last_poll_margin > mean_last + (2*sd_last) & 
                              !(mean < mean_b0 - (2*sd_b0)) & 
                              !(mean > mean_b0 + (2*sd_b0))), 
                   aes(x = last_poll_margin*100, y = mean*100, 
                       label = ifelse(add_label == 1,state_year_short, "")),
                   nudge_y = 0.0, color = "grey",
                   nudge_x = 20) +
  geom_label_repel(data = b0_last %>% 
                     filter(last_poll_margin < mean_last - (2*sd_last) & 
                              !(mean < mean_b0 - (2*sd_b0)) & 
                              !(mean > mean_b0 + (2*sd_b0))), 
                   aes(x = last_poll_margin*100, y = mean*100, 
                       label = ifelse(add_label == 1, state_year_short, "")),
                   nudge_y = 0.0, color = "grey",
                   nudge_x = -15) +
  theme_bw() +
  labs(x = "Margin last poll, Rep.-Dem. (%)", y = "Election day bias (%)")  +
  theme(text = element_text(size = 16),
        plot.margin=grid::unit(c(2,6,0,2), "mm"))

ggsave(filename = '~/results_vis/us_senate_predictable/plots/last_b0_label_plot.png', 
       plot = last_b0_label_plot, 
       width = 14, height = 8, bg='#ffffff') 


