#-------------------------------------------------------------------------------
#
# Electoral predictors - 
#   visualization CF Score & CF Score distance
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
  library(ggrepel)
}


# Data --------------------------------------------------------------------

b0_cf_dem <- readRDS("~/results_vis/us_senate_predictable/us_senate_b0_cf_dem.RDS")
avg_exp_b0_cf_dem <- readRDS("~/results_vis/us_senate_predictable/us_senate_avg_exp_b0_cf_dem.RDS")
b0_cf_rep <- readRDS("~/results_vis/us_senate_predictable/us_senate_b0_cf_rep.RDS")
avg_exp_b0_cf_rep <- readRDS("~/results_vis/us_senate_predictable/us_senate_avg_exp_b0_cf_rep.RDS")
ex_sd_dist <- readRDS("~/results_vis/us_senate_predictable/us_senate_ex_sd_cf_dist.RDS")
exp_ex_sd_dist <- readRDS("~/results_vis/us_senate_predictable/us_senate_exp_ex_sd_cf_dist.RDS")


# Preparation -------------------------------------------------------------

# specify jitter position+
pos <- position_jitter(height = 0.4, width = 0, seed = 2022)

# edb cf score dem
b0_cf_dem <- b0_cf_dem %>% 
  mutate(mean_b0 = mean(mean),
         sd_b0 = sd(mean), 
         mean_cf = mean(cf_score_dem),
         sd_cf = sd(cf_score_dem),
         add_label = if_else(mean > mean_b0 + (2*sd_b0) |
                               mean < mean_b0 - (2*sd_b0) |
                               cf_score_dem > mean_cf + (2*sd_cf) |
                               cf_score_dem < mean_cf - (2*sd_cf), 1, 0),
         state_year_short = paste0(state2abbr(state), " ", cycle))

# edb cf score rep
b0_cf_rep <- b0_cf_rep %>% 
  mutate(mean_b0 = mean(mean),
         sd_b0 = sd(mean), 
         mean_cf = mean(cf_score_rep),
         sd_cf = sd(cf_score_rep),
         add_label = if_else(mean > mean_b0 + (2*sd_b0) |
                               mean < mean_b0 - (2*sd_b0) |
                               cf_score_rep > mean_cf + (2*sd_cf) |
                               cf_score_rep < mean_cf - (2*sd_cf), 1, 0),
         state_year_short = paste0(state2abbr(state), " ", cycle))

# ex sd cf dist
ex_sd_dist <- ex_sd_dist %>% 
  mutate(cf_dist = cf_score_rep - cf_score_dem,
         mean_ex = mean(mean),
         sd_ex = sd(mean), 
         mean_cf = mean(cf_dist),
         sd_cf = sd(cf_dist),
         add_label = if_else(mean > mean_ex + (2*sd_ex) |
                               mean < mean_ex - (2*sd_ex) |
                               cf_dist > mean_cf + (2*sd_cf) |
                               cf_dist < mean_cf - (2*sd_cf), 1, 0),
         state_year_short = paste0(state2abbr(state), " ", cycle))

# exp. ex sd cf dist
exp_ex_sd_dist <- exp_ex_sd_dist %>% 
  mutate(cf_dist = cf_score_rep - cf_score_dem)


# Plots -------------------------------------------------------------------

# edb cf score plot
cf_edb_plot <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey") +
  geom_line(data = avg_exp_b0_cf_dem, aes(x = cf_score_dem, y = mean*100, 
                                          color = "Dem."))  +
  geom_ribbon(data = avg_exp_b0_cf_dem, aes(x = cf_score_dem, ymin = `2.5%`*100, 
                                            ymax = `97.5%`*100, fill = "Dem."), 
              alpha = 0.3) +
  geom_line(data = avg_exp_b0_cf_rep, aes(x = cf_score_rep, y = mean*100, 
                                          color = "Rep."))  +
  geom_ribbon(data = avg_exp_b0_cf_rep, aes(x = cf_score_rep, ymin = `2.5%`*100, 
                                            ymax = `97.5%`*100, fill = "Rep."), 
              alpha = 0.3) +
  geom_point(data = b0_cf_dem, aes(x = cf_score_dem, y = mean*100, 
                                   color = "Dem.")) +
  geom_segment(data = b0_cf_dem, aes(x = cf_score_dem, xend = cf_score_dem, 
                                         y = `2.5%`*100, yend = `97.5%`*100, 
                                     color = "Dem.")) +
  geom_point(data = b0_cf_rep, 
             aes(x = cf_score_rep, y = mean*100, color = "Rep.")) +
  geom_segment(data = b0_cf_rep, aes(x = cf_score_rep, xend = cf_score_rep, 
                                     y = `2.5%`*100, yend = `97.5%`*100, 
                                     color = "Rep.")) +  
  theme_bw() +
  labs(x = "CFscore", y = "Election-day bias (%)")  +
  theme(text = element_text(size = 16),
        plot.margin=grid::unit(c(2,10,0,2), "mm"),
        legend.position = c(0.95, 0.2),
        legend.background=element_blank())  +
  scale_color_manual(values = c("#0015BC","#DE0100"), name = "") +
  scale_fill_manual(values = c("#0015BC","#DE0100"), name = "")

# excess SD cf score distance plot
cf_dist_ex_sd_plot <- ggplot() +
  geom_line(data = exp_ex_sd_dist, aes(x = cf_dist, y = mean*100), 
            color = "darkgrey")  +
  geom_ribbon(data = exp_ex_sd_dist, 
              aes(x = cf_dist, ymin = `2.5%`*100, ymax = `97.5%`*100), 
              fill = "darkgrey", alpha = 0.5) +
  geom_point(data = ex_sd_dist, aes(x = cf_dist, y = mean*100)) +
  geom_segment(data = ex_sd_dist, aes(x = cf_dist, xend = cf_dist, 
                                     y = `2.5%`*100, yend = `97.5%`*100)) +
  theme_bw() +
  labs(x = "CFscore distance (Rep.-Dem.)", y = "Excess SD (%)")  +
  theme(text = element_text(size = 16),
        plot.margin=grid::unit(c(2,10,0,2), "mm"))

# combine plots
cf_dist_plot <- plot_grid(cf_edb_plot, cf_dist_ex_sd_plot, ncol = 1, 
                                  rel_widths = c(1,1), labels = "AUTO",
                                  label_x = 0.98, align = "v", axis = "lr")

# save plots
ggsave(filename = '~/results_vis/us_senate_predictable/plots/b0_cf_ex_sd_cf_dist.png', 
       plot = cf_dist_plot, 
       width = 14, height = 6, bg='#ffffff') 
ggsave(filename = '~/results_vis/us_senate_predictable/plots/b0_cf_ex_sd_cf_dist.eps', 
       plot = cf_dist_plot, 
       width = 14, height = 6, bg='#ffffff', device=cairo_ps) 


# edb cf score cycle plot
cf_edb_cycle_plot <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey") +
  geom_point(data = b0_cf_dem, 
             aes(x = cf_score_dem, y = mean*100, color = "Dem.")) +
  geom_segment(data = b0_cf_dem, aes(x = cf_score_dem, xend = cf_score_dem, 
                                     y = `2.5%`*100, yend = `97.5%`*100, 
                                     color = "Dem.")) +
  geom_point(data = b0_cf_rep, 
             aes(x = cf_score_rep, y = mean*100, color = "Rep.")) +
  geom_segment(data = b0_cf_rep, aes(x = cf_score_rep, xend = cf_score_rep, 
                                     y = `2.5%`*100, yend = `97.5%`*100, 
                                     color = "Rep.")) +  
  theme_bw() +
  labs(x = "CFscore", y = "Election-day bias (%)")  +
  theme(text = element_text(size = 16),
        plot.margin=grid::unit(c(2,10,0,2), "mm"),
        legend.position = "bottom",
        legend.background=element_blank())  +
  scale_color_manual(values = c("#0015BC","#DE0100"), name = "") +
  scale_fill_manual(values = c("#0015BC","#DE0100"), name = "") +
  facet_wrap(~cycle, ncol = 4)

# save
ggsave(filename = '~/results_vis/us_senate_predictable/plots/edb_cf_score_cycle.png', 
       plot = cf_edb_cycle_plot, 
       width = 14, height = 8, bg='#ffffff') 
ggsave(filename = '~/results_vis/us_senate_predictable/plots/edb_cf_score_cycle.eps', 
       plot = cf_edb_cycle_plot, 
       width = 14, height = 8, bg='#ffffff', dpi = 320) 

# excess SD cf score distance cycle plot
cf_dist_ex_sd_cycle_plot <- ggplot() +
  geom_point(data = ex_sd_dist, aes(x = cf_dist, y = mean*100)) +
  geom_segment(data = ex_sd_dist, aes(x = cf_dist, xend = cf_dist, 
                                      y = `2.5%`*100, yend = `97.5%`*100)) +
  theme_bw() +
  labs(x = "CFscore distance (Rep.-Dem.)", y = "Excess SD (%)")  +
  theme(text = element_text(size = 16),
        plot.margin=grid::unit(c(2,10,0,2), "mm"))  +
  facet_wrap(~cycle, ncol = 4)

# save
ggsave(filename = '~/results_vis/us_senate_predictable/plots/ex_sd_cf_dist_cycle.png', 
       plot = cf_dist_ex_sd_cycle_plot, 
       width = 14, height = 6, bg='#ffffff') 
ggsave(filename = '~/results_vis/us_senate_predictable/plots/ex_sd_cf_dist_cycle.eps', 
       plot = cf_dist_ex_sd_cycle_plot, 
       width = 14, height = 6, bg='#ffffff') 


# edb cf score dem labeld plot
cf_dem_edb_labeled_plot <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey") +
  geom_line(data = avg_exp_b0_cf_dem, aes(x = cf_score_dem, y = mean*100), 
            color = "grey")  +
  geom_ribbon(data = avg_exp_b0_cf_dem, aes(x = cf_score_dem, ymin = `2.5%`*100, 
                                            ymax = `97.5%`*100), 
              alpha = 0.3, fill = "grey") +
  geom_point(data = b0_cf_dem, aes(x = cf_score_dem, y = mean*100)) +
  geom_segment(data = b0_cf_dem, aes(x = cf_score_dem, xend = cf_score_dem, 
                                     y = `2.5%`*100, yend = `97.5%`*100)) +
  geom_label_repel(data = b0_cf_dem %>% 
                     filter(mean > mean_b0 + (2*sd_b0)), 
                   aes(x = cf_score_dem, y = mean*100, 
                       label = ifelse(add_label == 1, state_year_short, "")),
                   nudge_y = 3, color = "grey",
                   nudge_x = 0.1) +
  geom_label_repel(data = b0_cf_dem %>% 
                     filter(mean < mean_b0 - (2*sd_b0)), 
                   aes(x = cf_score_dem, y = mean*100, 
                       label = ifelse(add_label == 1,state_year_short, "")),
                   nudge_y = -3, color = "grey",
                   nudge_x = 0.1) +
  geom_label_repel(data = b0_cf_dem %>% 
                     filter(cf_score_dem > mean_cf + (2*sd_cf) & 
                              !(mean < mean_b0 - (2*sd_b0)) & 
                              !(mean > mean_b0 + (2*sd_b0))), 
                   aes(x = cf_score_dem, y = mean*100, 
                       label = ifelse(add_label == 1,state_year_short, "")),
                   nudge_y = 0.0, color = "grey",
                   nudge_x = 0.15) +
  geom_label_repel(data = b0_cf_dem %>% 
                     filter(cf_score_dem < mean_cf - (2*sd_cf) & 
                              !(mean < mean_b0 - (2*sd_b0)) & 
                              !(mean > mean_b0 + (2*sd_b0))), 
                   aes(x = cf_score_dem, y = mean*100, 
                       label = ifelse(add_label == 1,state_year_short, "")),
                   nudge_y = 0.0, color = "grey",
                   nudge_x = -0.10) +  
  theme_bw() +
  labs(x = "CFscore Dem.", y = "Election-day bias (%)")  +
  theme(text = element_text(size = 16),
        plot.margin=grid::unit(c(2,10,0,2), "mm"),
        legend.position = c(0.95, 0.2),
        legend.background=element_blank()) 

ggsave(filename = '~/results_vis/us_senate_predictable/plots/cf_dem_edb_labeled_plot.png', 
       plot = cf_dem_edb_labeled_plot, 
       width = 14, height = 8, bg='#ffffff') 


# edb cf score rep labeld plot
cf_rep_edb_labeled_plot <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey") +
  geom_line(data = avg_exp_b0_cf_rep, aes(x = cf_score_rep, y = mean*100), 
            color = "grey")  +
  geom_ribbon(data = avg_exp_b0_cf_rep, aes(x = cf_score_rep, ymin = `2.5%`*100, 
                                            ymax = `97.5%`*100), 
              alpha = 0.3, fill = "grey") +
  geom_point(data = b0_cf_rep, aes(x = cf_score_rep, y = mean*100)) +
  geom_segment(data = b0_cf_rep, aes(x = cf_score_rep, xend = cf_score_rep, 
                                     y = `2.5%`*100, yend = `97.5%`*100)) +
  geom_label_repel(data = b0_cf_rep %>% 
                     filter(mean > mean_b0 + (2*sd_b0)), 
                   aes(x = cf_score_rep, y = mean*100, 
                       label = ifelse(add_label == 1, state_year_short, "")),
                   nudge_y = 2, color = "grey",
                   nudge_x = 0.1) +
  geom_label_repel(data = b0_cf_rep %>% 
                     filter(mean < mean_b0 - (2*sd_b0)), 
                   aes(x = cf_score_rep, y = mean*100, 
                       label = ifelse(add_label == 1,state_year_short, "")),
                   nudge_y = -3, color = "grey",
                   nudge_x = 0.1) +
  geom_label_repel(data = b0_cf_rep %>% 
                     filter(cf_score_rep > mean_cf + (2*sd_cf) & 
                              !(mean < mean_b0 - (2*sd_b0)) & 
                              !(mean > mean_b0 + (2*sd_b0))), 
                   aes(x = cf_score_rep, y = mean*100, 
                       label = ifelse(add_label == 1,state_year_short, "")),
                   nudge_y = 0.0, color = "grey",
                   nudge_x = 0.1) +
  geom_label_repel(data = b0_cf_rep %>% 
                     filter(cf_score_rep < mean_cf - (2*sd_cf) & 
                              !(mean < mean_b0 - (2*sd_b0)) & 
                              !(mean > mean_b0 + (2*sd_b0))), 
                   aes(x = cf_score_rep, y = mean*100, 
                       label = ifelse(add_label == 1,state_year_short, "")),
                   nudge_y = 0.0, color = "grey",
                   nudge_x = -0.2) +  
  theme_bw() +
  labs(x = "CFscore Rep.", y = "Election-day bias (%)")  +
  theme(text = element_text(size = 16),
        plot.margin=grid::unit(c(2,10,0,2), "mm"),
        legend.position = c(0.95, 0.2),
        legend.background=element_blank()) 

ggsave(filename = '~/results_vis/us_senate_predictable/plots/cf_rep_edb_labeled_plot.png', 
       plot = cf_rep_edb_labeled_plot, 
       width = 14, height = 8, bg='#ffffff') 


# ex sd cf score dist labeld plot
cf_dist_ex_sd_labeled_plot <- ggplot() +
  geom_line(data = exp_ex_sd_dist, aes(x = cf_dist, y = mean*100), 
            color = "grey")  +
  geom_ribbon(data = exp_ex_sd_dist, aes(x = cf_dist, ymin = `2.5%`*100, 
                                            ymax = `97.5%`*100), 
              alpha = 0.3, fill = "grey") +
  geom_point(data = ex_sd_dist, aes(x = cf_dist, y = mean*100)) +
  geom_segment(data = ex_sd_dist, aes(x = cf_dist, xend = cf_dist, 
                                     y = `2.5%`*100, yend = `97.5%`*100)) +
  geom_label_repel(data = ex_sd_dist %>% 
                     filter(mean > mean_ex + (2*sd_ex)), 
                   aes(x = cf_dist, y = mean*100, 
                       label = ifelse(add_label == 1, state_year_short, "")),
                   nudge_y = 0.7, color = "grey",
                   nudge_x = 0.15) +
  geom_label_repel(data = ex_sd_dist %>% 
                     filter(mean < (mean_ex - (2*sd_ex))), 
                   aes(x = cf_dist, y = mean*100, 
                       label = ifelse(add_label == 1,state_year_short, "")),
                   nudge_y = -1, color = "grey",
                   nudge_x = 0.05) +
  geom_label_repel(data = ex_sd_dist %>% 
                     filter(cf_dist > mean_cf + (2*sd_cf) & 
                              !(mean < mean_ex - (2*sd_ex)) & 
                              !(mean > mean_ex + (2*sd_ex))), 
                   aes(x = cf_dist, y = mean*100, 
                       label = ifelse(add_label == 1,state_year_short, "")),
                   nudge_y = -0.5, color = "grey",
                   nudge_x = 0.2) +
  geom_label_repel(data = ex_sd_dist %>% 
                     filter(cf_dist < mean_cf - (2*sd_cf) & 
                              !(mean < mean_ex - (2*sd_ex)) & 
                              !(mean > mean_ex + (2*sd_ex))), 
                   aes(x = cf_dist, y = mean*100, 
                       label = ifelse(add_label == 1,state_year_short, "")),
                   nudge_y = -0.5, color = "grey",
                   nudge_x = -0.2) +  
  theme_bw() +
  labs(x = "CFscore distance (Rep. - Dem.)", y = "Excess SD (%)")  +
  theme(text = element_text(size = 16),
        plot.margin=grid::unit(c(2,10,0,2), "mm")) 

ggsave(filename = '~/results_vis/us_senate_predictable/plots/cf_dist_ex_sd_labeled_plot.png', 
       plot = cf_dist_ex_sd_labeled_plot, 
       width = 14, height = 8, bg='#ffffff') 



