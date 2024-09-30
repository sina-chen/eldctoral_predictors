#-------------------------------------------------------------------------------
#
# Electoral predictors - 
#   visualization minority absed on pictures and name
#
# Data: US Senate pre-election polls 1990 - 2022
#
#-------------------------------------------------------------------------------


# Libraries ---------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(cowplot)
library(grid)
library(gridExtra)
library(usdata)
library(stringi)


# Data --------------------------------------------------------------------

# name
b0_name <-  readRDS("~/results_vis/us_senate_predictable/us_senate_b0_minority_name.RDS")
avg_b0_name <- readRDS("~/results_vis/us_senate_predictable/us_senate_avg_b0_minority_name.RDS")

# name prob
b0_name_dem <- readRDS("~/results_vis/us_senate_predictable/us_senate_b0_minority_name_prob_dem.RDS")
avg_b0_name_dem  <- readRDS("~/results_vis/us_senate_predictable/us_senate_avg_exp_b0_name_prob_white_dem.RDS")
b0_name_rep <- readRDS("~/results_vis/us_senate_predictable/us_senate_b0_minority_name_prob_rep.RDS")
avg_b0_name_rep  <- readRDS("~/results_vis/us_senate_predictable/us_senate_avg_exp_b0_name_prob_white_rep.RDS")

# face
b0_face <-  readRDS("~/results_vis/us_senate_predictable/us_senate_b0_minority_face.RDS")
avg_b0_face <- readRDS("~/results_vis/us_senate_predictable/us_senate_avg_b0_minority_face.RDS")

# face prob
b0_face_dem <- readRDS("~/results_vis/us_senate_predictable/us_senate_b0_minority_face_prob_dem.RDS")
avg_b0_face_dem  <- readRDS("~/results_vis/us_senate_predictable/us_senate_avg_exp_b0_face_prob_white_dem.RDS")
b0_face_rep <- readRDS("~/results_vis/us_senate_predictable/us_senate_b0_minority_face_prob_rep.RDS")
avg_b0_face_rep  <- readRDS("~/results_vis/us_senate_predictable/us_senate_avg_exp_b0_face_prob_white_rep.RDS")


# Preparation -------------------------------------------------------------

# minority face
b0_face <- b0_face %>% 
  mutate(minority4_face = case_when(face_white_dem == 1 & face_white_rep == 0 ~ 
                                      "Dem. minority (face)",
                                    face_white_dem == 0 & face_white_rep == 1 ~ 
                                      "Rep. minority (face)",
                                    face_white_dem == 1 & face_white_rep == 1 ~ 
                                      "Both minority (face)",
                                    face_white_dem == 0 & face_white_rep == 0 ~ 
                                      "Neither minority (face)") %>% 
           factor(levels = c("Dem. minority (face)", 
                             "Rep. minority (face)", 
                             "Both minority (face)", 
                             "Neither minority (face)")) 
  )

avg_b0_face <- avg_b0_face %>% 
  mutate(minority4_face = case_when(feature == "minority_dem" ~ 
                                      "Dem. minority (face)",
                                    feature == "minority_rep" ~ 
                                      "Rep. minority (face)",
                                    feature == "minority_both" ~ 
                                      "Both minority (face)",
                                    feature == "minority_neither" ~ 
                                      "Neither minority (face)") %>% 
           factor(levels = c("Dem. minority (face)", 
                             "Rep. minority (face)", 
                             "Both minority (face)", 
                             "Neither minority (face)")),
         estimate = "Expected")

# minority name
b0_name <- b0_name %>% 
  mutate(minority4_name = case_when(minority_name_dem == 1 & 
                                      minority_name_rep == 0 ~ 
                                      "Dem. minority (name)",
                                    minority_name_dem == 0 &
                                      minority_name_rep == 1 ~ 
                                      "Rep. minority (name)",
                                    minority_name_dem == 1 &
                                      minority_name_rep == 1 ~ 
                                      "Both minority (name)",
                                    minority_name_dem == 0 & 
                                      minority_name_rep== 0 ~ 
                                      "Neither minority (name)") %>% 
           factor(levels = c("Dem. minority (name)", 
                             "Rep. minority (name)", 
                             "Both minority (name)", 
                             "Neither minority (name)")) 
  )

avg_b0_name <- avg_b0_name %>% 
  mutate(minority4_name = case_when(feature == "minority_dem" ~ 
                                      "Dem. minority (name)",
                                    feature == "minority_rep" ~ 
                                      "Rep. minority (name)",
                                    feature == "minority_both" ~ 
                                      "Both minority (name)",
                                    feature == "minority_neither" ~ 
                                      "Neither minority (name)") %>% 
           factor(levels = c("Dem. minority (name)", 
                             "Rep. minority (name)", 
                             "Both minority (name)", 
                             "Neither minority (name)")),
         estimate = "Expected")


# Plots -------------------------------------------------------------------


#### Name ####

# edb by minority name
minority_name_edb_plot <- ggplot(b0_name) +
  geom_segment(aes(y = as.factor(cycle), yend = as.factor(cycle), x = `2.5%`, xend = `97.5%`,
                   color = minority4_name),
               position = position_jitter(seed = 2022)) +
  geom_point(data = b0_name,
             aes(y = as.factor(cycle),x = mean, color = minority4_name), 
             position = position_jitter(seed = 2022)) +
  theme_bw() +
  labs(y = "", x = "")  +
  theme(text = element_text(size = 16), 
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "NULL",
        plot.margin=grid::unit(c(2,10,0,2), "mm"),
        axis.title.y = element_text(angle = 90, vjust = 0.5)) +
  facet_wrap(~minority4_name, ncol = 4) +
  scale_color_manual(values = c("#0015BC","#DE0100", "darkgrey",  "darkgrey")) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkgrey") +
  xlim(-0.20, 0.14) +
  scale_y_discrete(expand = c(0.08, 0.08), limits = rev) 

# average expected edb by minority
minority_name_avg_bias_plot <- ggplot(avg_b0_name) +
  geom_point(aes(y = estimate, x = mean, color = minority4_name)) +
  geom_segment(aes(y = estimate, yend = estimate, x = `2.5%`, xend = `97.5%`,
                   color = minority4_name)) +
  theme_bw() +
  facet_wrap(~minority4_name, ncol = 4)  +
  scale_color_manual(values = c("#0015BC","#DE0100", "darkgrey", "darkgrey"))  +
  theme(text = element_text(size = 16), 
        axis.text.x = element_text(size = 12),
        legend.position = "NULL",
        strip.text.x = element_blank(),
        plot.margin=grid::unit(c(2,10,0,2), "mm"),
        axis.title.y = element_text(angle = 90, vjust = 2))  +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkgrey") +
  labs(x = "Election-day bias (p.p.)", y = "")  +
  xlim(-0.20, 0.14)

# combine plots
minority_name_plot <- plot_grid(minority_name_edb_plot, NULL, minority_name_avg_bias_plot, 
                           ncol = 1, align = "v",  rel_heights = c(4, -0.2, 1))
rm(b0_name, avg_b0_name, minority_name_edb_plot, minority_name_avg_bias_plot)

# edb prob white name dem
name_prob_dem_edb_plot <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey") +
  geom_line(data = avg_b0_name_dem, aes(x = name_prob_white_dem, y = mean), 
            color = "#0015BC")  +
  geom_ribbon(data = avg_b0_name_dem, aes(x = name_prob_white_dem, ymin = `2.5%`, 
                                          ymax = `97.5%`), 
              fill = "#0015BC", alpha = 0.3) +
  geom_point(data = b0_name_dem, aes(x = name_prob_white_dem, y = mean), 
             color = "#0015BC") +
  geom_segment(data = b0_name_dem, aes(x = name_prob_white_dem, 
                                       xend = name_prob_white_dem, 
                                       y = `2.5%`, yend = `97.5%`), 
               color = "#0015BC") +  
  theme_bw() +
  labs(x= "Dem. prob. white (name)", y = "")  +
  theme(text = element_text(size = 16),
        plot.margin=grid::unit(c(2,10,0,12), "mm")) +
  xlim(0,1)

# edb prob white name rep
name_prob_rep_edb_plot <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey") +
  geom_line(data = avg_b0_name_rep, aes(x = name_prob_white_rep, y = mean), 
            color = "#DE0100")  +
  geom_ribbon(data = avg_b0_name_rep, aes(x = name_prob_white_rep, ymin = `2.5%`, 
                                          ymax = `97.5%`), 
              fill = "#DE0100", alpha = 0.3) +
  geom_point(data = b0_name_rep, aes(x = name_prob_white_rep, y = mean), 
             color = "#DE0100") +
  geom_segment(data = b0_name_rep, aes(x = name_prob_white_rep, 
                                       xend = name_prob_white_rep, 
                                       y = `2.5%`, yend = `97.5%`), 
               color = "#DE0100") +  
  theme_bw() +
  labs( x= "Rep. prob. white (name)", y = "")  +
  theme(text = element_text(size = 16),
        plot.margin=grid::unit(c(2,10,0,12), "mm"))  +
  xlim(0,1)


# name_prob_plot <- plot_grid(NULL, name_prob_dem_edb_plot, NULL, 
#                             name_prob_rep_edb_plot, ncol = 4, 
#                             rel_widths = c(0.055, 1,-0.055, 1)) +
#   draw_label("Probability white (name)", x = 0.5, y =  0, vjust =-0.5, 
#              angle = 0) +
#   draw_label("Election day bias", x =  0, y = 0.5, vjust = 1.5, angle = 90)
# 
# name_plot <- plot_grid(minority_name_plot, name_prob_plot, ncol = 1,
#                        rel_heights = c(1, 0.5))

name_plot <- plot_grid(minority_name_plot, 
                       name_prob_dem_edb_plot, 
                       name_prob_rep_edb_plot, ncol = 1,
                       rel_heights = c(1, 0.6, 0.6), labels = "AUTO",
                       label_x = 0.98) +
    draw_label("Election-day bias (p.p.)", x =  0.02, y = 0.3, vjust = 1.5, angle = 90)
rm(minority_name_plot, name_prob_dem_edb_plot, name_prob_rep_edb_plot, 
   avg_b0_name_dem, avg_b0_name_rep, b0_name_dem, b0_name_rep)

# save plot
ggsave(filename = '~/results_vis/us_senate_predictable/plots/b0_minority_name.png', 
       plot = name_plot, 
       width = 14, height = 11, bg='#ffffff') 
ggsave(filename = '~/results_vis/us_senate_predictable/plots/b0_minority_name.eps', 
       plot = name_plot, 
       width = 14, height = 11, bg='#ffffff', device=cairo_ps) 


#### Face ####

# edb by minority face
minority_face_edb_plot <- ggplot(b0_face) +
  geom_segment(aes(y = as.factor(cycle), yend = as.factor(cycle), x = `2.5%`, xend = `97.5%`,
                   color = minority4_face),
               position = position_jitter(seed = 2022)) +
  geom_point(data = b0_face,
             aes(y = as.factor(cycle),x = mean, color = minority4_face), 
             position = position_jitter(seed = 2022)) +
  theme_bw() +
  labs(y = "", x = "")  +
  theme(text = element_text(size = 16), 
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "NULL",
        plot.margin=grid::unit(c(2,10,0,2), "mm"),
        axis.title.y = element_text(angle = 90, vjust = 0.5)) +
  facet_wrap(~minority4_face, ncol = 4) +
  scale_color_manual(values = c("#0015BC","#DE0100", "darkgrey",  "darkgrey")) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkgrey") +
  xlim(-0.20, 0.14) +
  scale_y_discrete(expand = c(0.08, 0.08), limits = rev) 

# average expected edb by minority
minority_face_avg_bias_plot <- ggplot(avg_b0_face) +
  geom_point(aes(y = estimate, x = mean, color = minority4_face)) +
  geom_segment(aes(y = estimate, yend = estimate, x = `2.5%`, xend = `97.5%`,
                   color = minority4_face)) +
  theme_bw() +
  facet_wrap(~minority4_face, ncol = 4)  +
  scale_color_manual(values = c("#0015BC","#DE0100", "darkgrey", "darkgrey"))  +
  theme(text = element_text(size = 16), 
        axis.text.x = element_text(size = 12),
        legend.position = "NULL",
        strip.text.x = element_blank(),
        plot.margin=grid::unit(c(2,10,0,2), "mm"),
        axis.title.y = element_text(angle = 90, vjust = 2))  +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkgrey") +
  labs(x = "Election-day bias (p.p.)", y = "")  +
  xlim(-0.20, 0.14)

# combine plots
minority_face_plot <- plot_grid(minority_face_edb_plot, NULL, minority_face_avg_bias_plot, 
                                ncol = 1, align = "v",  rel_heights = c(4, -0.2, 1))

rm(b0_face, avg_b0_face, minority_face_edb_plot, minority_face_avg_bias_plot)

# edb prob white face dem
face_prob_dem_edb_plot <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey") +
  geom_line(data = avg_b0_face_dem, aes(x = 1-face_prob_white_dem, y = mean), 
            color = "#0015BC")  +
  geom_ribbon(data = avg_b0_face_dem, aes(x = 1-face_prob_white_dem, ymin = `2.5%`, 
                                            ymax = `97.5%`), 
              fill = "#0015BC", alpha = 0.3) +
  geom_point(data = b0_face_dem, aes(x = 1-face_prob_white_dem, y = mean), 
             color = "#0015BC") +
  geom_segment(data = b0_face_dem, aes(x = 1-face_prob_white_dem, 
                                       xend = 1-face_prob_white_dem, 
                                     y = `2.5%`, yend = `97.5%`), 
               color = "#0015BC") +  
  theme_bw() +
  labs(x = "Dem. prob. white (face)", y = "")  +
  theme(text = element_text(size = 16),
        plot.margin=grid::unit(c(2,10,0,12), "mm"),
        plot.title = element_text(hjust = 0.5))
  
# edb prob white face dem
face_prob_rep_edb_plot <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey") +
  geom_line(data = avg_b0_face_rep, aes(x = 1-face_prob_white_rep, y = mean), 
            color = "#DE0100")  +
  geom_ribbon(data = avg_b0_face_rep, aes(x = 1-face_prob_white_rep, ymin = `2.5%`, 
                                          ymax = `97.5%`), 
              fill = "#DE0100", alpha = 0.3) +
  geom_point(data = b0_face_rep, aes(x = 1-face_prob_white_rep, y = mean), 
             color = "#DE0100") +
  geom_segment(data = b0_face_rep, aes(x = 1-face_prob_white_rep, 
                                       xend = 1-face_prob_white_rep, 
                                       y = `2.5%`, yend = `97.5%`), 
               color = "#DE0100") +  
  theme_bw() +
  labs( x= "Rep. prob. white (face)", y = "")  +
  theme(text = element_text(size = 16),
        plot.margin=grid::unit(c(2,10,0,12), "mm"),
        plot.title = element_text(hjust = 0.5))

face_plot <- plot_grid(minority_face_plot, 
                       face_prob_dem_edb_plot, 
                       face_prob_rep_edb_plot, ncol = 1,
                       rel_heights = c(1, 0.6, 0.6),
                       labels = "AUTO", label_x = 0.98) +
  draw_label("Election-day bias (p.p.)", x =  0.02, y = 0.3, vjust = 1.5, angle = 90)
rm(avg_b0_face_dem, avg_b0_face_rep, b0_face_dem, b0_face_rep, 
   minority_face_plot, face_prob_dem_edb_plot, face_prob_rep_edb_plot)

# save plot
ggsave(filename = '~/results_vis/us_senate_predictable/plots/b0_minority_face.png', 
       plot = face_plot, 
       width = 14, height = 8, bg='#ffffff') 
ggsave(filename = '~/results_vis/us_senate_predictable/plots/b0_minority_face.eps', 
       plot = face_plot, 
       width = 14, height = 8, bg='#ffffff', device=cairo_ps) 

