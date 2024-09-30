#-------------------------------------------------------------------------------
#
# Electoral predictors - 
#   visualization race & gender
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
  library(stringi)
}

# Data --------------------------------------------------------------------

b0_race <- readRDS("~/results_vis/us_senate_predictable/us_senate_b0_minority.RDS")
avg_b0_race <- readRDS("~/results_vis/us_senate_predictable/us_senate_avg_b0_minority.RDS")
b0_gender <- readRDS("~/results_vis/us_senate_predictable/us_senate_b0_gender.RDS")
avg_b0_gender <- readRDS("~/results_vis/us_senate_predictable/us_senate_avg_b0_gender.RDS")
# b0_minority_population <- readRDS("~/results_vis/us_senate_predictable/us_senate_b0_minority_population.RDS")
# avg_exp_b0_minority_population <- readRDS("~/results_vis/us_senate_predictable/us_senate_avg_exp_b0_minority_population.RDS")


# Preparation -------------------------------------------------------------

# specify jitter position+
pos <- position_jitter(height = 0.5, width = 0, seed = 2022)

# minority
b0_race <- b0_race %>% 
  mutate(minority4 = factor(minority4, levels = c("Dem. minority", 
                                                "Rep. minority",
                                                "Both minority",
                                                "Neither minority")),
         mean_b0 = mean(mean),
         sd_b0 = sd(mean),
         state_year_short = if_else(mean > mean_b0 + (2*sd_b0) |
                                      mean < mean_b0 - (2*sd_b0),
                                    paste0(state2abbr(state), cycle), "")
         )

avg_b0_race <- avg_b0_race %>% 
  mutate(minority4 = case_when(feature == "minority_dem" ~ "Dem. minority",
                               feature == "minority_rep" ~ "Rep. minority",
                               feature == "minority_both" ~ "Both minority",
                               feature == "minority_neither" ~ 
                                 "Neither minority") %>% 
           factor(levels = c("Dem. minority", "Rep. minority", "Both minority", 
                             "Neither minority")),
         estimate = "Expected")

# gender            
b0_gender <- b0_gender %>% 
  mutate(gender4 = factor(gender4, levels = c("Dem. female", 
                                              "Rep. female",
                                              "Both female",
                                              "Neither female")),
         mean_b0 = mean(mean),
         sd_b0 = sd(mean),
         state_year_short = if_else(mean > mean_b0 + (2*sd_b0) |
                                      mean < mean_b0 - (2*sd_b0),
                                    paste0(state2abbr(state), cycle), "")
         )


avg_b0_gender <- avg_b0_gender %>% 
  mutate(gender4 = case_when(feature == "female_dem" ~ "Dem. female",
                               feature == "female_rep" ~ "Rep. female",
                               feature == "female_both" ~ "Both female",
                               feature == "minority_female" ~ 
                                 "Neither female") %>% 
           factor(levels = c("Dem. female", "Rep. female", "Both female", 
                             "Neither female")),
         estimate = "Expected")



# Plots -------------------------------------------------------------------

# edb by minority
minority_edb_plot <- ggplot(b0_race) +
  geom_segment(aes(y = as.factor(cycle), yend = as.factor(cycle), x = `2.5%`*100, xend = `97.5%`*100,
                   color = minority4),
               position = position_jitter(seed = 2022)) +
  geom_point(data = b0_race,
             aes(y = as.factor(cycle), x = mean*100, color = minority4), 
             position = position_jitter(seed = 2022)) +
  theme_bw() +
  labs(y = "", x = "")  +
  theme(text = element_text(size = 16), 
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "NULL",
        plot.margin=grid::unit(c(2,10,0,2), "mm"),
        axis.title.y = element_text(angle = 90, vjust = 0.5)) +
  facet_wrap(~minority4, ncol = 4) +
  scale_color_manual(values = c("#0015BC","#DE0100", "darkgrey",  "darkgrey")) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkgrey") +
  xlim(-20, 14) +
  scale_y_discrete(expand = c(0.08, 0.08), limits = rev) 

# average expected edb by minority
minority_avg_bias_plot <- ggplot(avg_b0_race) +
  geom_point(aes(y = estimate, x = mean*100, color = minority4)) +
  geom_segment(aes(y = estimate, yend = estimate, x = `2.5%`*100, 
                   xend = `97.5%`*100,
                   color = minority4)) +
  theme_bw() +
  facet_wrap(~minority4, ncol = 4)  +
  scale_color_manual(values = c("#0015BC","#DE0100", "darkgrey", "darkgrey"))  +
  theme(text = element_text(size = 16), 
        axis.text.x = element_text(size = 12),
        legend.position = "NULL",
        strip.text.x = element_blank(),
        plot.margin=grid::unit(c(2,10,0,2), "mm"),
        axis.title.y = element_text(angle = 90, vjust = 2))  +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkgrey") +
  labs(x = "Election-day bias (p.p.)", y = "")  +
  xlim(-20, 14)


minority_plot <- plot_grid(minority_edb_plot, NULL, minority_avg_bias_plot, 
                      ncol = 1, align = "v",  rel_heights = c(4, -0.22, 1))


# edb by gender
gender_edb_plot <- ggplot(b0_gender) +
  geom_segment(aes(y = as.factor(cycle), yend = as.factor(cycle), 
                   x = `2.5%`*100, xend = `97.5%`*100,
                   color = gender4),
               position = position_jitter(seed = 2022)) +
  geom_point(data = b0_gender,
             aes(y = as.factor(cycle),x = mean*100, color = gender4), 
             position = position_jitter(seed = 2022)) +
  theme_bw() +
  labs(x = "", y = "")  +
  theme(text = element_text(size = 16), 
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "NULL",
        plot.margin=grid::unit(c(2,10,0,2), "mm"),
        axis.title.y = element_text(angle = 90, vjust = 0.5)) +
  facet_wrap(~gender4, ncol = 4) +
  scale_color_manual(values = c("#0015BC", "#DE0100","darkgrey", "darkgrey")) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkgrey") +
  xlim(-20, 14) +
  scale_y_discrete(expand = c(0.08, 0.08), limits = rev)

# average edb by gender
avg_b0_gender_plot <- ggplot(avg_b0_gender) +
  geom_point(aes(y = estimate, x = mean*100, color = gender4)) +
  geom_segment(aes(y = estimate, yend = estimate, x = `2.5%`*100, 
                   xend = `97.5%`*100, color = gender4)) +
  theme_bw() +
  facet_wrap(~gender4, ncol = 4)  +
  scale_color_manual(values = c("#0015BC","#DE0100", "darkgrey", "darkgrey"))  +
  theme(text = element_text(size = 16), 
        axis.text.x = element_text(size = 12),
        legend.position = "NULL",
        strip.text.x = element_blank(),
        plot.margin=grid::unit(c(2,10,0,2), "mm"),
        axis.title.y = element_text(angle = 90, vjust = 2))  +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkgrey") +
  labs(x = "Election-day bias (p.p.)", y = "")  +
  xlim(-20, 14)

gender_plot <- plot_grid(gender_edb_plot, NULL, avg_b0_gender_plot, 
                      ncol = 1, align = "v",  rel_heights = c(4, -0.22, 1))

minority_gender_plot <- plot_grid(minority_plot, gender_plot, ncol = 1, 
                                  rel_heights = c(1,1), labels = "AUTO",
                                  label_x = 0.97)

ggsave(filename = '~/results_vis/us_senate_predictable/plots/b0_minority_gender.png', 
       plot = minority_gender_plot, 
       width = 14, height = 10, bg='#ffffff') 
ggsave(filename = '~/results_vis/us_senate_predictable/plots/b0_minority_gender.eps', 
       plot = minority_gender_plot, 
       width = 14, height = 10, bg='#ffffff') 

# labeld edb by minority
minority_edb_label_plot <- ggplot(b0_race) +
  geom_segment(aes(y = as.factor(cycle), yend = as.factor(cycle), 
                   x = `2.5%`*100, xend = `97.5%`*100,
                   color = minority4),
               position = pos) +
  geom_point(data = b0_race,
             aes(y = as.factor(cycle), x = mean*100, color = minority4), 
             position = pos) +
  geom_label_repel(data = b0_race, 
                   aes(x = mean*100, y = as.factor(cycle), 
                       label = state_year_short), color = "grey",
                   position = pos, max.overlaps = 15,
                   min.segment.length = 0) +
  theme_bw() +
  labs(x = "Election day bias (p.p.)", y = "Cycle")  +
  theme(text = element_text(size = 16),
        legend.position = "NULL",
        plot.margin=grid::unit(c(2,10,0,2), "mm"),
        axis.title.y = element_text(angle = 90, vjust = 0.5)) +
  facet_wrap(~minority4, ncol = 4) +
  scale_color_manual(values = c("#0015BC","#DE0100", "darkgrey", "darkgrey")) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkgrey") +
  xlim(-25, 25) +
  scale_y_discrete(expand = c(0.05, 0.05), limits = rev)  + 
  scale_alpha_discrete(range = c(0, 1)) 

ggsave(filename = '~/results_vis/us_senate_predictable/plots/edb_minority_label_plot.png', 
       plot = minority_edb_label_plot, 
       width = 14, height = 8, bg='#ffffff') 


# labeled edb by gender
gender_edb_label_plot <- ggplot(b0_gender) +
  geom_segment(aes(y = as.factor(cycle), yend = as.factor(cycle), x = `2.5%`, xend = `97.5%`,
                   color = gender4),
               position = pos) +
  geom_point(data = b0_gender,
             aes(y = as.factor(cycle),x = mean, color = gender4), 
             position = pos) +
  geom_label_repel(data = b0_gender, 
                   aes(x = mean, y = as.factor(cycle), 
                       label = state_year_short), color = "grey",
                   position = pos, max.overlaps = 155,
                   min.segment.length = 0) +
  theme_bw() +
  labs(x = "Election day bias (p.p.)", y = "Cycle")  +
  theme(text = element_text(size = 16),
        legend.position = "NULL",
        plot.margin=grid::unit(c(2,10,0,2), "mm"),
        axis.title.y = element_text(angle = 90, vjust = 0.5)) +
  facet_wrap(~gender4, ncol = 4) +
  scale_color_manual(values = c("#0015BC","#DE0100", "darkgrey", "darkgrey")) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkgrey") +
  xlim(-0.25, 0.25) +
  scale_y_discrete(expand = c(0.05, 0.05), limits = rev)  + 
  scale_alpha_discrete(range = c(0, 1)) 

ggsave(filename = '~/results_vis/us_senate_predictable/plots/edb_gender_label_plot.png', 
       plot = gender_edb_label_plot, 
       width = 14, height = 8, bg='#ffffff') 




