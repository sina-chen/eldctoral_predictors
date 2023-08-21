#-------------------------------------------------------------------------------
#
# Electoral predictors - Figure 1: Observed TSE distributions & 
#     estimated election-day bias and standard deviation
#
# Author: Sina Chen
#
#-------------------------------------------------------------------------------


# Libraries ---------------------------------------------------------------

{
  library(ggplot2)
  library(dplyr)
  library(ggpubr)
  library(ggstance)
  library(ggridges)
  library(cowplot)
}


# Data --------------------------------------------------------------------

# polls
polls <- readRDS("~/data/us/senate/us_senate_polls1990_2022_context_finance.RDS")


b0_summary <- readRDS("~/results_vis/us_senate_predictable/bias_senate1990_2022_empty.RDS")
v0_r_summary <- readRDS("~/results_vis/us_senate_predictable/v0_senate1990_2022_empty.RDS")


# Functions ---------------------------------------------------------------

# generate plot by cycle 
plot_cycle <- function(year, margin = unit(c(-1.1, 0.6, 0.5, 0.5), "cm"), 
                       xaxis_labels = F) {
  if(xaxis_labels == F){
    cycle_data <- plot_data %>% filter(cycle == year)
    plot <- ggplot() +
      mapply(function(mean, sd) {
        stat_function(fun = dnorm, args = list(mean = mean, sd = sd),
                      color = "grey30")
      }, 
      mean = cycle_data$mean_est,
      sd = sqrt(cycle_data$var_est)
      ) +
      scale_x_continuous(breaks = c(-0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3), 
                         limits = c(-0.35, 0.35)) +
      theme_bw() +
      labs(y = "", x = "") +
      theme(axis.text = element_blank(),
            plot.margin = margin, 
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.ticks.y = element_blank()) +
      geom_boxploth(data = cycle_data, inherit.aes = F, width = 1.7,
                    aes(y = -1, x = mean_est), varwidth = F) +
      geom_vline(xintercept = 0, linetype = "dashed") +
      coord_cartesian(xlim = c(-0.35, 0.35))
    
  } else if (xaxis_labels == T){
    cycle_data <- plot_data %>% filter(cycle == year)
    plot <- ggplot() +
      mapply(function(mean, sd) {
        stat_function(fun = dnorm, args = list(mean = mean, sd = sd),
                      color = "grey30")
      }, 
      mean = cycle_data$mean_est,
      sd = sqrt(cycle_data$var_est)
      ) +
      scale_x_continuous(breaks = c(-0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3), 
                         limits = c(-0.35, 0.35)) +
      theme_bw() +
      labs(y = "", x = "") +
      theme(axis.text.y = element_blank(),
            plot.margin = margin,
            axis.text.x = element_text(size = 16), 
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.ticks.y = element_blank()) +
      geom_boxploth(data = cycle_data, inherit.aes = F, width = 1.7,
                    aes(y = -1, x = mean_est), varwidth = F) +
      geom_vline(xintercept = 0, linetype = "dashed") +
      coord_cartesian(xlim = c(-0.35, 0.35))
  }
  
  return(plot)
}

# plot cycle tse
plot_cycle_tse <- function(year, margin = unit(c(-1.1, 0.6, 0.5, 0.5), "cm"), 
                           xaxis_labels = F, plot_data = polls) {
  if(xaxis_labels == F){
    cycle_data <- plot_data %>% filter(cycle == year)
    plot <- ggplot() +
      geom_density_ridges(data = plot_data %>% filter(cycle == year), 
                          aes(x = pct2_rep - vote2_rep, y = as.factor(cycle), 
                              color = state),
                          scale = 0.75, fill = NA, 
                          bandwidth = 0.0155) +
      theme_bw() +
      geom_vline(xintercept = 0, linetype = "dashed") + 
      scale_y_discrete(limits=rev) +
      labs(y = "", x = "") +
      theme(legend.position = "none",
            axis.text = element_blank(),
            axis.text.y = element_text(size = 16, vjust = -1.4),
            plot.margin = margin,
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()) +
      scale_color_manual(guide = "none", values = rep("grey30", 50)) +
      geom_boxploth(data = plot_data %>% filter(cycle == year), 
                    inherit.aes = F, width = 2,
                    aes(y = -0.6, x = pct2_rep - vote2_rep)) +
      scale_x_continuous(breaks = c(-0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3)) +
      coord_cartesian(xlim = c(-0.35, 0.35), ylim =c(-2,18))
    
  } else if (xaxis_labels == T){
    cycle_data <- plot_data %>% filter(cycle == year)
    plot <- ggplot() +
      geom_density_ridges(data = plot_data %>% filter(cycle == year), 
                          aes(x = pct2_rep - vote2_rep, y = as.factor(cycle), 
                              color = state),
                          scale = 0.75, fill = NA, bandwidth = 0.0155) +
      theme_bw() +
      geom_vline(xintercept = 0, linetype = "dashed") + 
      scale_y_discrete(limits=rev) +
      labs(y = "", x = "") +
      theme(legend.position = "none",
            axis.text = element_text(size = 16),
            axis.text.y = element_text(vjust = -1.4),
            plot.margin = margin,
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()) +
      scale_color_manual(guide = "none", values = rep("grey30", 50)) +
      geom_boxploth(data = plot_data %>% filter(cycle == year), inherit.aes = F, 
                    width = 2, aes(y = -0.6, x = pct2_rep - vote2_rep))  +
      scale_x_continuous(breaks = c(-0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3)) +
      coord_cartesian(xlim = c(-0.35, 0.35), ylim =c(-2,18))
  }
  
  return(plot)
}

# plot cycle tse vertical
plot_v_cycle_tse <- function(year, margin = unit(c(0, 0, 0, 0), "cm"), 
                           xaxis_labels = F, plot_data = polls) {
  if(xaxis_labels == F){
    cycle_data <- plot_data %>% filter(cycle == year)
    plot <- ggplot() +
      geom_density_ridges(data = plot_data %>% filter(cycle == year), 
                          aes(x = pct2_rep - vote2_rep, y = as.factor(cycle), 
                              color = state),
                          scale = 0.75, fill = NA, 
                          bandwidth = 0.0155) +
      theme_bw()  +
      coord_flip(xlim = c(-0.35, 0.35), ylim =c(-2,18)) +
      geom_vline(xintercept = 0, linetype = "dashed") + 
      scale_y_discrete(limits=rev) +
      labs(y = "", x = "") +
      theme(legend.position = "none",
            axis.text = element_blank(),
            axis.text.x = element_text(size = 18, vjust = -1.4),
            plot.margin = margin,
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.border = element_rect(color = NA)) +
      scale_color_manual(guide = "none", values = rep("grey30", 50)) +
      geom_boxplot(data = plot_data %>% filter(cycle == year), 
                    inherit.aes = F, width = 3,
                    aes(y = -0.6, x = pct2_rep - vote2_rep)) +
      scale_x_continuous(breaks = c(-0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3)) 
    
  } else if (xaxis_labels == T){
    cycle_data <- plot_data %>% filter(cycle == year)
    plot <- ggplot() +
      geom_density_ridges(data = plot_data %>% filter(cycle == year), 
                          aes(x = pct2_rep - vote2_rep, y = as.factor(cycle), 
                              color = state),
                          scale = 0.75, fill = NA, bandwidth = 0.0155)  +
      coord_flip(xlim = c(-0.35, 0.35), ylim =c(-2,18)) +
      theme_bw() +
      geom_vline(xintercept = 0, linetype = "dashed") + 
      scale_y_discrete(limits=rev) +
      labs(y = "", x = "") +
      theme(legend.position = "none",
            axis.text = element_text(size = 18),
            axis.text.x = element_text(vjust = -1.4),
            plot.margin = margin,
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.border = element_rect(color = NA)) +
      scale_color_manual(guide = "none", values = rep("grey30", 50)) +
      geom_boxplot(data = plot_data %>% filter(cycle == year), inherit.aes = F, 
                    width = 3, aes(y = -0.6, x = pct2_rep - vote2_rep))  +
      scale_x_continuous(breaks = c(-0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3))
  }
  
  return(plot)
}


# Preparation -------------------------------------------------------------

# compute election groups and ids for election year and state
polls <- polls %>%
  mutate(state_year = paste0(state, cycle),
         cycle = as.integer(cycle)) %>%
  group_by(state_year) %>%
  mutate(n_poll = n()) %>%
  ungroup() %>%
  filter(n_poll >= 5 & dte < 1460) %>% # exclude polls conducted more than 4 years (max time to orevious election)
  mutate(state_year_int = as.integer(as.factor(state_year)),
         t_sc = as.numeric(dte)/max(as.numeric(dte)))

# Election-level data
election_data <- polls %>%
  group_by(state_year, state_year_int, cycle, state, state_abb, vote2_rep) %>%
  summarise(n_avg = mean(sample_size)) %>%
  ungroup()






#### Data for plot ####

plot_data <- data.frame(mean_est = b0_summary$mean,
                        var_est = v0_r_summary$mean,
                        cycle = b0_summary$cycle,
                        state = b0_summary$state,
                        state_abb = b0_summary$state_abb,
                        state_year = b0_summary$state_year,
                        state_year_int = b0_summary$state_year_int)

rm(b0_summary, v0_r_summary)


# Plot --------------------------------------------------------------------

#### EDB ####

# generate plots for each cycle
plot1990 <- plot_cycle(1990, margin =  unit(c(0, 0.6, 0.5, 0.5), "cm"))
plot1992_2020 <- lapply(seq(1992, 2020, 2), plot_cycle) 
plot2022 <- plot_cycle(2022, xaxis_labels = T)

# append plots in list
plots <- append(list(plot1990), plot1992_2020, after = 1)
plots <- append(plots, list(plot2022), after = 16)

# plot
density_estimates_senate <- ggarrange(plotlist = plots, ncol = 1,
                                      heights = c(1.5,rep(1,15),1.2)) %>% 
  annotate_figure(bottom = text_grob("Estimated election-day bias and SD", 
                                     size = 18))

#### TSE ####

# generate plots for each cycle
plot_tse1990 <- plot_cycle_tse(year = 1990, margin =  unit(c(0, 0.6, 0.5, 0.5), "cm"))
plot_tse1992_2020 <- lapply(seq(1992, 2020, 2), plot_cycle_tse) 
plot_tse2022 <- plot_cycle_tse(2022, xaxis_labels = T)

# append plots in list
plots_tse <- append(list(plot_tse1990), plot_tse1992_2020, after = 1)
plots_tse <- append(plots_tse, list(plot_tse2022), after = 16)

# plot
tse_estimates_senate <- ggarrange(plotlist = plots_tse, ncol = 1,
                                  heights = c(1.5,rep(1,15),1.2)) %>% 
  annotate_figure(bottom = text_grob("Rep. 2 party poll support - Rep. 2 party vote share", 
                                     size = 18))

#### Combine plots ####

tse_density <- plot_grid(tse_estimates_senate,density_estimates_senate, 
                         ncol = 2, labels = "AUTO", label_x = 0.97, 
                         rel_widths = c(1.08,1), label_size = 18)

ggsave(filename = '~/results_vis/us_senate_predictable/plots/us_senate_tse_est_density.png',
       plot = tse_density,
       width = 20, height = 18, bg='#ffffff')

# ggsave(filename = '~/results_vis/us_senate_predictable/plots/us_senate_tse_est_density.eps',
#        plot = tse_density,
#        width = 20, height = 18, bg='#ffffff')



# Plot presentation --------------------------------------------------------


# generate plots for each cycle
plot_tse1990 <- plot_v_cycle_tse(year = 1990,  xaxis_labels = T, 
                                 margin = unit(c(0, 0.1, 0, 0.1), "cm"))
plot_tse1992_2020 <- lapply(seq(1992, 2020, 2), plot_v_cycle_tse,
                            margin = unit(c(0, 0.1, 0, 0), "cm")) 
plot_tse2022 <- plot_v_cycle_tse(2022, margin =  unit(c(0, 0.1, 0, 0.1),"cm"))

# append plots in list
plots_tse <- append(list(plot_tse1990), plot_tse1992_2020, after = 1)
plots_tse <- append(plots_tse, list(plot_tse2022), after = 16)

# plot
tse_estimates_senate <- ggarrange(plotlist = plots_tse, nrow = 1,
                                  widths = c(1.5,rep(1,16))
                                  ) %>% 
  annotate_figure(left = text_grob("Rep. 2 party poll support - Rep. 2 party vote share", 
                                     size = 22, rot = 90))

ggsave(filename = '~/results_vis/us_senate_predictable/plots/us_senate_polling_error_dist.png',
         plot = tse_estimates_senate,
         width = 20, height = 8, bg='#ffffff')
 


