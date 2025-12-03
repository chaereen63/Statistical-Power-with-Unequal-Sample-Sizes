# Figures
#### Figure 1. Type I Error Rate bar Plot (d=0) ####
# Type I error data (d=0)
type1_data <- data.frame(
  Condition = rep(c("A", "B", "C", "D", "E"), each = 6),
  N = rep(rep(c(60, 120, 240), each = 2), times = 5),
  test = rep(c("Student's t-test", "Welch's t-test"), times = 15),
  error_rate = c(
    # Condition A
    0.05, 0.05, 0.051, 0.051, 0.049, 0.049,
    # Condition B
    0.05, 0.05, 0.049, 0.048, 0.053, 0.053,
    # Condition C
    0.051, 0.053, 0.049, 0.049, 0.052, 0.052,
    # Condition D
    0.021, 0.05, 0.021, 0.052, 0.02, 0.049,
    # Condition E
    0.097, 0.052, 0.096, 0.05, 0.097, 0.048
  )
)

# Panel generation function
create_type1_panel <- function(data, N_value, show_y_label = FALSE) {
  subset_data <- data[data$N == N_value, ]
  
  p <- ggplot(subset_data, aes(x = Condition, y = error_rate, 
                               fill = test, color = test)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), 
             width = 0.7, linewidth = 0.5) +
    geom_hline(yintercept = 0.05, linetype = "dashed", color = "red", linewidth = 0.6) +
    annotate("text", x = 0.6, y = 0.05, label = ".05", 
             vjust = -0.5, hjust = 0, size = 5, color = "red") +
    scale_fill_manual(values = c("Student's t-test" = "#FFA500", 
                                 "Welch's t-test" = "#0066CC"),
                      name = "Method") +
    scale_color_manual(values = c("Student's t-test" = "#CC7A00", 
                                  "Welch's t-test" = "#004C99"),
                       name = "Method") +
    labs(title = bquote(bold("N = " ~ .(N_value))),
         x = "",
         y = if(show_y_label) "Type I Error Rate" else "") +
    scale_y_continuous(breaks = seq(0, 0.1, 0.02), limits = c(0, 0.1)) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),
      axis.title = element_text(size = 12),
      axis.title.y = element_text(size = if(show_y_label) 12 else 0),
      axis.text = element_text(size = 11),
      legend.position = "none",
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  return(p)
}

# Generate 3 panels
panel_N60 <- create_type1_panel(type1_data, 60, show_y_label = TRUE)
panel_N120 <- create_type1_panel(type1_data, 120, show_y_label = FALSE)
panel_N240 <- create_type1_panel(type1_data, 240, show_y_label = FALSE)

# Generate legend
legend_data <- data.frame(
  x = c(1, 2),
  y = c(1, 1),
  test = c("Student's t-test", "Welch's t-test")
)

legend_plot <- ggplot(legend_data, aes(x = x, y = y, fill = test, color = test)) +
  geom_bar(stat = "identity", linewidth = 0.5) +
  scale_fill_manual(values = c("Student's t-test" = "#FFA500", 
                               "Welch's t-test" = "#0066CC"),
                    name = "Method") +
  scale_color_manual(values = c("Student's t-test" = "#CC7A00", 
                                "Welch's t-test" = "#004C99"),
                     name = "Method") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 11, face = "bold"))

legend <- get_legend(legend_plot)

# Final combination
combined_plot <- grid.arrange(
  panel_N60, panel_N120, panel_N240,
  ncol = 3,
  bottom = textGrob("Condition\n", gp = gpar(fontsize = 12))
)

# Save with legend included
p1 <- arrangeGrob(combined_plot, legend, ncol = 1, heights = c(0.9, 0.1))
print(p1)


# ggsave("type1_error_barplot.png", p1, width = 9, height = 5.5, dpi = 600)

#### Figure 2. Simulation Results Line Plot (By Sample Size) ####
library(ggplot2)
library(gridExtra)
library(grid)
library(cowplot)

# New simulation data (excluding d=0)
sim_data_line <- data.frame(
  Condition = rep(c("A", "B", "C", "D", "E"), each = 18),
  N = rep(rep(c(60, 120, 240), each = 6), times = 5),
  d = rep(rep(c(0.2, 0.5, 0.8), each = 2), times = 15),
  test = rep(c("Student's t-test", "Welch's t-test"), times = 45),
  power = c(
    # Condition A (variance ratio = 1, sample ratio = 1:1)
    # N=60
    0.118, 0.117, 0.482, 0.482, 0.865, 0.864,
    # N=120
    0.189, 0.189, 0.775, 0.775, 0.992, 0.992,
    # N=240
    0.337, 0.337, 0.97, 0.97, 1, 1,
    
    # Condition B (variance ratio = 0.5, sample ratio = 1:1)
    # N=60
    0.119, 0.118, 0.481, 0.479, 0.862, 0.861,
    # N=120
    0.196, 0.195, 0.779, 0.778, 0.992, 0.992,
    # N=240
    0.337, 0.337, 0.972, 0.972, 1, 1,
    
    # Condition C (variance ratio = 1, sample ratio = 2:1)
    # N=60
    0.11, 0.109, 0.438, 0.431, 0.822, 0.813,
    # N=120
    0.176, 0.175, 0.725, 0.721, 0.985, 0.984,
    # N=240
    0.308, 0.307, 0.954, 0.953, 1, 1,
    
    # Condition D (variance ratio = 0.5, sample ratio = 2:1)
    # N=60
    0.08, 0.121, 0.388, 0.473, 0.802, 0.862,
    # N=120
    0.138, 0.195, 0.703, 0.775, 0.984, 0.991,
    # N=240
    0.259, 0.338, 0.953, 0.972, 1, 1,
    
    # Condition E (variance ratio = 0.5, sample ratio = 1:2)
    # N=60
    0.152, 0.103, 0.484, 0.389, 0.835, 0.761,
    # N=120
    0.22, 0.162, 0.752, 0.672, 0.985, 0.972,
    # N=240
    0.358, 0.282, 0.954, 0.929, 1, 1
  )
)

# Create group variable
sim_data_line$group <- paste(sim_data_line$test, sim_data_line$d, sep = "_")

# Background lines: Only Welch's A, C
background_data <- sim_data_line[sim_data_line$Condition %in% c("A", "C") & 
                                   sim_data_line$test == "Welch's t-test", ]

# Highlight lines: D, E
highlight_data <- sim_data_line[sim_data_line$Condition %in% c("D", "E"), ]

# Panel generation function
create_line_panel <- function(bg_data, hl_data, N_value, show_y_label = FALSE) {
  bg_subset <- bg_data[bg_data$N == N_value, ]
  hl_subset <- hl_data[hl_data$N == N_value, ]
  
  # Separate Student's and Welch's
  hl_student <- hl_subset[hl_subset$test == "Student's t-test", ]
  hl_welch <- hl_subset[hl_subset$test == "Welch's t-test", ]
  
  p <- ggplot() +
    # Background horizontal lines (Only Welch's A, C) - light colors
    geom_segment(data = bg_subset, 
                 aes(x = 0.5, xend = 2.3, y = power, yend = power),
                 color = "#0066CC", linetype = "solid",
                 linewidth = 0.5, alpha = 0.3) +
    # Add labels (A, C)
    geom_text(data = bg_subset, 
              aes(x = 2.4, y = power, label = Condition),
              color = "#0066CC", hjust = 0, size = 2.8, alpha = 0.5) +
    # Student's t-test lines (longdash)
    geom_line(data = hl_student, 
              aes(x = Condition, y = power, group = group),
              color = "#FFA500", linetype = "longdash",
              linewidth = 0.8) +
    geom_point(data = hl_student, 
               aes(x = Condition, y = power, shape = factor(d)),
               color = "#FFA500",
               size = 2.5, stroke = 0.8) +
    # Welch's t-test lines (solid)
    geom_line(data = hl_welch, 
              aes(x = Condition, y = power, group = group),
              color = "#0066CC", linetype = "solid",
              linewidth = 0.8) +
    geom_point(data = hl_welch, 
               aes(x = Condition, y = power, shape = factor(d)),
               color = "#0066CC",
               size = 2.5, stroke = 0.8) +
    scale_shape_manual(values = c("0.2" = 16, "0.5" = 15, "0.8" = 17),
                       name = "Effect Size",
                       labels = c("d=0.2", "d=0.5", "d=0.8")) +
    scale_x_discrete(limits = c("D", "E")) +
    labs(title = bquote(bold("N = " ~ .(N_value))),
         x = "",
         y = if(show_y_label) "Power" else "") +
    scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1.05)) +
    coord_cartesian(clip = "off") + 
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),
      axis.title = element_text(size = 12),
      axis.title.y = element_text(size = if(show_y_label) 12 else 0),
      axis.text = element_text(size = 11),
      legend.position = "none",
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = margin(5, 5, 5, 10)
    )
  
  return(p)
}

# Generate 3 panels (first one only with y-axis label)
panel_N60 <- create_line_panel(background_data, highlight_data, 60, show_y_label = TRUE)
panel_N120 <- create_line_panel(background_data, highlight_data, 120, show_y_label = FALSE)
panel_N240 <- create_line_panel(background_data, highlight_data, 240, show_y_label = FALSE)

# Data for legend generation (manually created)
legend_grob <- grid::legendGrob(
  labels = c("Student's t-test", "Welch's t-test", "d=0.2", "d=0.5", "d=0.8"),
  pch = c(NA, NA, 16, 15, 17),
  gp = grid::gpar(
    col = c("#FFA500", "#0066CC", "black", "black", "black"),
    lty = c(2, 1, 0, 0, 0),
    lwd = c(2, 2, 0, 0, 0)
  ),
  nrow = 1
)

# 1×3 horizontal arrangement
combined_plot <- plot_grid(
  plot_grid(panel_N60, panel_N120, panel_N240, ncol = 3),
  ncol = 1,
  rel_heights = c(1, 0.1)
)

# Add x-axis label at the bottom
final_plot <- grid.arrange(
  combined_plot,
  bottom = textGrob("Condition", gp = gpar(fontsize = 12), 
                    vjust = -1)
)

print(final_plot)

# Save
# ggsave("simulation_results_lineplot_revised2.png", final_plot, 
#        width = 9, height = 5.5, dpi = 600)

#### Figure 3. Power Function: 4-panel comparison with Simulation Results ####
library(ggplot2)
library(gridExtra)
library(grid)
library(cowplot)


### 2:1 A to E ###
# Basic alpha setting
alpha <- 0.05

# Simulation data (2:1, 1:2)
sim_data <- data.frame(
  Condition = rep(c("A", "B", "C", "D", "E"), each = 24),
  N = rep(rep(c(60, 120, 240), each = 8), times = 5),
  d = rep(rep(c(0.0, 0.2, 0.5, 0.8), each = 2), times = 15),
  test = rep(c("Student's t-test", "Welch's t-test"), times = 60),
  power = c(
    # Condition A (variance ratio = 1.00, sample ratio = 1.00)
    # N=60
    0.049, 0.049, 0.118, 0.117, 0.482, 0.482, 0.865, 0.864,
    # N=120
    0.051, 0.051, 0.189, 0.189, 0.775, 0.775, 0.992, 0.992,
    # N=240
    0.05, 0.05, 0.337, 0.337, 0.97, 0.97, 1, 1,
    
    # Condition B (variance ratio = 0.50, sample ratio = 1.00)
    # N=60
    0.05, 0.05, 0.119, 0.118, 0.481, 0.479, 0.862, 0.861,
    # N=120
    0.052, 0.052, 0.196, 0.195, 0.779, 0.778, 0.992, 0.992,
    # N=240
    0.05, 0.05, 0.337, 0.337, 0.972, 0.972, 1, 1,
    
    # Condition C (variance ratio = 1.00, sample ratio = 0.50)
    # N=60
    0.052, 0.053, 0.11, 0.109, 0.438, 0.431, 0.822, 0.813,
    # N=120
    0.051, 0.051, 0.176, 0.175, 0.725, 0.721, 0.985, 0.984,
    # N=240
    0.051, 0.051, 0.308, 0.307, 0.954, 0.953, 1, 1,
    
    # Condition D (variance ratio = 0.50, sample ratio = 0.50)
    # N=60
    0.028, 0.047, 0.08, 0.121, 0.388, 0.473, 0.802, 0.862,
    # N=120
    0.029, 0.049, 0.138, 0.195, 0.703, 0.775, 0.984, 0.991,
    # N=240
    0.029, 0.05, 0.259, 0.338, 0.953, 0.972, 1, 1,
    
    # Condition E (variance ratio = 0.50, sample ratio = 2.00)
    # N=60
    0.079, 0.049, 0.152, 0.103, 0.484, 0.389, 0.835, 0.761,
    # N=120
    0.08, 0.05, 0.22, 0.162, 0.752, 0.672, 0.985, 0.972,
    # N=240
    0.08, 0.05, 0.358, 0.282, 0.954, 0.929, 1, 1
  )
)

# Integrated t-test Power function
t_power <- function(d, var1, var2, n1, n2, alpha = 0.05, var_equal = FALSE) {
  kappa <- n2/n1
  N <- (n1 + n2)
  
  if (var_equal) {
    # Student's t-test (equal variance)
    rho <- 1
    df <- n1 + n2 - 2
  } else {
    # Welch's t-test (unequal variance)
    rho <- var2/var1
    # Welch-Satterthwaite degrees of freedom
    df <- (var1/n1 + var2/n2)^2 / ((var1/n1)^2/(n1-1) + (var2/n2)^2/(n2-1))
  }
  
  # weight in NCP
  w <- sqrt((N/2) * ((kappa*(1+rho)) / ((1+kappa)*(kappa+rho))))
  
  # NCP
  ncp <- d * w
  
  # Power calculation (two-tailed test)
  power <- 1 - pt(qt(1-alpha/2, df=df), df=df, ncp=ncp) + 
    pt(qt(alpha/2, df=df), df=df, ncp=ncp)
  
  return(power)
}

# Power calculation function for each condition
calculate_power_condition <- function(var1, var2, kappa, N_values, d_values, alpha) {
  power_list <- list()
  
  for(N in N_values) {
    # Calculate n1, n2
    n1 <- round(N / (1 + kappa))
    n2 <- N - n1
    
    # Calculate Welch's t-test power
    welch_power_values <- numeric(length(d_values))
    for(i in 1:length(d_values)) {
      welch_power_values[i] <- t_power(d_values[i], var1, var2, n1, n2, alpha, var_equal = FALSE)
    }
    
    # Calculate Student's t-test power
    student_power_values <- numeric(length(d_values))
    for(i in 1:length(d_values)) {
      student_power_values[i] <- t_power(d_values[i], var1, var2, n1, n2, alpha, var_equal = TRUE)
    }
    
    # Create data frame
    temp_df <- data.frame(
      d = rep(d_values, 2),
      power = c(welch_power_values, student_power_values),
      test = rep(c("Welch's t-test", "Student's t-test"), each = length(d_values)),
      N = N
    )
    
    power_list[[as.character(N)]] <- temp_df
  }
  
  # Combine all data
  power_df <- do.call(rbind, power_list)
  return(power_df)
}

# Panel generation function (single condition)
create_panel <- function(power_df, sim_df, condition_name, kappa, rho, label_positions) {
  # Title: condition name + parameter information
  title_text <- bquote(bold(.(condition_name)) ~ ": " ~ n[2]/n[1] == .(kappa) ~ ", " ~ rho == .(rho))
  
  p <- ggplot(power_df, aes(x = d, y = power, color = test, linetype = test, group = interaction(test, N))) +
    geom_line(linewidth = 1.2) +
    # Add simulation results
    geom_point(data = sim_df, aes(x = d, y = power, shape = test, group = test), 
               color = "#E63946", size = 2.2, stroke = 1.2) +
    scale_color_manual(values = c("Welch's t-test" = "#0066CC", 
                                  "Student's t-test" = "gray50")) +
    scale_linetype_manual(values = c("Welch's t-test" = "solid", 
                                     "Student's t-test" = "longdash")) +
    scale_shape_manual(values = c("Welch's t-test" = 16, 
                                  "Student's t-test" = 1)) +
    # Total sample size labels
    annotate("text", x = label_positions$N240[1], y = label_positions$N240[2], 
             label = "N=240", size = 4, hjust = 0) +
    annotate("text", x = label_positions$N120[1], y = label_positions$N120[2], 
             label = "N=120", size = 4, hjust = 0) +
    annotate("text", x = label_positions$N60[1], y = label_positions$N60[2], 
             label = "N=60", size = 4, hjust = 0) +
    ggtitle(title_text) +
    labs(x = bquote("Effect Size (Cohen's "* italic(d)*")"),
         y = "Power") +
    scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
    scale_x_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0, size = 13, face = "bold"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 11),
      legend.position = "none",
      panel.grid.minor = element_line(color = "gray95"),
      panel.grid.major = element_line(color = "gray90")
    )
  
  return(p)
}

# Panel generation function to overlay A and B
create_panel_AB <- function(power_df_A, power_df_B, sim_df_A, sim_df_B, label_positions) {
  # Add Condition column to A and B data
  power_df_A$Condition <- "A"
  power_df_B$Condition <- "B"
  
  # Combine two data frames
  power_df_combined <- rbind(power_df_A, power_df_B)
  
  # Combine simulation data as well
  sim_df_A$Condition <- "A"
  sim_df_B$Condition <- "B"
  sim_df_combined <- rbind(sim_df_A, sim_df_B)
  
  # Title
  title_text <- bquote(bold("A & B") ~ ": " ~ n[2]/n[1] == 1 ~ ", " ~ rho == "1 or 1/2")
  
  p <- ggplot(power_df_combined, aes(x = d, y = power, color = test, linetype = test, 
                                     group = interaction(test, N, Condition))) +
    geom_line(linewidth = 1.2, alpha = 0.8) +
    # Add simulation results
    geom_point(data = sim_df_combined, aes(x = d, y = power, shape = test, group = interaction(test, Condition)), 
               color = "#E63946", size = 2.2, stroke = 1.2) +
    scale_color_manual(values = c("Welch's t-test" = "#0066CC", 
                                  "Student's t-test" = "gray50")) +
    scale_linetype_manual(values = c("Welch's t-test" = "solid", 
                                     "Student's t-test" = "longdash")) +
    scale_shape_manual(values = c("Welch's t-test" = 16, 
                                  "Student's t-test" = 1)) +
    # Total sample size labels
    annotate("text", x = label_positions$N240[1], y = label_positions$N240[2], 
             label = "N=240", size = 4, hjust = 0) +
    annotate("text", x = label_positions$N120[1], y = label_positions$N120[2], 
             label = "N=120", size = 4, hjust = 0) +
    annotate("text", x = label_positions$N60[1], y = label_positions$N60[2], 
             label = "N=60", size = 4, hjust = 0) +
    ggtitle(title_text) +
    labs(x = bquote("Effect Size (Cohen's "* italic(d)*")"),
         y = "Power") +
    scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
    scale_x_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0, size = 13, face = "bold"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 11),
      legend.position = "none",
      panel.grid.minor = element_line(color = "gray95"),
      panel.grid.major = element_line(color = "gray90")
    )
  
  return(p)
}

# Set effect size (d) range
d_values <- seq(0, 1, by = 0.01)

# Set total sample size
N_values <- c(60, 120, 240)

# Condition A (n2/n1 = 1, rho = 1)
power_df_A <- calculate_power_condition(var1 = 4, var2 = 4, kappa = 1, N_values, d_values, alpha)
sim_df_A <- sim_data[sim_data$Condition == "A", ]

# Condition B (n2/n1 = 1, rho = 0.5)
power_df_B <- calculate_power_condition(var1 = 4, var2 = 2, kappa = 1, N_values, d_values, alpha)
sim_df_B <- sim_data[sim_data$Condition == "B", ]

# Panel overlaying A and B
panel_AB <- create_panel_AB(power_df_A, power_df_B, sim_df_A, sim_df_B,
                            list(N240 = c(0.3, 0.97), N120 = c(0.435, 0.895), N60 = c(0.62, 0.83)))

# Condition C (n2/n1 = 2, rho = 1)
power_df_C <- calculate_power_condition(var1 = 4, var2 = 4, kappa = 1/2, N_values, d_values, alpha)
sim_df_C <- sim_data[sim_data$Condition == "C", ]
panel_C <- create_panel(power_df_C, sim_df_C, "C", "2", "1", 
                        list(N240 = c(0.30, 0.97), N120 = c(0.48, 0.9), N60 = c(0.64, 0.8)))

# Condition D (n2/n1 = 2, rho = 0.5)
power_df_D <- calculate_power_condition(var1 = 4, var2 = 2, kappa = 1/2, N_values, d_values, alpha)
sim_df_D <- sim_data[sim_data$Condition == "D", ]
panel_D <- create_panel(power_df_D, sim_df_D, "D", "1/2", "1/2", 
                        list(N240 = c(0.3, 0.97), N120 = c(0.42, 0.87), N60 = c(0.58, 0.77)))

# Condition E (n2/n1 = 1/2, rho = 0.5)
power_df_E <- calculate_power_condition(var1 = 4, var2 = 2, kappa = 2, N_values, d_values, alpha)
sim_df_E <- sim_data[sim_data$Condition == "E", ]
panel_E <- create_panel(power_df_E, sim_df_E, "E", "2", "1/2", 
                        list(N240 = c(0.33, 0.97), N120 = c(0.46, 0.86), N60 = c(0.62, 0.77)))

# Generate legend (separately)
legend_df_line <- data.frame(
  d = c(0, 0.5, 0, 0.5),
  power = c(0.5, 0.6, 0.45, 0.55),
  test = rep(c("Welch's t-test", "Student's t-test"), each = 2)
)

legend_df_point <- data.frame(
  d = c(0.25, 0.25),
  power = c(0.55, 0.475),
  test = c("Welch's t-test", "Student's t-test")
)

legend_plot <- ggplot() +
  geom_line(data = legend_df_line, aes(x = d, y = power, color = test, linetype = test), linewidth = 1) +
  geom_point(data = legend_df_point, aes(x = d, y = power, shape = test), 
             color = "#E63946", size = 1.8, stroke = 1) +
  scale_color_manual(values = c("Welch's t-test" = "#0066CC", 
                                "Student's t-test" = "gray50"),
                     name = "Theoretical") +
  scale_linetype_manual(values = c("Welch's t-test" = "solid", 
                                   "Student's t-test" = "longdash"),
                        name = "Theoretical") +
  scale_shape_manual(values = c("Welch's t-test" = 16, 
                                "Student's t-test" = 1),
                     name = "Simulation") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 9))

# Extract legend only
legend <- get_legend(legend_plot)

# Arrange 4 panels (2×2)
combined_plot <- grid.arrange(
  panel_AB, panel_C,
  panel_D, panel_E,
  ncol = 2,
  bottom = legend
)

print(combined_plot)

# save
# ggsave("power_function_4panel.png", combined_plot, width = 9, height = 8, dpi = 600)

#### Figure 4. contour plot ####
library(ggplot2)
library(gridExtra)
library(grid)
library(viridis)

# Welch's t-test Power function
welch_power <- function(d, var1, var2, n1, n2, alpha = 0.05) {
  m <- n2/n1
  N <- (n1 + n2)
  rho <- var2/var1
  
  # Welch-Satterthwaite degrees of freedom
  df <- (var1/n1 + var2/n2)^2 / ((var1/n1)^2/(n1-1) + (var2/n2)^2/(n2-1))
  
  # weight in NCP
  w <- sqrt((N/2) * ((m*(1+rho)) / ((1+m)*(m+rho))))
  
  # NCP
  ncp <- d * w
  
  # Power calculation (two-tailed test)
  power <- 1 - pt(qt(1-alpha/2, df=df), df=df, ncp=ncp) + 
    pt(qt(alpha/2, df=df), df=df, ncp=ncp)
  
  return(power)
}

# m, rho function #
# Fixed conditions
N <- 120
alpha <- 0.05
d <- 0.5  # Fixed effect size

# Range of m and ρ
m_range <- seq(0.2, 5, length.out = 100)
rho_range <- seq(0.2, 5, length.out = 100)

# Generate grid for power calculation
power_grid <- expand.grid(m = m_range, rho = rho_range)

# Calculate power for each combination
power_grid$power <- sapply(1:nrow(power_grid), function(i) {
  m <- power_grid$m[i]
  rho <- power_grid$rho[i]
  
  # Calculate n1, n2
  n1 <- round(N / (1 + m))
  n2 <- N - n1
  
  # Fix var1 to 1 and var2 = rho * var1
  var1 <- 1
  var2 <- rho * var1
  
  # Calculate power
  if(n1 < 2 || n2 < 2) return(NA)
  
  power <- welch_power(d, var1, var2, n1, n2, alpha)
  return(power)
})


# special condition points
special_points <- data.frame(
  m = c(1, 1/2, 1/3, 2/3),
  rho = c(1, 1/2, 1/2, 1/2),
  label = c("1", "1/2", "1/3", "2/3"),
  color = c("red", "black", "black", "black")
)
# points for vertical lines (excluding m=1)
special_points_vline <- special_points[special_points$m != 1, ]

# Contour plot
p4 <- ggplot(power_grid, aes(x = m, y = rho, z = power)) +
  geom_contour_filled(bins = 20) +
  geom_contour(color = "white", alpha = 0.3, bins = 20) +
  scale_fill_viridis_d(option = "plasma", name = "Power") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "white", alpha = 0.5) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "white", alpha = 0.5) +
  # add vertical lines from special points to m-axis
  geom_segment(data = special_points_vline,
               aes(x = m, xend = m, y = rho, yend = 0),
               linetype = "dotted", color = "gray30", size = 0.5,
               inherit.aes = FALSE) +
  # add special points
  geom_point(data = special_points, 
             aes(x = m, y = rho, color = color),
             size = 1.5, shape = 16, inherit.aes = FALSE) +
  # add m coordinates on x-axis
  geom_text(data = special_points_vline,
            aes(x = m, y = -0.15, label = label),
            size = 2.5, color = "black",
            inherit.aes = FALSE) +
  scale_color_identity() +  # Apply color directly
  labs(title = bquote("Welch's "* italic(t)*"-test Power as a Function of m and ρ"),
       subtitle = bquote("N =" ~ .(N)*" & " * italic(d) * " = " ~ .(d)),
       x = bquote(m == n[2]/n[1]),
       y = bquote(rho == sigma[2]^2/sigma[1]^2))+
  
  guides(fill = guide_legend(reverse = TRUE)) +  # legend
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    legend.position = "right"
  ) +
  coord_cartesian(clip = "off", ylim = c(0.2, 5))

print(p4)

# ggsave("./study2/contourPlot.png", p4, width = 8,height = 6,dpi=600)

#### Figure 5. contour plot (log scale) ####
library(ggplot2)
library(gridExtra)
library(grid)
library(viridis)

# Welch's t-test Power function
welch_power <- function(d, var1, var2, n1, n2, alpha = 0.05) {
  m <- n2/n1
  N <- (n1 + n2)
  rho <- var2/var1
  
  # Welch-Satterthwaite degrees of freedom
  df <- (var1/n1 + var2/n2)^2 / ((var1/n1)^2/(n1-1) + (var2/n2)^2/(n2-1))
  
  # weight in NCP
  w <- sqrt((N/2) * ((m*(1+rho)) / ((1+m)*(m+rho))))
  
  # NCP
  ncp <- d * w
  
  # Power calculation (two-tailed test)
  power <- 1 - pt(qt(1-alpha/2, df=df), df=df, ncp=ncp) + 
    pt(qt(alpha/2, df=df), df=df, ncp=ncp)
  
  return(power)
}

# m, rho function #
# Fixed conditions
N <- 120
alpha <- 0.05
d <- 0.5  # Fixed effect size

# Range of m and ρ
m_range <- seq(0.2, 5, length.out = 100)
rho_range <- seq(0.2, 5, length.out = 100)

# Generate grid for power calculation
power_grid <- expand.grid(m = m_range, rho = rho_range)

# Calculate power for each combination
power_grid$power <- sapply(1:nrow(power_grid), function(i) {
  m <- power_grid$m[i]
  rho <- power_grid$rho[i]
  
  # Calculate n1, n2
  n1 <- round(N / (1 + m))
  n2 <- N - n1
  
  # Fix var1 to 1 and var2 = rho * var1
  var1 <- 1
  var2 <- rho * var1
  
  # Calculate power
  if(n1 < 2 || n2 < 2) return(NA)
  
  power <- welch_power(d, var1, var2, n1, n2, alpha)
  return(power)
})


# special condition points
special_points <- data.frame(
  m = c(1, 1/2, 1/3, 2/3),
  rho = c(1, 1/2, 1/2, 1/2),
  label = c("1", "1/2", "1/3", "2/3"),
  color = c("red", "black", "black", "black")
)
# points for vertical lines (excluding m=1)
special_points_vline <- special_points[special_points$m != 1, ]

# Contour plot
p5 <- ggplot(power_grid, aes(x = m, y = rho, z = power)) +
  geom_contour_filled(bins = 20) +
  geom_contour(color = "white", alpha = 0.3, bins = 20) +
  scale_fill_viridis_d(option = "plasma", name = "Power") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "white", alpha = 0.5) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "white", alpha = 0.5) +
  #log scale
  scale_x_log10() +
  scale_y_log10() +
  # add vertical lines from special points to m-axis
  geom_segment(data = special_points_vline,
               aes(x = m, xend = m, y = rho, yend = 0.2),
               linetype = "dotted", color = "gray30", size = 0.5,
               inherit.aes = FALSE) +
  # add special points
  geom_point(data = special_points, 
             aes(x = m, y = rho, color = color),
             size = 1.5, shape = 16, inherit.aes = FALSE) +
  # add m coordinates on x-axis
  geom_text(data = special_points_vline,
            aes(x = m, y = 0.19, label = label),
            size = 2.5, color = "black",
            inherit.aes = FALSE) +
  scale_color_identity() +  # Apply color directly
  labs(title = bquote("Welch's "* italic(t)*"-test Power as a Function of m and ρ (Log Scale)"),
       subtitle = bquote("N =" ~ .(N)*" & " * italic(d) * " = " ~ .(d)),
       x = bquote(m == n[2]/n[1]),
       y = bquote(rho == sigma[2]^2/sigma[1]^2))+
  
  guides(fill = guide_legend(reverse = TRUE)) +  # legend
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    legend.position = "right"
  ) +
  coord_cartesian(clip = "off", ylim = c(0.2, 5))

p5
# ggsave("contourPlotLogScale.png", p5, width = 8,height = 6,dpi=600)
 