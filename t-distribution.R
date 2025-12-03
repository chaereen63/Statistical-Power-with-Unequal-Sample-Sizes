# Central t-distribution graph and statistical calculation function (using pooled variance)
plot_t_distribution <- function(n1, n2, s1, s2, xlim = NULL, ylim_pdf = NULL, ylim_cdf = c(0, 1)) {
  
  # === Basic calculations ===
  # Degrees of freedom calculation
  df1 <- n1 - 1
  df2 <- n2 - 1
  df_pooled <- df1 + df2
  
  # Pooled variance calculation
  pooled_var <- ((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2)
  pooled_sd <- sqrt(pooled_var)
  
  # Standard error calculation
  se1 <- s1 / sqrt(n1)
  se2 <- s2 / sqrt(n2)
  se_pooled <- pooled_sd * sqrt(1/n1 + 1/n2)
  
  # === Parameter information output ===
  cat("=== Parameter Information ===\n")
  cat(sprintf("n1 = %d , n2 = %d\n", n1, n2))
  cat(sprintf("s1 = %.1f , s2 = %.1f\n", s1, s2))
  cat(sprintf("Degrees of freedom: df = %d (n1 + n2 - 2)\n", df_pooled))
  cat(sprintf("Pooled variance = %.4f\n", pooled_var))
  cat(sprintf("Pooled SD = %.4f\n", pooled_sd))
  cat(sprintf("s1/√n1 = %.4f\n", se1))
  cat(sprintf("s2/√n2 = %.4f\n", se2))
  cat(sprintf("SE(pooled) = %.4f\n", se_pooled))
  cat("\n")
  
  # === Draw t-distribution graph ===
  # Set x-axis range (user-defined or default)
  if(is.null(xlim)) {
    x_range <- seq(-4 * se_pooled, 4 * se_pooled, length.out = 1000)
  } else {
    x_range <- seq(xlim[1], xlim[2], length.out = 1000)
  }
  
  # Convert to standardized t values
  t_values <- x_range / se_pooled
  
  # Calculate t-distribution density and cumulative probability (scaled by SE)
  y_density <- dt(t_values, df_pooled) / se_pooled
  y_cumulative <- pt(t_values, df_pooled)
  
  # Set 2x1 panel layout
  par(mfrow = c(1, 2), mar = c(4, 4, 3, 2))
  
  # 1. Probability density function (PDF)
  plot(x_range, y_density, type = "l", col = "blue", lwd = 3,
       main = sprintf("t-Distribution Density Function\nt(%d), SE = %.4f", df_pooled, se_pooled),
       xlab = "Mean Difference (X̄₁ - X̄₂)", ylab = "Density", 
       ylim = if(is.null(ylim_pdf)) c(0, max(y_density) * 1.1) else ylim_pdf,
       xlim = if(is.null(xlim)) range(x_range) else xlim)
  
  # Add mean line
  abline(v = 0, col = "gray", lty = 2, lwd = 1)
  
  # Add grid
  grid(col = "lightgray", lty = 3)
  
  # 2. Cumulative distribution function (CDF)
  plot(x_range, y_cumulative, type = "l", col = "red", lwd = 3,
       main = sprintf("t-Distribution Cumulative Distribution Function\nt(%d), SE = %.4f", df_pooled, se_pooled),
       xlab = "Mean Difference (X̄₁ - X̄₂)", ylab = "P(T ≤ t)", 
       ylim = ylim_cdf,
       xlim = if(is.null(xlim)) range(x_range) else xlim)
  
  # Add 50% line
  abline(h = 0.5, col = "gray", lty = 2, lwd = 1)
  abline(v = 0, col = "gray", lty = 2, lwd = 1)
  
  # Add grid
  grid(col = "lightgray", lty = 3)
  
  # Restore original layout
  par(mfrow = c(1, 1))
  
  # === Quantile comparison (actual mean difference scale) ===
  cat("=== Key Quantiles for Mean Difference ===\n")
  cat(sprintf("Quantile   Mean Diff Scale   Standardized t\n"))
  cat("--------   --------------   --------------\n")
  
  quantiles <- c(0.025, 0.05, 0.1, 0.9, 0.95, 0.975)
  q_labels <- c("2.5%", "5%", "10%", "90%", "95%", "97.5%")
  
  for(i in 1:length(quantiles)) {
    q_t_standardized <- qt(quantiles[i], df_pooled)
    q_mean_diff <- q_t_standardized * se_pooled
    
    cat(sprintf("%-8s %14.4f   %14.3f\n", q_labels[i], q_mean_diff, q_t_standardized))
  }
  
  # === Critical value information ===
  cat("\n=== Two-tailed Test Critical Values (α = 0.05) ===\n")
  t_critical_std <- qt(0.975, df_pooled)
  mean_diff_critical <- t_critical_std * se_pooled
  cat(sprintf("Standardized t-value: ±%.3f\n", t_critical_std))
  cat(sprintf("Mean difference critical value: ±%.4f\n", mean_diff_critical))
  
  cat("\n=== One-tailed Test Critical Values (α = 0.05) ===\n")
  t_one_sided_std <- qt(0.95, df_pooled)
  mean_diff_one_sided <- t_one_sided_std * se_pooled
  cat(sprintf("Standardized t-value: %.3f\n", t_one_sided_std))
  cat(sprintf("Mean difference critical value: %.4f\n", mean_diff_one_sided))
  
  # Return results as list
  results <- list(
    n1 = n1, n2 = n2, s1 = s1, s2 = s2,
    df_pooled = df_pooled,
    pooled_var = pooled_var, pooled_sd = pooled_sd,
    se1 = se1, se2 = se2, se_pooled = se_pooled,
    t_critical_two_sided = mean_diff_critical,
    t_critical_one_sided = mean_diff_one_sided,
    t_critical_std_two_sided = t_critical_std,
    t_critical_std_one_sided = t_one_sided_std
  )
  
  invisible(results)
}


#### noncentral t-distribution (using pooled variance) ####
# Noncentral t-distribution graph and statistical calculation function (using pooled variance)
plot_noncentral_t <- function(n1, n2, s1, s2, delta = 0, xlim = NULL, ylim_pdf = NULL, ylim_cdf = c(0, 1), show_central = TRUE) {
  
  # === Basic calculations ===
  # Degrees of freedom calculation
  df1 <- n1 - 1
  df2 <- n2 - 1
  df_pooled <- df1 + df2
  
  # Pooled variance calculation
  pooled_var <- ((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2)
  pooled_sd <- sqrt(pooled_var)
  
  # Standard error calculation
  se1 <- s1 / sqrt(n1)
  se2 <- s2 / sqrt(n2)
  se_pooled <- pooled_sd * sqrt(1/n1 + 1/n2)
  
  # Calculate noncentrality parameter
  ncp <- delta / se_pooled
  
  # === Parameter information output ===
  cat("=== Parameter Information ===\n")
  cat(sprintf("n1 = %d , n2 = %d\n", n1, n2))
  cat(sprintf("s1 = %.1f , s2 = %.1f\n", s1, s2))
  cat(sprintf("Degrees of freedom: df = %d (n1 + n2 - 2)\n", df_pooled))
  cat(sprintf("Pooled variance = %.4f\n", pooled_var))
  cat(sprintf("Pooled SD = %.4f\n", pooled_sd))
  cat(sprintf("SE(pooled) = %.4f\n", se_pooled))
  cat(sprintf("Delta (δ) = %.2f\n", delta))
  cat(sprintf("Noncentrality parameter (δ) = %.4f\n", ncp))
  cat("\n")
  
  # === Draw t-distribution graph ===
  # Set x-axis range (user-defined or default)
  if(is.null(xlim)) {
    x_range <- seq(delta - 4 * se_pooled, delta + 4 * se_pooled, length.out = 1000)
  } else {
    x_range <- seq(xlim[1], xlim[2], length.out = 1000)
  }
  
  # Convert to standardized t values
  t_values <- x_range / se_pooled
  
  # Calculate noncentral t-distribution density and cumulative probability (scaled by SE)
  y_density_nc <- dt(t_values, df_pooled, ncp = ncp) / se_pooled
  y_cumulative_nc <- pt(t_values, df_pooled, ncp = ncp)
  
  # Also calculate central t-distribution (for comparison)
  y_density_c <- dt(t_values, df_pooled) / se_pooled
  y_cumulative_c <- pt(t_values, df_pooled)
  
  # Set 2x1 panel layout
  par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
  
  # 1. Probability density function (PDF)
  max_density <- max(c(y_density_nc, if(show_central) y_density_c else 0))
  
  plot(x_range, y_density_nc, type = "l", col = "red", lwd = 3,
       main = sprintf("Noncentral t-Distribution (δ = %.3f)\ndf = %d, Delta = %.2f", ncp, df_pooled, delta),
       xlab = "Mean Difference (X̄₁ - X̄₂)", ylab = "Density", 
       ylim = if(is.null(ylim_pdf)) c(0, max_density * 1.1) else ylim_pdf,
       xlim = if(is.null(xlim)) range(x_range) else xlim)
  
  # Add central t-distribution (for comparison)
  if(show_central) {
    lines(x_range, y_density_c, col = "blue", lwd = 2, lty = 2)
    legend("topright", 
           legend = c(sprintf("Noncentral t (δ=%.3f)", ncp), "Central t (δ=0)"),
           col = c("red", "blue"), lwd = c(3, 2), lty = c(1, 2),
           cex = 0.8, x.intersp = 0.5, y.intersp = 0.5, text.width=strwidth("Central t (δ=0)", cex=0.8))
  }
  
  # Add baseline at 0
  abline(v = 0, col = "gray", lty = 2, lwd = 1)
  
  # Add grid
  grid(col = "lightgray", lty = 3)
  
  # 2. Cumulative distribution function (CDF)
  plot(x_range, y_cumulative_nc, type = "l", col = "red", lwd = 3,
       main = sprintf("Noncentral t-Distribution CDF (δ = %.3f)\ndf = %d, Delta = %.2f", ncp, df_pooled, delta),
       xlab = "Mean Difference (X̄₁ - X̄₂)", ylab = "P(T ≤ t)", 
       ylim = ylim_cdf,
       xlim = if(is.null(xlim)) range(x_range) else xlim)
  
  # Add central t-distribution CDF (for comparison)
  if(show_central) {
    lines(x_range, y_cumulative_c, col = "blue", lwd = 2, lty = 2)
    legend("bottomright", 
           legend = c(sprintf("Noncentral t (δ=%.3f)", ncp), "Central t (δ=0)"),
           col = c("red", "blue"), lwd = c(3, 2), lty = c(1, 2),
           cex = 0.8, x.intersp = 0.5, y.intersp = 0.5, text.width=strwidth("Central t (δ=0)", cex=0.8))
  }
  
  # Add 50% line and baseline at 0
  abline(h = 0.5, col = "gray", lty = 2, lwd = 1)
  abline(v = 0, col = "gray", lty = 2, lwd = 1)
  
  # Add grid
  grid(col = "lightgray", lty = 3)
  
  # Restore original layout
  par(mfrow = c(1, 1))
  
  # === Quantile comparison (noncentral t-distribution) ===
  cat("=== Key Quantiles for Noncentral t-Distribution ===\n")
  cat(sprintf("Quantile   Mean Diff Scale   Standardized t\n"))
  cat("--------   --------------   --------------\n")
  
  quantiles <- c(0.025, 0.05, 0.1, 0.9, 0.95, 0.975)
  q_labels <- c("2.5%", "5%", "10%", "90%", "95%", "97.5%")
  
  for(i in 1:length(quantiles)) {
    q_t_standardized <- qt(quantiles[i], df_pooled, ncp = ncp)
    q_mean_diff <- q_t_standardized * se_pooled
    
    cat(sprintf("%-8s %14.4f   %14.3f\n", q_labels[i], q_mean_diff, q_t_standardized))
  }
  
  # === Power analysis ===
  cat("\n=== Power Analysis (Two-tailed test, α = 0.05) ===\n")
  
  # Critical values from central t-distribution
  t_critical_std <- qt(0.975, df_pooled)
  mean_diff_critical_upper <- t_critical_std * se_pooled
  mean_diff_critical_lower <- -t_critical_std * se_pooled
  
  cat(sprintf("Critical values (central t): ±%.4f (±%.3f standardized)\n", 
              mean_diff_critical_upper, t_critical_std))
  
  # Power calculation in noncentral t-distribution
  # P(T > t_critical | ncp) + P(T < -t_critical | ncp)
  power_upper <- 1 - pt(t_critical_std, df_pooled, ncp = ncp)
  power_lower <- pt(-t_critical_std, df_pooled, ncp = ncp)
  total_power <- power_upper + power_lower
  
  cat(sprintf("Power (reject H0 when delta = %.2f): %.4f (%.2f%%)\n", 
              delta, total_power, total_power * 100))
  cat(sprintf("  - Upper tail power: %.4f\n", power_upper))
  cat(sprintf("  - Lower tail power: %.4f\n", power_lower))
  
  # === One-tailed test power ===
  cat("\n=== Power Analysis (One-tailed test, α = 0.05) ===\n")
  t_one_sided_std <- qt(0.95, df_pooled)
  mean_diff_one_sided <- t_one_sided_std * se_pooled
  
  cat(sprintf("Critical value (one-sided): %.4f (%.3f standardized)\n", 
              mean_diff_one_sided, t_one_sided_std))
  
  # Power for right-tailed test (H1: μ1 > μ2)
  power_right <- 1 - pt(t_one_sided_std, df_pooled, ncp = ncp)
  cat(sprintf("Power (right-tailed test): %.4f (%.2f%%)\n", 
              power_right, power_right * 100))
  
  # Power for left-tailed test (H1: μ1 < μ2)
  power_left <- pt(-t_one_sided_std, df_pooled, ncp = ncp)
  cat(sprintf("Power (left-tailed test): %.4f (%.2f%%)\n", 
              power_left, power_left * 100))
  
  # Return results as list
  results <- list(
    n1 = n1, n2 = n2, s1 = s1, s2 = s2,
    df_pooled = df_pooled,
    pooled_var = pooled_var, pooled_sd = pooled_sd,
    se1 = se1, se2 = se2, se_pooled = se_pooled,
    delta = delta,
    ncp = ncp,
    t_critical_two_sided = mean_diff_critical_upper,
    t_critical_one_sided = mean_diff_one_sided,
    t_critical_std_two_sided = t_critical_std,
    t_critical_std_one_sided = t_one_sided_std,
    power_two_sided = total_power,
    power_one_sided_right = power_right,
    power_one_sided_left = power_left
  )
  
  invisible(results)
}