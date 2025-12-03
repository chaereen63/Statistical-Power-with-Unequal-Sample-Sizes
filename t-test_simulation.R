source("functions.R")
library(tidyverse)
library(asht)  # bfTest

output_dir <- "./results"
intermediate_dir <- file.path("results") # Separate folders
# Check folder existence
if (!dir.exists("./results")) dir.create("./results", recursive = TRUE)
if (!dir.exists("./intresults")) dir.create("./intresults", recursive = TRUE)
dir.exists("./results")
dir.exists("./intresults")

# Integrated function for scenario settings and replication generation
create_settings <- function(var1, var2, effect_size, n1, n2, replications = 50000) {
  # Calculate standard deviations
  sd1 <- sqrt(var1)
  sd2 <- sqrt(var2)
  
  # Calculate average variance
  av_var <- mean(c(var1, var2))
  
  # Calculate mean difference based on effect size
  mu_diff <- effect_size * sqrt(av_var)
  
  # Calculate mean for each group
  mu1 <- mu_diff/2
  mu2 <- -mu_diff/2
  
  # Generate replications
  tibble(
    var1 = var1,
    var2 = var2,
    sd1 = sd1,
    sd2 = sd2,
    effect_size = effect_size,
    av_var = av_var,
    mu_diff = mu_diff,
    mu1 = mu1,
    mu2 = mu2,
    n1 = n1,
    n2 = n2,
    varr = var2 / var1,
    replication = 1:replications,
    seed = sample.int(.Machine$integer.max, replications)
  )
}

# Scenario settings generation (60, 120, 240)
scenarios_config <- list(
  list(var1 = 4, var2 = 4, n1 = 120, n2 = 120, scenario = 1),
  list(var1 = 4, var2 = 4, n1 = 144, n2 = 96, scenario = 2),
  list(var1 = 4, var2 = 2, n1 = 120, n2 = 120, scenario = 3),
  list(var1 = 4, var2 = 2, n1 = 144, n2 = 96, scenario = 4),
  list(var1 = 4, var2 = 2, n1 = 96, n2 = 144, scenario = 5)
)

# Simulation function (memory efficient) - Modified to include bfTest
run_simulation_batch <- function(scenario_config, effect_size = 0, replications = 50000) {
  
  cat("Processing scenario", scenario_config$scenario, 
      "- n1:", scenario_config$n1, "n2:", scenario_config$n2, 
      "var1:", scenario_config$var1, "var2:", scenario_config$var2, "\n")
  
  # Generate settings
  settings <- create_settings(
    var1 = scenario_config$var1,
    var2 = scenario_config$var2,
    effect_size = effect_size,
    n1 = scenario_config$n1,
    n2 = scenario_config$n2,
    replications = replications
  ) %>%
    mutate(scenario = scenario_config$scenario)
  
  # Pre-allocate result vectors for memory efficiency - Added bfTest vectors
  student_p_values <- numeric(replications)
  welch_p_values <- numeric(replications)
  bf_p_values <- numeric(replications)  # Added bfTest p-value
  
  student_t_values <- numeric(replications)
  welch_t_values <- numeric(replications)
  bf_t_values <- numeric(replications)  # Added bfTest t-value
  
  student_df <- numeric(replications)  # Student t-test degrees of freedom
  welch_df <- numeric(replications)    # Welch t-test degrees of freedom
  bf_R_values <- numeric(replications) # Added bfTest R value
  
  # Sample statistics storage vectors
  sample_mean1 <- numeric(replications)
  sample_mean2 <- numeric(replications)
  sample_sd1 <- numeric(replications)
  sample_sd2 <- numeric(replications)
  
  # Effect size storage vectors
  pooled_d <- numeric(replications)
  hete_d <- numeric(replications) # Heterogeneous variance assumption
  
  # Batch processing (for memory management)
  batch_size <- 1000  # Number of simulations to process at once
  n_batches <- ceiling(replications / batch_size)
  
  for (batch in 1:n_batches) {
    start_idx <- (batch - 1) * batch_size + 1
    end_idx <- min(batch * batch_size, replications)
    current_batch_size <- end_idx - start_idx + 1
    
    if (batch %% 10 == 0) {
      cat("  Batch", batch, "of", n_batches, "completed\n")
    }
    
    # Run simulation for current batch
    for (i in 1:current_batch_size) {
      idx <- start_idx + i - 1
      current_row <- settings[idx, ]
      
      # Set seed
      set.seed(current_row$seed)
      
      # Generate data
      data <- simulate_data(
        mean1 = current_row$mu1,
        mean2 = current_row$mu2,
        sd1   = current_row$sd1,
        sd2   = current_row$sd2,
        n1    = current_row$n1,
        n2    = current_row$n2,
        seed  = current_row$seed
      )
      
      # Run t-tests and bfTest
      student_test <- t.test(data$x1, data$x2, var.equal = TRUE)
      welch_test <- t.test(data$x1, data$x2, var.equal = FALSE)
      bf_test <- bfTest(data$x1, data$x2)  # Added bfTest
      
      # Store p-values and t-statistics (including bfTest results)
      student_p_values[idx] <- student_test$p.value
      welch_p_values[idx] <- welch_test$p.value
      bf_p_values[idx] <- bf_test$p.value  # bfTest p-value
      
      student_t_values[idx] <- student_test$statistic
      welch_t_values[idx] <- welch_test$statistic
      bf_t_values[idx] <- bf_test$statistic  # bfTest t-value
      
      student_df[idx] <- student_test$parameter
      welch_df[idx] <- welch_test$parameter
      bf_R_values[idx] <- bf_test$parameter  # bfTest R value
      
      # Store sample statistics
      sample_mean1[idx] <- mean(data$x1)
      sample_mean2[idx] <- mean(data$x2)
      sample_sd1[idx] <- sd(data$x1)
      sample_sd2[idx] <- sd(data$x2)
      
      # Calculate effect sizes
      n1 <- current_row$n1
      n2 <- current_row$n2
      
      # Cohen's d (using pooled standard deviation)
      pooled_sd <- sqrt(((n1-1) * sample_sd1[idx]^2 + (n2-1) * sample_sd2[idx]^2) / (n1 + n2 - 2))
      pooled_d[idx] <- (sample_mean1[idx] - sample_mean2[idx]) / pooled_sd
      arith_sd <- sqrt((sample_sd1[idx]^2+sample_sd2[idx]^2)/2)
      hete_d[idx] <- (sample_mean1[idx] - sample_mean2[idx]) / arith_sd
    }
  }
  
  # Generate final results (including bfTest results)
  results <- tibble(
    # Scenario information
    scenario = scenario_config$scenario,
    replication = 1:replications,
    
    # Design information (population parameters)
    n1 = scenario_config$n1,
    n2 = scenario_config$n2,
    var1 = scenario_config$var1,
    var2 = scenario_config$var2,
    sd1 = sqrt(scenario_config$var1),
    sd2 = sqrt(scenario_config$var2),
    
    # Population parameters (designed values)
    pop_mean1 = settings$mu1,
    pop_mean2 = settings$mu2,
    pop_effect_size = effect_size,  # Population effect size
    
    # Seed information (for reproducibility)
    seed = settings$seed,
    
    # Sample statistics (from actually generated data)
    sample_mean1 = sample_mean1,
    sample_mean2 = sample_mean2,
    sample_sd1 = sample_sd1,
    sample_sd2 = sample_sd2,
    sample_mean_diff = sample_mean1 - sample_mean2,
    
    # Test statistics (added bfTest)
    student_t = student_t_values,
    welch_t = welch_t_values,
    bf_t = bf_t_values,  # Added bfTest t-value
    
    student_p = student_p_values,
    welch_p = welch_p_values,
    bf_p = bf_p_values,  # Added bfTest p-value
    
    student_df = student_df,
    welch_df = welch_df,
    bf_R = bf_R_values,  # Added bfTest R value
    
    # Sample effect sizes
    cohens_d = pooled_d,           # Standard Cohen's d (variable name corrected)
    hete_d = hete_d,              # Heterogeneous variance assumption effect size
    
    # Additional sample effect size calculations
    glass_delta1 = sample_mean_diff / sample_sd1,  # Glass's Δ (using control group SD)
    glass_delta2 = sample_mean_diff / sample_sd2,  # Glass's Δ (using experimental group SD)
    hedges_g = cohens_d * (1 - 3/(4*(n1 + n2 - 2) - 1))  # Hedges' g (small sample correction)
  )
  
  return(results)
}

# Main simulation execution function
run_full_simulation <- function(effect_size = 0, replications = 50000, 
                                output_file = NULL, save_intermediate = TRUE, 
                                intermediate_dir = "intermediate_results") {
  
  start_time <- Sys.time()
  cat("Starting simulation with effect size =", effect_size, 
      "and", replications, "replications per scenario\n")
  
  # Create intermediate results directory
  if (save_intermediate && !dir.exists(intermediate_dir)) {
    dir.create(intermediate_dir, recursive = TRUE)
    cat("Created intermediate directory:", intermediate_dir, "\n")
  }
  
  # List to store all scenario results
  all_results <- list()
  
  # Run simulation for each scenario
  for (i in seq_along(scenarios_config)) {
    scenario_start <- Sys.time()
    
    results <- run_simulation_batch(
      scenario_config = scenarios_config[[i]],
      effect_size = effect_size,
      replications = replications
    )
    
    all_results[[i]] <- results
    
    scenario_end <- Sys.time()
    cat("Scenario", i, "completed in", 
        round(difftime(scenario_end, scenario_start, units = "mins"), 2), "minutes\n")
    
    # Intermediate save option
    if (save_intermediate) {
      if (!is.null(output_file)) {
        # When output_file exists: save based on that filename
        base_name <- tools::file_path_sans_ext(basename(output_file))
        temp_file <- file.path(intermediate_dir, paste0(base_name, "_scenario_", i, ".RDS"))
      } else {
        # When output_file is NULL: use default filename
        temp_file <- file.path(intermediate_dir, paste0("simulation_scenario_", i, "_es_", effect_size, ".RDS"))
      }
      
      saveRDS(results, file = temp_file)
      cat("Intermediate results saved:", temp_file, "\n")
    }
    
    # Memory cleanup with garbage collection
    gc()
  }
  
  # Combine all results
  cat("Combining all results...\n")
  final_results <- bind_rows(all_results)
  
  # Final save
  if (!is.null(output_file)) {
    saveRDS(final_results, file = output_file)
    cat("Final results saved:", output_file, "\n")
  }
  
  end_time <- Sys.time()
  total_time <- difftime(end_time, start_time, units = "hours")
  cat("Total simulation completed in", round(total_time, 2), "hours\n")
  cat("Total simulations:", nrow(final_results), "\n")
  
  return(final_results)
}


# Execution
cat("simulation")
results <- run_full_simulation(
  effect_size = 0.0, # Modify according to effect size condition 
  replications = 30000,
  output_file = file.path(output_dir, "S24results240_E0.RDS"), # Modify filename
  save_intermediate = TRUE,
  intermediate_dir = intermediate_dir
) 

# Check basic content and missing values
print(results)
names(results)
sum(is.na(results))