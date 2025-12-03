# Statistical Power for Independent Samples t-Tests with Unequal Sample Sizes

This repository contains simulation codes and analysis scripts for evaluating statistical power in independent samples t-tests under various conditions of sample size ratios and variance heterogeneity.

## Requirements

- R (version 4.0.0 or higher)
- RStudio (recommended)
- Required R packages:
  - `tidyverse` (includes `dplyr`, `tidyr`)
  - `ggplot2`
  - `gridExtra`
  - `cowplot`
  - `viridis`
  - `asht`
  - `purrr`

## File Structure

```
├── t-test_simulation.R    # Main simulation code
├── t-test_figures.R       # Plotting and visualization code
├── supplementary.R        # additional analysis
├── functions.R            # all fucntions
└── results/               # Output directory for simulation results
```

## Usage

### Running Simulations

```r
# Run the main simulation
source("t-test_simulation.R")
```

### Generating Figures

```r
# Create plots and visualizations
source("t-test_figures.R")
```

### Output Files

Simulation results are saved as RDS files with the following naming convention:

```
[Study]results[Total N]_E[Effect Size]_[Sample Ratio].RDS
```

**Examples:**
- `S22results60_E2_2.RDS` → Study 2-2, N=60, effect size=0.2, sample ratio=2:1
- `S24results120_E5_1.RDS` → Study 2-4, N=120, effect size=0.5, sample ratio=1:1

Simulation results and reproducibility materials will be available at OSF.

## Data
### Key Parameters

- **Effect sizes (d):** 0.0, 0.2, 0.5, 0.8
- **Total sample sizes (N):** 60, 120, 240
- **Sample ratios:** 1:1, 2:1, 1:2
- **Variance ratios:** 1:1, 2:1, 1:2
- **Replications:** 30,000 per condition

## Simulation Conditions

The simulation evaluates five main conditions:

| Condition | Sample Ratio (n₁:n₂) | Variance Ratio (σ₁²:σ₂²) |
|-----------|----------------------|---------------------------|
| A         | 1:1                  | 1:1                       |
| B         | 1:1                  | 2:1                       |
| C         | 2:1                  | 1:1                       |
| D         | 2:1                  | 2:1                       |
| E         | 1:2                  | 2:1                       |

## Reproducibility

This code reproduces all simulation results presented in the associated research paper. Each simulation uses fixed seeds for complete reproducibility.


## Contact

For questions or issues, please contact [cherish4@g.skku.edu] or open an issue on this repository.
