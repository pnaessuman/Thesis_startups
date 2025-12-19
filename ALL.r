library(dplR)
library(treeclim)
library(pointRes)
library(tidyverse)
library(VSLiteR)
library(ggplot2)
library(DEoptim)

source("External/Vs.R")
source("External/lloret_indices.R")
beech <- read.rwl("Data/bausenberg_beech.rwl")
oak <- read.rwl("Data/bausenberg_oak.rwl")
climate <- read.csv2("Data/climate_bausenberg.csv")[, c(1, 2, 3, 6)]

Beech_d <- detrend(beech, method = "Spline", nyrs = 32)
Oak_d <- detrend(oak, method = "Spline", nyrs = 32)

beech_1 <- Beech_d %>% 
  na.omit()

oak_1 <- oak_d %>% 
  na.omit()

beech_1$Beech_05

# Get the years from row names
years <- as.numeric(rownames(oak_1))

# Create a separate data frame for EACH tree column
for (tree_name in names(oak_1)) {
  # Create the data frame with year + this tree's data
  df <- data.frame(
    year = years,
    std = oak_1[[tree_name]]
  )
  
  # Name the data frame after the tree
  # Creates: Oak_01_df, Oak_05_df, Oak_10_df, etc.
  assign(paste0(tree_name, "_df"), df, envir = .GlobalEnv)
  
  cat("Created:", tree_name, "_df\n")
}

# List all new data frames
ls(pattern = "_df$")

# Check one of them
head(Oak_01_df)
head(climate)

# Get the years from row names
years <- as.numeric(rownames(beech_1))

# Create a separate data frame for EACH tree column
for (tree_name in names(beech_1)) {
  # Create the data frame with year + this tree's data
  df <- data.frame(
    year = years,
    std = beech_1[[tree_name]]
  )
  
  assign(paste0(tree_name, "_df"), df, envir = .GlobalEnv)
  
  cat("Created:", tree_name, "_df\n")
}

# List all new data frames
ls(pattern = "_df$")

head(Beech_01_df)

drought_years <- c(1976, 2003)

# Comparison of Drought Response 1976 and 2003 (Beech)

Beech_01_res <- resistance(Beech_01_df, drought_years)
Beech_02_res <- resistance(Beech_02_df, drought_years)
Beech_03_res <- resistance(Beech_03_df, drought_years)
Beech_04_res <- resistance(Beech_04_df, drought_years)
Beech_05_res <- resistance(Beech_05_df, drought_years)
Beech_06_res <- resistance(Beech_06_df, drought_years)
Beech_07_res <- resistance(Beech_07_df, drought_years)
Beech_08_res <- resistance(Beech_08_df, drought_years)
Beech_09_res <- resistance(Beech_09_df, drought_years)
Beech_10_res <- resistance(Beech_10_df, drought_years)
Beech_11_res <- resistance(Beech_11_df, drought_years)
Beech_12_res <- resistance(Beech_12_df, drought_years)
Beech_13_res <- resistance(Beech_13_df, drought_years)
Beech_14_res <- resistance(Beech_14_df, drought_years)
Beech_15_res <- resistance(Beech_15_df, drought_years)


Beech_01_rsl <- resilience(Beech_01_df, drought_years)
Beech_02_rsl <- resilience(Beech_02_df, drought_years)
Beech_03_rsl <- resilience(Beech_03_df, drought_years)
Beech_04_rsl <- resilience(Beech_04_df, drought_years)
Beech_05_rsl <- resistance(Beech_05_df, drought_years)
Beech_06_rsl <- resilience(Beech_06_df, drought_years)
Beech_07_rsl <- resilience(Beech_07_df, drought_years)
Beech_08_rsl <- resilience(Beech_08_df, drought_years)
Beech_09_rsl <- resilience(Beech_09_df, drought_years)
Beech_10_rsl <- resilience(Beech_10_df, drought_years)
Beech_11_rsl <- resilience(Beech_11_df, drought_years)
Beech_12_rsl <- resilience(Beech_12_df, drought_years)
Beech_13_rsl <- resilience(Beech_13_df, drought_years)
Beech_14_rsl <- resilience(Beech_14_df, drought_years)
Beech_15_rsl <- resilience(Beech_15_df, drought_years)


Beech_01_rec <- recovery(Beech_01_df, drought_years)
Beech_02_rec <- recovery(Beech_02_df, drought_years)
Beech_03_rec <- recovery(Beech_03_df, drought_years)
Beech_04_rec <- recovery(Beech_04_df, drought_years)
Beech_05_rec <- recovery(Beech_05_df, drought_years)
Beech_06_rec <- recovery(Beech_06_df, drought_years)
Beech_07_rec <- recovery(Beech_07_df, drought_years)
Beech_08_rec <- recovery(Beech_08_df, drought_years)
Beech_09_rec <- recovery(Beech_09_df, drought_years)
Beech_10_rec <- recovery(Beech_10_df, drought_years)
Beech_11_rec <- recovery(Beech_11_df, drought_years)
Beech_12_rec <- recovery(Beech_12_df, drought_years)
Beech_13_rec <- recovery(Beech_13_df, drought_years)
Beech_14_rec <- recovery(Beech_14_df, drought_years)
Beech_15_rec <- recovery(Beech_15_df, drought_years)

# Comparison of Drought Response 1976 and 2003 (Oak)

Oak_01_res <- resistance(Oak_01_df, drought_years)
Oak_02_res <- resistance(Oak_02_df, drought_years)
Oak_03_res <- resistance(Oak_03_df, drought_years)
Oak_04_res <- resistance(Oak_04_df, drought_years)
Oak_05_res <- resistance(Oak_05_df, drought_years)
Oak_06_res <- resistance(Oak_06_df, drought_years)
Oak_07_res <- resistance(Oak_07_df, drought_years)
Oak_08_res <- resistance(Oak_08_df, drought_years)
Oak_09_res <- resistance(Oak_09_df, drought_years)
Oak_10_res <- resistance(Oak_10_df, drought_years)
Oak_12_res <- resistance(Oak_12_df, drought_years)
Oak_13_res <- resistance(Oak_13_df, drought_years)
Oak_14_res <- resistance(Oak_14_df, drought_years)
Oak_15_res <- resistance(Oak_15_df, drought_years)


Oak_01_rsl <- resilience(Oak_01_df, drought_years)
Oak_02_rsl <- resilience(Oak_02_df, drought_years)
Oak_03_rsl <- resilience(Oak_03_df, drought_years)
Oak_04_rsl <- resilience(Oak_04_df, drought_years)
Oak_05_rsl <- resilience(Oak_05_df, drought_years)
Oak_06_rsl <- resilience(Oak_06_df, drought_years)
Oak_07_rsl <- resilience(Oak_07_df, drought_years)
Oak_08_rsl <- resilience(Oak_08_df, drought_years)
Oak_09_rsl <- resilience(Oak_09_df, drought_years)
Oak_10_rsl <- resilience(Oak_10_df, drought_years)
Oak_12_rsl <- resilience(Oak_12_df, drought_years)
Oak_13_rsl <- resilience(Oak_13_df, drought_years)
Oak_14_rsl <- resilience(Oak_14_df, drought_years)
Oak_15_rsl <- resilience(Oak_15_df, drought_years)


Oak_01_rec <- recovery(Oak_01_df, drought_years)
Oak_02_rec <- recovery(Oak_02_df, drought_years)
Oak_03_rec <- recovery(Oak_03_df, drought_years)
Oak_04_rec <- recovery(Oak_04_df, drought_years)
Oak_05_rec <- recovery(Oak_05_df, drought_years)
Oak_06_rec <- recovery(Oak_06_df, drought_years)
Oak_07_rec <- recovery(Oak_07_df, drought_years)
Oak_08_rec <- recovery(Oak_08_df, drought_years)
Oak_09_rec <- recovery(Oak_09_df, drought_years)
Oak_10_rec <- recovery(Oak_10_df, drought_years)
Oak_12_rec <- recovery(Oak_12_df, drought_years)
Oak_13_rec <- recovery(Oak_13_df, drought_years)
Oak_14_rec <- recovery(Oak_14_df, drought_years)
Oak_15_rec <- recovery(Oak_15_df, drought_years)


# VS-Lite model run

estimate_params <- function(chronology, climate_data, phi = 50, .iter = 200){
  input_historic <- make_vsinput_historic_indiv(chronology, climate_data)
  
  params <- vs_params(
    input_historic$std,
    input_historic$tmean,
    input_historic$prec,
    input_historic$syear,
    input_historic$eyear,
    .phi = phi,
    iter = .iter
  )
  
  return(params)
}


run_vslite_forward <- function(params, climate_data, phi = 50, .iter = 200) {
  
  # Create transient input from climate data
  input_transient <- make_vsinput_transient(climate_data)
  
  # Run forward projection
  forward_result <- vs_run_forward(
    params,
    input_transient$tmean,
    input_transient$prec,
    input_transient$syear,
    input_transient$eyear,
    .phi = phi
  )
  
  # Convert to data frame
  result_df <- data.frame(
    year = forward_result[, 1],
    projected_growth = forward_result[, 2]
  )
  
  return(result_df)
}

# Beech

Beech_01_p <- estimate_params(Beech_01_df, climate, .iter = 200)
Beech_01_f <- run_vslite_forward(Beech_01_p, climate)

Beech_02_p <- estimate_params(Beech_02_df, climate, .iter = 200)
Beech_02_f <- run_vslite_forward(Beech_02_p, climate)

Beech_03_p <- estimate_params(Beech_03_df, climate, .iter = 200)
Beech_03_f <- run_vslite_forward(Beech_03_p, climate)

Beech_04_p <- estimate_params(Beech_04_df, climate, .iter = 200)
Beech_04_f <- run_vslite_forward(Beech_04_p, climate)

Beech_05_p <- estimate_params(Beech_05_df, climate, .iter = 200)
Beech_05_f <- run_vslite_forward(Beech_05_p, climate)

Beech_06_p <- estimate_params(Beech_06_df, climate, .iter = 200)
Beech_06_f <- run_vslite_forward(Beech_06_p, climate)

Beech_07_p <- estimate_params(Beech_07_df, climate, .iter = 200)
Beech_07_f <- run_vslite_forward(Beech_07_p, climate)

Beech_08_p <- estimate_params(Beech_08_df, climate, .iter = 200)
Beech_08_f <- run_vslite_forward(Beech_08_p, climate)

Beech_09_p <- estimate_params(Beech_09_df, climate, .iter = 200)
Beech_09_f <- run_vslite_forward(Beech_09_p, climate)

Beech_10_p <- estimate_params(Beech_10_df, climate, .iter = 200)
Beech_10_f <- run_vslite_forward(Beech_10_p, climate)

Beech_11_p <- estimate_params(Beech_11_df, climate, .iter = 200)
Beech_11_f <- run_vslite_forward(Beech_11_p, climate)

Beech_12_p <- estimate_params(Beech_12_df, climate, .iter = 200)
Beech_12_f <- run_vslite_forward(Beech_12_p, climate)

Beech_13_p <- estimate_params(Beech_13_df, climate, .iter = 200)
Beech_13_f <- run_vslite_forward(Beech_13_p, climate)

Beech_14_p <- estimate_params(Beech_14_df, climate, .iter = 200)
Beech_14_f <- run_vslite_forward(Beech_14_p, climate)

Beech_15_p <- estimate_params(Beech_15_df, climate, .iter = 200)
Beech_15_f <- run_vslite_forward(Beech_15_p, climate)

# Oak

Oak_01_p <- estimate_params(Oak_01_df, climate, .iter = 200)
Oak_01_f <- run_vslite_forward(Oak_01_p, climate)

Oak_02_p <- estimate_params(Oak_02_df, climate, .iter = 200)
Oak_02_f <- run_vslite_forward(Oak_02_p, climate)

Oak_03_p <- estimate_params(Oak_03_df, climate, .iter = 200)
Oak_03_f <- run_vslite_forward(Oak_03_p, climate)

Oak_04_p <- estimate_params(Oak_04_df, climate, .iter = 200)
Oak_04_f <- run_vslite_forward(Oak_04_p, climate)

Oak_05_p <- estimate_params(Oak_05_df, climate, .iter = 200)
Oak_05_f <- run_vslite_forward(Oak_05_p, climate)

Oak_06_p <- estimate_params(Oak_06_df, climate, .iter = 200)
Oak_06_f <- run_vslite_forward(Oak_06_p, climate)

Oak_07_p <- estimate_params(Oak_07_df, climate, .iter = 200)
Oak_07_f <- run_vslite_forward(Oak_07_p, climate)

Oak_08_p <- estimate_params(Oak_08_df, climate, .iter = 200)
Oak_08_f <- run_vslite_forward(Oak_08_p, climate)

Oak_09_p <- estimate_params(Oak_09_df, climate, .iter = 200)
Oak_09_f <- run_vslite_forward(Oak_09_p, climate)

Oak_10_p <- estimate_params(Oak_10_df, climate, .iter = 200)
Oak_10_f <- run_vslite_forward(Oak_10_p, climate)

Oak_12_p <- estimate_params(Oak_12_df, climate, .iter = 200)
Oak_12_f <- run_vslite_forward(Oak_12_p, climate)

Oak_13_p <- estimate_params(Oak_13_df, climate, .iter = 200)
Oak_13_f <- run_vslite_forward(Oak_13_p, climate)

Oak_14_p <- estimate_params(Oak_14_df, climate, .iter = 200)
Oak_14_f <- run_vslite_forward(Oak_14_p, climate)

Oak_15_p <- estimate_params(Oak_15_df, climate, .iter = 200)
Oak_15_f <- run_vslite_forward(Oak_15_p, climate)


#Rescaling forward data and make it ready for lloret indices

rescale_to <- function(x, target) 
{ (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE) * sd(target, na.rm = TRUE) + mean(target, na.rm = TRUE)}


rescale_df <- function(ref_df, fwd_df) {
  
  # Find overlapping years
  common_years <- intersect(ref_df$year, fwd_df$year)
  
  # Subset both datasets to the common years
  ref_subset <- ref_df %>% filter(year %in% common_years)
  fwd_subset <- fwd_df %>% filter(year %in% common_years)
  
  # Ensure they're in the same order (by year)
  ref_subset <- ref_subset %>% arrange(year)
  fwd_subset <- fwd_subset %>% arrange(year)
  
  # Rescale forward projected_growth to match reference std
  # Using simple z-score rescaling
  fwd_scaled <- scale(fwd_subset$projected_growth)
  rescaled_std <- as.numeric(fwd_scaled * sd(ref_subset$std) + mean(ref_subset$std))
  
  # Create the exact output format you requested
  result_df <- data.frame(
    year = common_years,
    std = rescaled_std
  )
  
  return(result_df)
}

Beech_01_r <- rescale_df(Beech_01_df, Beech_01_f)
Beech_02_r <- rescale_df(Beech_02_df, Beech_02_f)
Beech_03_r <- rescale_df(Beech_03_df, Beech_03_f)
Beech_04_r <- rescale_df(Beech_04_df, Beech_04_f)
Beech_05_r <- rescale_df(Beech_05_df, Beech_05_f)
Beech_06_r <- rescale_df(Beech_06_df, Beech_06_f)
Beech_07_r <- rescale_df(Beech_07_df, Beech_07_f)
Beech_08_r <- rescale_df(Beech_08_df, Beech_08_f)
Beech_09_r <- rescale_df(Beech_09_df, Beech_09_f)
Beech_10_r <- rescale_df(Beech_10_df, Beech_10_f)
Beech_11_r <- rescale_df(Beech_11_df, Beech_11_f)
Beech_12_r <- rescale_df(Beech_12_df, Beech_12_f)
Beech_13_r <- rescale_df(Beech_13_df, Beech_13_f)
Beech_14_r <- rescale_df(Beech_14_df, Beech_14_f)
Beech_15_r <- rescale_df(Beech_15_df, Beech_15_f)

Oak_01_r <- rescale_df(Oak_01_df, Oak_01_f)
Oak_02_r <- rescale_df(Oak_02_df, Oak_02_f)
Oak_03_r <- rescale_df(Oak_03_df, Oak_03_f)
Oak_04_r <- rescale_df(Oak_04_df, Oak_04_f)
Oak_05_r <- rescale_df(Oak_05_df, Oak_05_f)
Oak_06_r <- rescale_df(Oak_06_df, Oak_06_f)
Oak_07_r <- rescale_df(Oak_07_df, Oak_07_f)
Oak_08_r <- rescale_df(Oak_08_df, Oak_08_f)
Oak_09_r <- rescale_df(Oak_09_df, Oak_09_f)
Oak_10_r <- rescale_df(Oak_10_df, Oak_10_f)
Oak_12_r <- rescale_df(Oak_12_df, Oak_12_f)
Oak_13_r <- rescale_df(Oak_13_df, Oak_13_f)
Oak_14_r <- rescale_df(Oak_14_df, Oak_14_f)
Oak_15_r <- rescale_df(Oak_15_df, Oak_15_f)


# Run lloret Indices for the Rescaled forward data
# Comparison of Drought Response 1976 and 2003 (Beech Forward)

Beech_01_res_f <- resistance(Beech_01_r, drought_years)
Beech_02_res_f <- resistance(Beech_02_r, drought_years)
Beech_03_res_f <- resistance(Beech_03_r, drought_years)
Beech_04_res_f <- resistance(Beech_04_r, drought_years)
Beech_05_res_f <- resistance(Beech_05_r, drought_years)
Beech_06_res_f <- resistance(Beech_06_r, drought_years)
Beech_07_res_f <- resistance(Beech_07_r, drought_years)
Beech_08_res_f <- resistance(Beech_08_r, drought_years)
Beech_09_res_f <- resistance(Beech_09_r, drought_years)
Beech_10_res_f <- resistance(Beech_10_r, drought_years)
Beech_11_res_f <- resistance(Beech_11_r, drought_years)
Beech_12_res_f <- resistance(Beech_12_r, drought_years)
Beech_13_res_f <- resistance(Beech_13_r, drought_years)
Beech_14_res_f <- resistance(Beech_14_r, drought_years)
Beech_15_res_f <- resistance(Beech_15_r, drought_years)


Beech_01_rsl_f <- resilience(Beech_01_r, drought_years)
Beech_02_rsl_f <- resilience(Beech_02_r, drought_years)
Beech_03_rsl_f <- resilience(Beech_03_r, drought_years)
Beech_04_rsl_f <- resilience(Beech_04_r, drought_years)
Beech_05_rsl_f <- resistance(Beech_05_r, drought_years)
Beech_06_rsl_f <- resilience(Beech_06_r, drought_years)
Beech_07_rsl_f <- resilience(Beech_07_r, drought_years)
Beech_08_rsl_f <- resilience(Beech_08_r, drought_years)
Beech_09_rsl_f <- resilience(Beech_09_r, drought_years)
Beech_10_rsl_f <- resilience(Beech_10_r, drought_years)
Beech_11_rsl_f <- resilience(Beech_11_r, drought_years)
Beech_12_rsl_f <- resilience(Beech_12_r, drought_years)
Beech_13_rsl_f <- resilience(Beech_13_r, drought_years)
Beech_14_rsl_f <- resilience(Beech_14_r, drought_years)
Beech_15_rsl_f <- resilience(Beech_15_r, drought_years)


Beech_01_rec_f <- recovery(Beech_01_r, drought_years)
Beech_02_rec_f <- recovery(Beech_02_r, drought_years)
Beech_03_rec_f <- recovery(Beech_03_r, drought_years)
Beech_04_rec_f <- recovery(Beech_04_r, drought_years)
Beech_05_rec_f <- recovery(Beech_05_r, drought_years)
Beech_06_rec_f <- recovery(Beech_06_r, drought_years)
Beech_07_rec_f <- recovery(Beech_07_r, drought_years)
Beech_08_rec_f <- recovery(Beech_08_r, drought_years)
Beech_09_rec_f <- recovery(Beech_09_r, drought_years)
Beech_10_rec_f <- recovery(Beech_10_r, drought_years)
Beech_11_rec_f <- recovery(Beech_11_r, drought_years)
Beech_12_rec_f <- recovery(Beech_12_r, drought_years)
Beech_13_rec_f <- recovery(Beech_13_r, drought_years)
Beech_14_rec_f <- recovery(Beech_14_r, drought_years)
Beech_15_rec_f <- recovery(Beech_15_r, drought_years)

# Comparison of Drought Response 1976 and 2003 (Oak Forward)

Oak_01_res_f <- resistance(Oak_01_r, drought_years)
Oak_02_res_f <- resistance(Oak_02_r, drought_years)
Oak_03_res_f <- resistance(Oak_03_r, drought_years)
Oak_04_res_f <- resistance(Oak_04_r, drought_years)
Oak_05_res_f <- resistance(Oak_05_r, drought_years)
Oak_06_res_f <- resistance(Oak_06_r, drought_years)
Oak_07_res_f <- resistance(Oak_07_r, drought_years)
Oak_08_res_f <- resistance(Oak_08_r, drought_years)
Oak_09_res_f <- resistance(Oak_09_r, drought_years)
Oak_10_res_f <- resistance(Oak_10_r, drought_years)
Oak_12_res_f <- resistance(Oak_12_r, drought_years)
Oak_13_res_f <- resistance(Oak_13_r, drought_years)
Oak_14_res_f <- resistance(Oak_14_r, drought_years)
Oak_15_res_f <- resistance(Oak_15_r, drought_years)


Oak_01_rsl_f <- resilience(Oak_01_r, drought_years)
Oak_02_rsl_f <- resilience(Oak_02_r, drought_years)
Oak_03_rsl_f <- resilience(Oak_03_r, drought_years)
Oak_04_rsl_f <- resilience(Oak_04_r, drought_years)
Oak_05_rsl_f <- resilience(Oak_05_r, drought_years)
Oak_06_rsl_f <- resilience(Oak_06_r, drought_years)
Oak_07_rsl_f <- resilience(Oak_07_r, drought_years)
Oak_08_rsl_f <- resilience(Oak_08_r, drought_years)
Oak_09_rsl_f <- resilience(Oak_09_r, drought_years)
Oak_10_rsl_f <- resilience(Oak_10_r, drought_years)
Oak_12_rsl_f <- resilience(Oak_12_r, drought_years)
Oak_13_rsl_f <- resilience(Oak_13_r, drought_years)
Oak_14_rsl_f <- resilience(Oak_14_r, drought_years)
Oak_15_rsl_f <- resilience(Oak_15_r, drought_years)


Oak_01_rec_f <- recovery(Oak_01_r, drought_years)
Oak_02_rec_f <- recovery(Oak_02_r, drought_years)
Oak_03_rec_f <- recovery(Oak_03_r, drought_years)
Oak_04_rec_f <- recovery(Oak_04_r, drought_years)
Oak_05_rec_f <- recovery(Oak_05_r, drought_years)
Oak_06_rec_f <- recovery(Oak_06_r, drought_years)
Oak_07_rec_f <- recovery(Oak_07_r, drought_years)
Oak_08_rec_f <- recovery(Oak_08_r, drought_years)
Oak_09_rec_f <- recovery(Oak_09_r, drought_years)
Oak_10_rec_f <- recovery(Oak_10_r, drought_years)
Oak_12_rec_f <- recovery(Oak_12_r, drought_years)
Oak_13_rec_f <- recovery(Oak_13_r, drought_years)
Oak_14_rec_f <- recovery(Oak_14_r, drought_years)
Oak_15_rec_f <- recovery(Oak_15_r, drought_years)

####

all_data <- bind_rows(
  # Beech original data
  tibble(
    tree_id = paste0("Beech_", sprintf("%02d", 1:15)),
    resistance = map(list(Beech_01_res, Beech_02_res, Beech_03_res, Beech_04_res,
                          Beech_05_res, Beech_06_res, Beech_07_res, Beech_08_res,
                          Beech_09_res, Beech_10_res, Beech_11_res, Beech_12_res,
                          Beech_13_res, Beech_14_res, Beech_15_res), ~ .x),
    resilience = map(list(Beech_01_rsl, Beech_02_rsl, Beech_03_rsl, Beech_04_rsl,
                          Beech_05_rsl, Beech_06_rsl, Beech_07_rsl, Beech_08_rsl,
                          Beech_09_rsl, Beech_10_rsl, Beech_11_rsl, Beech_12_rsl,
                          Beech_13_rsl, Beech_14_rsl, Beech_15_rsl), ~ .x),
    recovery = map(list(Beech_01_rec, Beech_02_rec, Beech_03_rec, Beech_04_rec,
                        Beech_05_rec, Beech_06_rec, Beech_07_rec, Beech_08_rec,
                        Beech_09_rec, Beech_10_rec, Beech_11_rec, Beech_12_rec,
                        Beech_13_rec, Beech_14_rec, Beech_15_rec), ~ .x)
  ) %>%
    pivot_longer(cols = c(resistance, resilience, recovery), 
                 names_to = "index_type", values_to = "index_data") %>%
    unnest(index_data) %>%
    rename(index_value = index) %>%
    mutate(species = "Beech", data_type = "original"),
  
  # Beech forward data
  tibble(
    tree_id = paste0("Beech_", sprintf("%02d", 1:15)),
    resistance = map(list(Beech_01_res_f, Beech_02_res_f, Beech_03_res_f, Beech_04_res_f,
                          Beech_05_res_f, Beech_06_res_f, Beech_07_res_f, Beech_08_res_f,
                          Beech_09_res_f, Beech_10_res_f, Beech_11_res_f, Beech_12_res_f,
                          Beech_13_res_f, Beech_14_res_f, Beech_15_res_f), ~ .x),
    resilience = map(list(Beech_01_rsl_f, Beech_02_rsl_f, Beech_03_rsl_f, Beech_04_rsl_f,
                          Beech_05_rsl_f, Beech_06_rsl_f, Beech_07_rsl_f, Beech_08_rsl_f,
                          Beech_09_rsl_f, Beech_10_rsl_f, Beech_11_rsl_f, Beech_12_rsl_f,
                          Beech_13_rsl_f, Beech_14_rsl_f, Beech_15_rsl_f), ~ .x),
    recovery = map(list(Beech_01_rec_f, Beech_02_rec_f, Beech_03_rec_f, Beech_04_rec_f,
                        Beech_05_rec_f, Beech_06_rec_f, Beech_07_rec_f, Beech_08_rec_f,
                        Beech_09_rec_f, Beech_10_rec_f, Beech_11_rec_f, Beech_12_rec_f,
                        Beech_13_rec_f, Beech_14_rec_f, Beech_15_rec_f), ~ .x)
  ) %>%
    pivot_longer(cols = c(resistance, resilience, recovery), 
                 names_to = "index_type", values_to = "index_data") %>%
    unnest(index_data) %>%
    rename(index_value = index) %>%
    mutate(species = "Beech", data_type = "forward"),
  
  # Oak original data
  tibble(
    tree_id = paste0("Oak_", sprintf("%02d", c(1:10, 12:15))),
    resistance = map(list(Oak_01_res, Oak_02_res, Oak_03_res, Oak_04_res,
                          Oak_05_res, Oak_06_res, Oak_07_res, Oak_08_res,
                          Oak_09_res, Oak_10_res, Oak_12_res, Oak_13_res,
                          Oak_14_res, Oak_15_res), ~ .x),
    resilience = map(list(Oak_01_rsl, Oak_02_rsl, Oak_03_rsl, Oak_04_rsl,
                          Oak_05_rsl, Oak_06_rsl, Oak_07_rsl, Oak_08_rsl,
                          Oak_09_rsl, Oak_10_rsl, Oak_12_rsl, Oak_13_rsl,
                          Oak_14_rsl, Oak_15_rsl), ~ .x),
    recovery = map(list(Oak_01_rec, Oak_02_rec, Oak_03_rec, Oak_04_rec,
                        Oak_05_rec, Oak_06_rec, Oak_07_rec, Oak_08_rec,
                        Oak_09_rec, Oak_10_rec, Oak_12_rec, Oak_13_rec,
                        Oak_14_rec, Oak_15_rec), ~ .x)
  ) %>%
    pivot_longer(cols = c(resistance, resilience, recovery), 
                 names_to = "index_type", values_to = "index_data") %>%
    unnest(index_data) %>%
    rename(index_value = index) %>%
    mutate(species = "Oak", data_type = "original"),
  
  # Oak forward data
  tibble(
    tree_id = paste0("Oak_", sprintf("%02d", c(1:10, 12:15))),
    resistance = map(list(Oak_01_res_f, Oak_02_res_f, Oak_03_res_f, Oak_04_res_f,
                          Oak_05_res_f, Oak_06_res_f, Oak_07_res_f, Oak_08_res_f,
                          Oak_09_res_f, Oak_10_res_f, Oak_12_res_f, Oak_13_res_f,
                          Oak_14_res_f, Oak_15_res_f), ~ .x),
    resilience = map(list(Oak_01_rsl_f, Oak_02_rsl_f, Oak_03_rsl_f, Oak_04_rsl_f,
                          Oak_05_rsl_f, Oak_06_rsl_f, Oak_07_rsl_f, Oak_08_rsl_f,
                          Oak_09_rsl_f, Oak_10_rsl_f, Oak_12_rsl_f, Oak_13_rsl_f,
                          Oak_14_rsl_f, Oak_15_rsl_f), ~ .x),
    recovery = map(list(Oak_01_rec_f, Oak_02_rec_f, Oak_03_rec_f, Oak_04_rec_f,
                        Oak_05_rec_f, Oak_06_rec_f, Oak_07_rec_f, Oak_08_rec_f,
                        Oak_09_rec_f, Oak_10_rec_f, Oak_12_rec_f, Oak_13_rec_f,
                        Oak_14_rec_f, Oak_15_rec_f), ~ .x)
  ) %>%
    pivot_longer(cols = c(resistance, resilience, recovery), 
                 names_to = "index_type", values_to = "index_data") %>%
    unnest(index_data) %>%
    rename(index_value = index) %>%
    mutate(species = "Oak", data_type = "forward")
) %>%
  mutate(
    drought_year = factor(year),
    index_type = factor(index_type, levels = c("resistance", "resilience", "recovery")),
    data_type = factor(data_type, levels = c("original", "forward"))
  )

# 2. CREATE BOX PLOTS - SIMPLE VERSION
# Function to create box plots for a specific species and index
create_boxplot <- function(species_name, index_name) {
  
  plot_data <- all_data %>%
    filter(species == species_name, index_type == index_name)
  
  ggplot(plot_data, aes(x = data_type, y = index_value, fill = data_type)) +
    geom_boxplot(alpha = 0.8, outlier.shape = NA) +
    geom_jitter(width = 0.2, alpha = 0.6, size = 2, aes(color = data_type)) +
    facet_wrap(~ drought_year, ncol = 2) +
    scale_fill_manual(values = c("original" = "#3498db", "forward" = "#e74c3c")) +
    scale_color_manual(values = c("original" = "#2980b9", "forward" = "#c0392b")) +
    labs(
      title = paste(species_name, "-", str_to_title(index_name)),
      x = "Data Type",
      y = paste(str_to_title(index_name), "Index"),
      fill = "Data Type",
      color = "Data Type"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12),
      legend.position = "bottom",
      strip.text = element_text(size = 11, face = "bold")
    )
}

# 3. CREATE ALL BOX PLOTS
# Beech box plots
beech_resistance_box <- create_boxplot("Beech", "resistance")
beech_resilience_box <- create_boxplot("Beech", "resilience")
beech_recovery_box <- create_boxplot("Beech", "recovery")

# Oak box plots
oak_resistance_box <- create_boxplot("Oak", "resistance")
oak_resilience_box <- create_boxplot("Oak", "resilience")
oak_recovery_box <- create_boxplot("Oak", "recovery")

# 4. DISPLAY ALL PLOTS
beech_resistance_box
beech_resilience_box
beech_recovery_box

oak_resistance_box
oak_resilience_box
oak_recovery_box
