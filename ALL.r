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

beech_1 <- beech %>% 
  na.omit()

oak_1 <- oak %>% 
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

# Comparison of Drought Response 1976 and 2003
Beech_01_r <- resistance(Beech_01_df, drought_years)
Beech_02_r <- resistance(Beech_02_df, drought_years)
Beech_03_r <- resistance(Beech_03_df, drought_years)
Beech_04_r <- resistance(Beech_04_df, drought_years)
Beech_05_r <- resistance(Beech_05_df, drought_years)
Beech_06_r <- resistance(Beech_06_df, drought_years)
Beech_07_r <- resistance(Beech_07_df, drought_years)
Beech_08_r <- resistance(Beech_08_df, drought_years)
Beech_09_r <- resistance(Beech_09_df, drought_years)
Beech_10_r <- resistance(Beech_10_df, drought_years)
Beech_11_r <- resistance(Beech_11_df, drought_years)
Beech_12_r <- resistance(Beech_12_df, drought_years)
Beech_13_r <- resistance(Beech_13_df, drought_years)
Beech_14_r <- resistance(Beech_14_df, drought_years)
Beech_15_r <- resistance(Beech_15_df, drought_years)


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



Oak_01_r <- resistance(Oak_01_df, drought_years)
Oak_02_r <- resistance(Oak_02_df, drought_years)
Oak_03_r <- resistance(Oak_03_df, drought_years)
Oak_04_r <- resistance(Oak_04_df, drought_years)
Oak_05_r <- resistance(Oak_05_df, drought_years)
Oak_06_r <- resistance(Oak_06_df, drought_years)
Oak_07_r <- resistance(Oak_07_df, drought_years)
Oak_08_r <- resistance(Oak_08_df, drought_years)
Oak_09_r <- resistance(Oak_09_df, drought_years)
Oak_10_r <- resistance(Oak_10_df, drought_years)
Oak_12_r <- resistance(Oak_12_df, drought_years)
Oak_13_r <- resistance(Oak_13_df, drought_years)
Oak_14_r <- resistance(Oak_14_df, drought_years)
Oak_15_r <- resistance(Oak_15_df, drought_years)


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
  input_historic <- make_vsinput_historic(chronology, climate_data)
  
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


