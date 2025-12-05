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
    trw = oak_1[[tree_name]]
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
    trw = beech_1[[tree_name]]
  )
  
  assign(paste0(tree_name, "_df"), df, envir = .GlobalEnv)
  
  cat("Created:", tree_name, "_df\n")
}

# List all new data frames
ls(pattern = "_df$")

head(Beech_01_df)

