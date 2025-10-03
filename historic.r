library(dplR)
library(treeclim)
library(pointRes)

source("External/Vs.R")
beech <- read.rwl("Data/buche_chrono.rwl")
climate_beech <- read.csv2("Data/climate_bausenberg.csv")[, c(1, 2, 3, 6)]
spruce <- read.rwl("Data/spruce_alpine.rwl")
climate_spruce <- read.csv2("Data/climate_alpine.csv")

#Chron

beech_c <- chron(beech)
spruce_c <- chron(spruce)

plot(beech_c)

#filter drought years(1976 and 2003) for both beech and spruce

climate_beech$year <- as.numeric(climate_beech$year)  # Ensure year is numeric
climate_years_b <- climate_beech[climate_beech$year %in% c(1976, 2003), ]

climate_spruce$year <- as.numeric(climate_spruce$year)  
climate_years_s <- climate_spruce[climate_spruce$year %in% c(1976, 2003), ]

#Create Data Frames for Each Species

beech_df <- data.frame(year = as.numeric(rownames(beech_c)), rwi = beech_c$std)
spruce_df <- data.frame(year = as.numeric(rownames(spruce_c)), rwi = spruce_c$std)

# Merge Tree-Ring Data with Climate Data
# 1976

beech_merged_1976 <- merge(beech_df[beech_df$year == 1976, ], climate_beech[climate_beech$year == 1976, ], by = "year")
spruce_merged_1976 <- merge(spruce_df[spruce_df$year == 1976, ], climate_spruce[climate_spruce$year == 1976, ], by = "year")

# 2003

beech_merged_2003 <- merge(beech_df[beech_df$year == 2003, ], climate_beech[climate_beech$year == 2003, ], by = "year")
spruce_merged_2003 <- merge(spruce_df[spruce_df$year == 2003, ], climate_spruce[climate_spruce$year == 2003, ], by = "year")

# Sort by the 3Rs (Recovery, Resistance, Resilience)

beech_res <- res.comp(beech)

