library(dplR)
library(treeclim)
library(pointRes)


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
climate_years_b <- climate_data[climate_beech$year %in% c(1976, 2003), ]

climate_spruce$year <- as.numeric(climate_spruce$year)  # Ensure year is numeric
climate_years_s <- climate_data[climate_spruce$year %in% c(1976, 2003), ]

