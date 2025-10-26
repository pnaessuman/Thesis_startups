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
climate_beech <- read.csv2("Data/climate_bausenberg.csv")[, c(1, 2, 3, 6)]
spruce <- read.rwl("Data/alpine_spruce_trees.rwl")
climate_spruce <- read.csv2("Data/climate_alpine.csv")

beech_d <- detrend(beech, method = "Spline", nyrs = 32)
spruce_d <- detrend(spruce, method = "Spline", nyrs = 32)

beech_c <- chron(beech_d)
spruce_c <- chron(spruce_d)

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

beech_res <- res.comp(beech_d)
resist_b <- beech_res$resist
recov_b <- beech_res$recov
resil_b <- beech_res$resil
beech_years <- as.numeric(rownames(resist_b))
beech_resist_1976 <- resist_b[beech_years == 1976, ] 
beech_recov_1976 <- recov_b[beech_years == 1976, ]
beech_resil_1976 <- resil_b[beech_years == 1976, ]

beech_resist_2003 <- resist_b[beech_years == 2003, ] 
beech_recov_2003 <- recov_b[beech_years == 2003, ] 
beech_resil_2003 <- resil_b[beech_years == 2003, ] 


Spruce_res <- res.comp(spruce_d)
resist_s <- Spruce_res$resist
recov_s <- Spruce_res$recov
resil_s <- Spruce_res$resil
spruce_years <- as.numeric(rownames(resist_s))
spruce_resist_1976 <- resist_s[spruce_years == 1976, ]
spruce_recov_1976 <- recov_s[spruce_years == 1976, ]
spruce_resil_1976 <- resil_s[spruce_years == 1976, ]

spruce_resist_2003 <- resist_s[spruce_years == 2003, ] 
spruce_recov_2003 <- recov_s[spruce_years == 2003, ] 
spruce_resil_2003 <- resil_s[spruce_years == 2003, ] 


library(tidyverse)

# Comparison of Drought Response 1976 and 2003

tree_metrics_1976 <- bind_rows(
  tibble(Species = "Beech", Metric = "Resistance", Value = beech_resist_1976),
  tibble(Species = "Beech", Metric = "Recovery",   Value = beech_recov_1976),
  tibble(Species = "Beech", Metric = "Resilience", Value = beech_resil_1976),
  tibble(Species = "Spruce", Metric = "Resistance", Value = spruce_resist_1976),
  tibble(Species = "Spruce", Metric = "Recovery",   Value = spruce_recov_1976),
  tibble(Species = "Spruce", Metric = "Resilience", Value = spruce_resil_1976))

ggplot(tree_metrics_1976, aes(x = Metric, y = Value, fill = Species)) +
  geom_boxplot(outlier.shape = 16, alpha = 0.7) +
  labs(
    title = "Comparison of Drought Response (1976)",
    x = "Drought Metric",
    y = "Value",
    fill = "Species"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top"
  )


tree_metrics_2003 <- bind_rows(
  tibble(Species = "Beech", Metric = "Resistance", Value = beech_resist_2003),
  tibble(Species = "Beech", Metric = "Recovery",   Value = beech_recov_2003),
  tibble(Species = "Beech", Metric = "Resilience", Value = beech_resil_2003),
  tibble(Species = "Spruce", Metric = "Resistance", Value = spruce_resist_2003),
  tibble(Species = "Spruce", Metric = "Recovery",   Value = spruce_recov_2003),
  tibble(Species = "Spruce", Metric = "Resilience", Value = spruce_resil_2003))

ggplot(tree_metrics_2003, aes(x = Metric, y = Value, fill = Species)) +
  geom_boxplot(outlier.shape = 16, alpha = 0.7) +
  labs(
    title = "Comparison of Drought Response (2003)",
    x = "Drought Metric",
    y = "Value",
    fill = "Species"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top"
  )



# Calibration Run & model forward for beech

input_historic_b <- make_vsinput_historic(beech_c, climate_beech)

beech_params <- vs_params(input_historic_b$trw,
                           input_historic_b$tmean,
                           input_historic_b$prec,
                           input_historic_b$syear,
                           input_historic_b$eyear,
                           .phi = 50) # approx. latitude in degrees

#Run model forward
input_transient <- make_vsinput_transient(climate_beech)

beech_forward <- vs_run_forward(beech_params,
                                input_transient$tmean,
                                input_transient$prec,
                                input_transient$syear,
                                input_transient$eyear,
                                .phi = 50)

#Compare the model and observation
beech_observed <- data.frame(
  year = as.numeric(rownames(beech_c)),
  observed = scale(beech_c$std)
)

beech_combined <- merge(beech_observed, beech_forward)
beech_combined$modelled <- scale(beech_combined$trw)
beech_combined$trw <- NULL
beech_combined <- tidyr::pivot_longer(beech_combined, -1, names_to = "variant",
                                      values_to = "rwi")

ggplot(beech_combined, aes(year, rwi)) +
  geom_line(aes(colour = variant))


# Calibration Run & model forward for spruce

#Calibration Run
input_historic_s <- make_vsinput_historic(spruce_c, climate_spruce)

spruce_params <- vs_params(input_historic_s$trw,
                           input_historic_s$temp,
                           input_historic_s$prec,
                           input_historic_s$syear,
                           input_historic_s$eyear,
                           .phi = 50) # approx. latitude in degrees

#Run model forward
input_transient_s <- make_vsinput_transient(climate_spruce)

spruce_forward <- vs_run_forward(spruce_params,
                                 input_transient_s$temp,
                                 input_transient_s$prec,
                                 input_transient_s$syear,
                                 input_transient_s$eyear,
                                 .phi = 50)

#Compare the model and observation
spruce_observed <- data.frame(
  year = as.numeric(rownames(spruce_c)),
  observed = scale(spruce_c$std)
)

spruce_combined <- merge(spruce_observed, spruce_forward)
spruce_combined$modelled <- scale(spruce_combined$trw)
spruce_combined$trw <- NULL
spruce_combined <- tidyr::pivot_longer(spruce_combined, -1, names_to = "variant",
                                       values_to = "rwi")

ggplot(spruce_combined, aes(year, rwi)) +
  geom_line(aes(colour = variant))

cor (spruce_combined$rwi [spruce_combined$variant == "modelled"], 
     spruce_combined$rwi [spruce_combined$variant == "observed"])
