# Dr, PreDr, PostDr
# Resistance = Dr / PreDr
# Recovery = PostDr / Dr
# Resilience = PostDr / PreDr

res_dr <- function(x, years) {
  x[as.numeric(rownames(x)) %in% years, ]
}

res_predr <- function(x, years, n = 3) {
  sapply(years, function(y) {
    mean(x[as.numeric(rownames(x)) %in% c((y - n):(y - 1)), ])
  })
}

res_postdr <- function(x, years, n = 3) {
  sapply(years, function(y) {
    mean(x[as.numeric(rownames(x)) %in% c((y + n):(y + 1)), ])
  })
}

# x: data.frame with dendro data
# y: vector of critical years
# n: number of years before and after the critical
#    years
resistance <- function(x, years, n = 3) {
  out <- res_dr(x, years) / res_predr(x, years, n)
  data.frame(
    year = years,
    index = out
  )
}

recovery <- function(x, years, n = 3) {
  out <- res_postdr(x, years, n) / res_dr(x, years)
  data.frame(
    year = years,
    index = out
  )
}

resilience <- function(x, years, n = 3) {
  out <- res_postdr(x, years, n) / res_predr(x, years, n)
  data.frame(
    year = years,
    index = out
  )
}

# USAGE EXAMPLE

library(dplR)

beech <- read.rwl("data/buche_chrono.rwl")

drought_years <- c(1976, 2003)

resistance(beech, drought_years)

