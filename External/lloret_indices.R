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

resistance <- function(x, years, n = 3) {
  res_dr(x, years) / res_predr(x, years, n)
}

recovery <- function(x, years, n = 3) {
  res_postdr(x, years, n) / res_dr(x, years)
}

resilience <- function(x, years, n = 3) {
  res_postdr(x, years, n) / res_predr(x, years, n)
}
