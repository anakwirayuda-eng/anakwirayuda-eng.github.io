# Helper besar sampel dasar untuk pembelajaran biostatistik kedokteran.
# Semua fungsi di bawah ini dimaksudkan untuk latihan dan draft proposal,
# bukan pengganti konsultasi statistik pada desain yang kompleks.

z_alpha <- function(alpha = 0.05) {
  qnorm(1 - alpha / 2)
}

z_power <- function(power = 0.80) {
  qnorm(power)
}

inflate_dropout <- function(n, dropout = 0.10) {
  ceiling(n / (1 - dropout))
}

ss_prop_descriptive <- function(p = 0.50, d = 0.10, alpha = 0.05) {
  z <- z_alpha(alpha)
  ceiling((z^2 * p * (1 - p)) / (d^2))
}

ss_mean_descriptive <- function(sd, d, alpha = 0.05) {
  z <- z_alpha(alpha)
  ceiling((z^2 * sd^2) / (d^2))
}

ss_two_prop <- function(p1, p2, alpha = 0.05, power = 0.80, dropout = 0.10) {
  za <- z_alpha(alpha)
  zb <- z_power(power)
  pbar <- (p1 + p2) / 2
  numerator <- (za * sqrt(2 * pbar * (1 - pbar)) + zb * sqrt(p1 * (1 - p1) + p2 * (1 - p2)))^2
  n_each <- ceiling(numerator / ((p1 - p2)^2))
  inflate_dropout(n_each, dropout)
}

ss_two_mean <- function(sd, delta, alpha = 0.05, power = 0.80, dropout = 0.10) {
  za <- z_alpha(alpha)
  zb <- z_power(power)
  n_each <- ceiling((2 * (sd^2) * (za + zb)^2) / (delta^2))
  inflate_dropout(n_each, dropout)
}

ss_correlation <- function(r, alpha = 0.05, power = 0.80, dropout = 0.10) {
  za <- z_alpha(alpha)
  zb <- z_power(power)
  fisher <- 0.5 * log((1 + r) / (1 - r))
  n <- ceiling(((za + zb)^2 / (fisher^2)) + 3)
  inflate_dropout(n, dropout)
}

example_run <- function() {
  list(
    deskriptif_proporsi = ss_prop_descriptive(p = 0.30, d = 0.08),
    deskriptif_mean = ss_mean_descriptive(sd = 12, d = 4),
    dua_proporsi = ss_two_prop(p1 = 0.30, p2 = 0.50),
    dua_rerata = ss_two_mean(sd = 10, delta = 5),
    korelasi = ss_correlation(r = 0.35)
  )
}

if (!interactive()) {
  print(example_run())
}

