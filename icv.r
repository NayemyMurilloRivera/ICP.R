# cuantiles chi-cuadrado
chi_inf <- function(alpha, n) {
  qchisq(1 - alpha/2, n - 1)
}

chi_sup <- function(alpha, n) {
  qchisq(alpha/2, n - 1)
}

# intervalo de confianza para la varianza
ic_varianza <- function(s2, alpha, n) {

  chi1 <- chi_inf(alpha, n)
  chi2 <- chi_sup(alpha, n)

  lim_inf <- ((n - 1) * s2) / chi1
  lim_sup <- ((n - 1) * s2) / chi2

  cat("IC de la varianza = [",
      round(lim_inf, 4), ",",
      round(lim_sup, 4), "]")
}
s <- c(46.4,46.1,45.8,47.0,46.1,45.9,45.2,46.0,45.8,46.9)
s2 <- var(s)

ic_varianza(s2, 0.05, length(s))