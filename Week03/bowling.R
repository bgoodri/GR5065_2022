Pr <- function(x, n = 10, Upsilon = 0) {
  b <- n + 2 + Upsilon
  ifelse(x > n, 0, log(1 + 1 / (n + 1 + Upsilon - x), b)) / (1 - log(1 + Upsilon, b))
}

Omega <- 0:10
names(Omega) <- as.character(Omega)
