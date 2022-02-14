Pr <- function(x, n = 10, theta = 1) {
  if (theta > 1e-10)
    return(ifelse(x > n, 0, log1p(1 / (n + theta - x))) / log1p((n + 1) / theta))
  if (theta < 0) stop("theta must be >= 0")
  if (theta == 0) return(x == n)
  # formula is not inherently numerically accurate if n == x and theta < 1e-10 so
  Pr_all <- 1 - sum(log1p(1 / (n + theta - 0:(n - 1L)))) / log1p((n + 1) / theta)
  ifelse(x > n, 0,
         ifelse(x == n, Pr_all,
                log1p(1 / (n + theta - x)) / log1p((n + 1) / theta)))
}

Omega <- 0:10
names(Omega) <- as.character(Omega)
