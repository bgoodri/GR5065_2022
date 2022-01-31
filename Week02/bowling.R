# probability of knocking down x out of n pins
Pr <- function(x, n = 10) ifelse(x > n, 0, log(1 + 1 / (n + 1 - x), base = n + 2))

Omega <- 0:10 # 0, 1, ..., 10
names(Omega) <- as.character(Omega)

# probability of knocking down x_1 pins on the first roll and x_2 on the second
joint_Pr <- matrix(0, nrow = length(Omega), ncol = length(Omega))
rownames(joint_Pr) <- colnames(joint_Pr) <- names(Omega)
for (x_1 in Omega) {
  Pr_x_1 <- Pr(x_1, n = 10)
  for (x_2 in 0:(10 - x_1))
    joint_Pr[x_1 + 1, x_2 + 1] <- Pr_x_1 * Pr(x_2, n = 10 - x_1)
}

total <- row(joint_Pr) - 1L + col(joint_Pr) - 1L
