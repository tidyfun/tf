library(tf)

x <- tf_rgp(50) |> tfb_fpc()

y <- tf_rgp(15, arg = 11L) |> tf_sparsify()

y_xbase <- tf_rebase(y, x)

plot(y, lwd = 2)
lines(y_xbase, lty = 2, col = 2)

all.equal(tf_basis(y_xbase, as_tfd = TRUE),
          tf_basis(x, as_tfd = TRUE))


#------------------------------------------------------------------------------


g <- 201
n <- 50

arg <- seq(0, 1, l = g)
basis <- cbind(sin(2 * pi * arg), cos(2 * pi * arg),
               sin(4 * pi * arg), cos(4 * pi * arg))
norms <- tfd(t(basis))^2 |> tf_integrate() |> sqrt()
basis <- basis/norms

true_scores <- t(mvtnorm::rmvnorm(n, mean = rep(0, 4), sigma = diag((4:1)^2)))

data <- t(basis %*% true_scores)
data[rbinom(n*g, 1, .3)] <- NA



delta <- c(0, diff(arg))
# trapezoid integration weights:
weights <- 0.5 * c(delta[-1] + head(delta, -1), tail(delta, 1))
mean <- colMeans(data, na.rm = TRUE)
w_mat <- matrix(weights, ncol = length(arg), nrow = nrow(data), byrow = TRUE)
w_mat[is.na(data)] <- 0
data[is.na(data)] <- 0
data_wc <- t((t(data) - mean) * sqrt(t(w_mat)))



pc <- svd(data_wc, nu = 0, nv = min(dim(data)))
pve_observed <- cumsum(pc$d^2) / sum(pc$d^2)
use <- min(which(pve_observed >= .99))

efunctions <- pc$v[, 1:use] / sqrt(weights)

t(efuns) |> tfd() |> plot(col = 2)
t(basis) |> tfd() |> lines()

evalues <- (pc$d[1:use])^2
scores <- .fpc_wsvd_scores(data, efunctions, mean, weights)

plot(as.vector(t(scores)), as.vector(true_scores))
# check results are still orthogonal functions
#
