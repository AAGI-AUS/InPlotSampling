N <- 600 # population size
set_size <- 3
H <- set_size # the number of samples to be ranked in each set
with_replacement <- FALSE

model_based <- FALSE
sampling_method <- "JPS" # JPS, RSS

sigma <- 4 # population standard deviation
mu <- 10 # population mean
n_rankers <- 3 # number of rankers
n <- 30 # sample size

alpha <- 0.05
rhos <- rep(0.75, n_rankers)
taus <- sigma * sqrt(1 / rhos^2 - 1)

pop <- qnorm((1:N) / (N + 1), mu, sigma)
rho <- 0.75
tau <- sigma * sqrt(1 / rho^2 - 1)
X <- pop + tau * rnorm(N, 0, 1)

# JPS sampling
if (sampling_method == "JPS") {
  data <- JPSD2F(pop, n, H, taus, n_rankers, with_replacement)
}

#  Ranked set sampling
if (sampling_method == "RSS") {
  pop <- cbind(pop, X)
  data <- RSS(pop, n, H, n_rankers, with_replacement)
  data <- data[order(data[, 2]), ]
}

ONE <- rss_jps_estimate(data, set_size, sampling_method, 0.80, with_replacement, model_based, N)

w_or_wo <- " without "
if (with_replacement) {
  w_or_wo <- " with "
}
print(paste0(sampling_method, " sample with number of rankers K=", n_rankers, w_or_wo, "replacement"))

if (model_based == 0) {
  print("Design based inference is developed")
} else {
  print("Super-population  model is used")
}
print(ONE)


# # Example case
# N <- 150
# Setsize <- 3
# H <- Setsize
# Replace <- 0 # ( if Replace=0, Design is D_0, if Replace =1, Design= D_2  )
# Model <- 0 # ( if Model=0, Design based inference, if Model=1, super population model is used)
# sig <- 4
# K <- 6
# n <- 100
# sim <- 50000
# alpha <- 0.05
# rhoM <- matrix(c(
#     rep(0.8, K),
#     rep(0.8, K / 2), rep(0.3, K / 2),
#     rep(0.5, K),
#     rep(0.8, K / 3), rep(0.5, K / 3), rep(0.2, K / 3),
#     1, rep(0.8, (K - 1))
# ), nrow = 5, ncol = K, byrow = TRUE)
# rhoV <- rhoM[2, ]
# # rhoV=0.9
# tauV <- sig * sqrt(1 / rhoV^2 - 1)
# # tau=rep(0.5,K)
# pop <- qnorm((1:N) / (N + 1), 50, 3)
# popmean <- mean(pop)
# storE <- matrix(0, ncol = 6, nrow = sim)
# storV <- matrix(0, ncol = 6, nrow = sim)
# EstimatorID <- c("UnWeighted", "Sd.Weighted", "Aggregate Weight", "JPS Estimate", "SRS estimate", "Minimum")
# colnames(storE) <- EstimatorID
# colnames(storV) <- EstimatorID
# # ( iter in (1:sim)){
# Data0 <- JPSD0F(pop, n, Setsize, K, tauV) # this function generates JPS data from design D_0
# # Data0 <- JPSD2F(pop, n, Setsize, tauV, N, K) # this function generates JPS data from design D_2
#
# ESTREP0 <- JPSLF(Data0, Setsize, Replace, N, model = 0) # this function computes the estimators
# ESTREP1 <- JPSLF(testdata, Setsize, Replace, N, model = 0) # this function computes the estimators
#
#
# # storE[iter,]=ESTREP0[,2]
# # storV[iter,]=ESTREP0[,3]
# # }
# # print(apply(storE,2,var))
# # print(apply(storV,2,mean))
#
# print(ESTREP0)
