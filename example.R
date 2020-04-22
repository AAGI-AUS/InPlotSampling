# Example case
N <- 150
Setsize <- 3
H <- Setsize
Replace <- 0 # ( if Replace=0, Design is D_0, if Replace =1, Design= D_2  )
Model <- 0 # ( if Model=0, Design based inference, if Model=1, super population model is used)
sig <- 4
K <- 6
n <- 100
sim <- 50000
alpha <- 0.05
rhoM <- matrix(c(
    rep(0.8, K),
    rep(0.8, K / 2), rep(0.3, K / 2),
    rep(0.5, K),
    rep(0.8, K / 3), rep(0.5, K / 3), rep(0.2, K / 3),
    1, rep(0.8, (K - 1))
), nrow = 5, ncol = K, byrow = TRUE)
rhoV <- rhoM[2, ]
# rhoV=0.9
tauV <- sig * sqrt(1 / rhoV^2 - 1)
# tau=rep(0.5,K)
pop <- qnorm((1:N) / (N + 1), 50, 3)
popmean <- mean(pop)
storE <- matrix(0, ncol = 6, nrow = sim)
storV <- matrix(0, ncol = 6, nrow = sim)
EstimatorID <- c("UnWeighted", "Sd.Weighted", "Aggregate Weight", "JPS Estimate", "SRS estimate", "Minimum")
colnames(storE) <- EstimatorID
colnames(storV) <- EstimatorID
# ( iter in (1:sim)){
Data0 <- JPSD0F(pop, n, Setsize, K, tauV) # this function generates JPS data from design D_0
# Data0 <- JPSD2F(pop, n, Setsize, tauV, N, K) # this function generates JPS data from design D_2

ESTREP0 <- JPSLF(Data0, Setsize, Replace, N, Model = 0) # this function computes the estimators
# storE[iter,]=ESTREP0[,2]
# storV[iter,]=ESTREP0[,3]
# }
# print(apply(storE,2,var))
# print(apply(storV,2,mean))

print(ESTREP0)
