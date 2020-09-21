###########################################################################################
#TEST the Functions
###########################################################################################
N=600
Setsize=3
H=Setsize
Replace=FALSE #( Without replacement salection  )
#Replace=TRUE  # ( With replacement selection)
Model=0 #( if Model=0, Design based inference, if Model=1, super population model is used)
Method="RSS" # (Sampling method is RSS)
#Method="JPS"  # Sam pling metod is JPS
sig=4  # population standard devistion
mu=10  # population mean
K=3   # number of rankers
n=30  # sample size
#sim=5000
#sim=2
alpha=0.05
rhoV=rep(0.75,K)
tauV=sig*sqrt(1/rhoV^2-1)

pop=qnorm((1:N)/(N+1),mu,sig)
popmean=mean(pop)
#Y =qnorm( (1:N)/(N+1), mu, sig)
rho=0.75
tau=sig*sqrt(1/rho^2-1)
X=pop+tau*rnorm(N,0,1)
if( Method=="RSS")pop=cbind(pop,X)

if(Method=="JPS"){
    if(Replace)  Data=JPSD0F(pop,n,Setsize,tauV,N,K) # This function generates JPS data sampling with replacement
    if(!Replace) Data=JPSD2F(pop,n,Setsize,tauV,N,K) # This function generates JPS data sampling without replacement
}
##############################################################
#####  RSS
if(Method=="RSS"){
    if(Replace)  Data=RSSDF(pop,n,H,K) # This function generates RSS data sampling  with replacement
    if(!Replace) Data= RSSNRF(pop,n,H,K) # This function generates RSS data sampling without replacement
    Data=Data[order(Data[,2]),]
}

ONE=OneSample(Data,Setsize,Method,0.80, Replace,Model,N)
if(Replace) print(paste(Method, "Sample with number of rankers K=", K, "Sampling with replacement")) else
    print(paste(Method, "Sample with number of rankers K=", K, "Sampling without replacement"))
if(Model==0) print("Design based inference is developed") else print("Super-population  model is used")
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
