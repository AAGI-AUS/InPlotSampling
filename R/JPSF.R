

JPSD0F=function(pop,n,H,tau,N,K){
  #tau: controls the ranking quality
  #n:sample size
  # H: Set szie
  # pop: population
  N=length(pop) # population size
  #SRSI=sample(1:N,n,replace=TRUE)
  #SRS=pop[SRSI] # first create a simple randopm sample
  #redpop=pop[-SRSI] # remove the slected SRS from, the population
  #NR=length(redpop) # reduced population size
  pRIn=1:N #  reduced population index
  #################################################
  # below  consruct rank for each SRS unit post experimentally
  JPS=matrix(0,ncol=(K+1),nrow=n) # store JPS sample
  ##############################################
  for (i in (1:n)){
    #Yi=SRS[i] # measured unit
    Compi=sample(pRIn,H) # select H-1 unit to construct compariosn set
    Set=pop[Compi] # combine H-1 unit with the  measured unit Y-i
    Yi=Set[1]
    JPS[i,1]=Yi
    for( k in (2:(K+1))){
      DCSet=Set+tau[k-1]*rnorm(H,0,1) # adjust ranking quality using Dell-Clutter
      # model
      RankSet=rank(DCSet)   # rank the units
      JPS[i,k]=RankSet[1] #  JPS sample for the i-th unit
    }
  }
colnames(JPS)=c("Y",paste("R",1:K,sep=""))
  return(JPS)
}

#############################################
# Ths function generates JPS sample        ##   
#############################################
JPSD2F=function(pop,n,H,tau,N,K){
  # K: Number of rankers
  #tau: controls the ranking quality
  #n:sample size
  # H: Set szie
  # pop: population
  N=length(pop) # population size
  nsets=matrix(sample(pop,n*H),ncol=H,nrow=n)
  #################################################
  # below  consruct rank for each SRS unit post experimentally
  JPS=matrix(0,ncol=K+1,nrow=n) # store JPS sample
  ##############################################
  for (i in (1:n)){
    Set=nsets[i,] #select compariosn set i
    tem=rep(0,K) #initialize to store ranks of he rankers for ocm,parion set i
    for (k in (1:K)){
      DCSet=Set+tau[k]*rnorm(H,0,1) # adjust ranking quality using Dell-Clutter
      # model
      RankSet=rank(DCSet)   # ranks the units in the comparion set i by ranker k
      tem[k]=RankSet[1] #  the rank of the i-th mesured unit by ranker k
    }
    JPS[i,]=c(Set[1],tem)  # meaured value of unit i and ranks by k rankers
  }
  colnames(JPS)=c("Y",paste("R",1:K,sep=""))
  return(JPS)
}


#####################################################################
#####################################################################
#  THis function computes the coefficient of variance estimator  ####
#####################################################################
CoefF=function(H,n){
  # H: set size
  # n: sample size
  kv=1:H
  ######################################################
  # Expected value of I_1^2/d_n^2    ###################
  E.I2.dn2=sum((kv/H)^(n-1))/H^2 # E{(I_1/d_n)^2}    ##
  ######################################################
  
  ######################################################
  # Compute the expected value I_1^2/(n_1 d_n^2)      ##
  ######################################################
  
  indM=rep(0,3)  
  for(k in (2:H)){
    AA=expand.grid((1:(k-1)),1:(n-k+1))
    TEm=cbind(rep(k,dim(AA)[1]),AA)
    indM=rbind(indM,TEm)
  }
  indM=indM[-1,]
  indM=as.matrix(indM)
  triplesum=apply(indM,1,TRIPLEF,n=n,H=H)
  E.I2.n1dn2=(1/n+sum(triplesum))/H^n
  
  #E.I2.n1dn2 <- (1/H^n)*(1/n+tot)
  ######################################################
  ######################################################
  VarI1.dn=E.I2.dn2-(1/H^2)
  coef1D0=1/(H^2)-VarI1.dn/(H-1)
  coef2D0=VarI1.dn
  coef3D0=E.I2.n1dn2-VarI1.dn
  
  CoefD0=c(coef1D0,coef2D0,coef3D0)
  return(CoefD0)
}


TRIPLEF=function(uv,H,n){
  coef1 <- (-1)^(uv[2]-1)/(uv[1]^2*uv[3])
  coef2 <- choose((H-1),(uv[1]-1))
  coef3 <- choose((uv[1]-1),(uv[2]-1))
  coef4 <- choose(n,uv[3])
  coef5 <- (uv[1]-uv[2])^(n-uv[3])
  ret=coef1*coef2*coef3*coef4*coef5
  return(ret)
}


###########################################################
# This function Computes JPS estimator and its variance  ##
###########################################################
#JPSD0: 
#First column: Response
#Second column: Ranks
JPSED0F=function(RV, Y, H,Coef,N,Replace,Model){
  # print(Coef)
  RVD=data.frame(RV)  
  M.est=mean(aggregate(Y,RVD,mean)$x)  # JPS estimate
  YIYJ=expand.grid(Y,Y)
  GSample.Size=aggregate(RV,data.frame(RV),length)$x
  dn=length(GSample.Size)
  #print(dn)
  GSample.Size1=GSample.Size[GSample.Size>1]
  dn.star=length(GSample.Size1)
  RhRhp=expand.grid(RV,RV)
  YIYJ2=(YIYJ[,1]-YIYJ[,2])^2
  group.mean= aggregate(YIYJ2,RhRhp,mean)
  Y2hhT2=group.mean[group.mean[,1]- group.mean[,2]==0,]$x
  Y2hhT2=Y2hhT2[GSample.Size>1]
  T2s=H*sum(Y2hhT2*GSample.Size1^2/(GSample.Size1*(GSample.Size1-1)))/(2*dn.star)
  Y2hhT1=group.mean[group.mean[,1]- group.mean[,2]!=0,]$x
  T1s=sum(Y2hhT1)/(2*Coef[1]*dn^2)
  VestD0=Coef[2]*T1s/(H-1)+Coef[3]*T2s
  if(Replace ==1)  {VEST=Coef[2]*T2s+Coef[3]*(N-1)*(T1s+T2s)/(N*(H-1));
  if(VEST <= 0) VEST=Coef[2]*T2s/2} else VEST= Coef[2]*T1s/(H-1)+Coef[3]*T2s   
  if(Model==1) {VEST=(T1s+T2s)/H^2*((-1/N)+Coef[2]*H/(H-1))+T2s*((Coef[3]+Coef[2])+Coef[2]*H/(H-1))
  if(VEST <= 0 ) VEST=T2s*((Coef[3]+Coef[2])+Coef[2]*H/(H-1))
  }
  return(c(M.est,VEST))
}







WEIGHTF=function(RV,H){
  UniquR=unique(RV)
  weightV=rep(0,H)
  K=length(RV)
  for( h in (1:length(UniquR))){
    UR=UniquR[h]  
    weightV[UR]=length(RV[RV==UR])/K  
  }
  return(weightV)
}




ListF=function(Data,Setsize,Replace,N,Model,CoefD0){
  Y=Data[,1]
  Ranks=Data[,-1]
  n=length(Y)
  H=Setsize
  # CoefD0=CoefD0F(Setsize,n)
  # print(c(N,Replace))
  ##################################################################
  #  For design D2 coefficients
  if(Replace ==1){
    if(is.null(N) ){print("Population size N must be provided for without replacement sampling")} 
    coef1D2=CoefD0[1]
    coef2D2=1/(H*(H-1))+CoefD0[3]+CoefD0[2]-(CoefD0[2]+1/H^2)*H/(H-1)
    coef3D2=CoefD0[2]-1/(N-1)*(1/H-(CoefD0[1]+1/H^2))
    CoefD=c(coef1D2,coef2D2,coef3D2)} else CoefD=CoefD0
  if(Model==1) CoefD=CoefD0
  ##################################################################
  #print(Ranks)
  WEIGHT=t(apply(data.frame(Ranks),1,WEIGHTF,H=Setsize))
  #print(WEIGHT)
  WY=Y%*%WEIGHT
  Eff.Ssize=apply(WEIGHT,2,sum)
  W.est=mean(WY[Eff.Ssize>0]/Eff.Ssize[Eff.Ssize>0])
  
  EST=t(apply(data.frame(Ranks),2,JPSED0F, Y=Y, H=Setsize,Coef=CoefD,N,Replace,Model)) # Estimate of population mean and variance
  # for each ranking method
  # print("From ListF" )
  #  print(EST)
  # print(EST)
  prec.weight=(1/EST[,2])/sum(1/EST[,2])   # precison weight-- inverse of the variance
  Cest=sum(prec.weight*EST[,1])   # point estimate for the population mean
  # JackNrep=matrix(0,nrow=n,ncol=2)
  #  CoefD0n.1=CoefD0F(Setsize,(n-1))
  retest=c(mean(EST[,1]),Cest,W.est)
  return(retest)
}


JACKVF=function(u){
  # n=length(u)-1
  #jackrep=n*u[1]-(n-1)*u[-1]
  #  ret=var(jackrep)
  ret= (n-1)*var(u)*((n-1)/n)^2
}




JPSLF=function(Data,Setsize,Replace,N,Model){
  nK=dim(Data)
  n=nK[1]
  K=nK[2]-1
  Coefn=CoefF(Setsize,n)
  #######################################
  # Compute coefficient to go into JPSED0G
  if(Replace ==1){
    if(is.null(N) ){print("Population size N must be provided for without replacement sampling")} 
    coef1D2=Coefn[1]
    coef2D2=1/(H*(H-1))+Coefn[3]+Coefn[2]-(Coefn[2]+1/H^2)*H/(H-1)
    coef3D2=Coefn[2]-1/(N-1)*(1/H-(Coefn[1]+1/H^2))
    CoefD=c(coef1D2,coef2D2,coef3D2)} else CoefD=Coefn
  if(Model==1) CoefD=Coefn
  ############################################
  if(K==1) {
  #  print(K)
  JPSE.V=JPSED0F(Data[,2], Data[,1], Setsize,CoefD,N,Replace,Model) # single ranking method estiamte
#  print(JPSE.V)
   Estimator="JPS"
   Point.Est=JPSE.V[1]
   Variance.Point=JPSE.V[2]
   Lower.Limit=Point.Est-qt(1-alpha/2,n-1)*sqrt(Variance.Point)
   Upper.Limit=Point.Est+qt(1-alpha/2,n-1)*sqrt(Variance.Point)
   Summary.return=data.frame(Estimator,Point.Est,Variance.Point,Lower.Limit,Upper.Limit)
  return(Summary.return)
  }
  JPSE.E=ListF(Data, Setsize,Replace,N,Model,Coefn) # estimate based on combined ranking method
  ################
  # create list of deta frames by deleting each row
  # this is used for Jackknife variance estimate
  DataL=vector('list',n)
  for( i in (1:n)){
    DataL[[i]]=Data[-i,]
  }
  Coefn1=CoefF(Setsize,n-1)
  ###########################################
  delet1=lapply(DataL,ListF,Setsize,Replace,N,Model,Coefn1) # Compute n different combined estimate
  # by deleting one observation at a time 
  
  
  JPSE.V=JPSED0F(Data[,2], Data[,1], Setsize,CoefD,N,Replace,Model)# single ranking method estiamte
  #print(JPSE.V)
  
  delet1M=do.call(rbind,delet1) #Convert list to matrix  
  delet1M=rbind(JPSE.E,delet1M)
  fc=1
  if(Replace==1) fc=(1-n/N)
  JackV=fc*apply(delet1M,2,JACKVF)  # Jackknife variance estimate of the combined estimator
  Variance.est=c(JackV,JPSE.V[2]) # bind the variance of JPS estimator based on single ranking method
  Estimate.est=c(JPSE.E,JPSE.V[1]) #bind the  JPS estimator based on single ranking method
  min.ind=which(Variance.est==min(Variance.est)) # Find the estiamtor having minimum variance
  # among four estimator    
  Variance.est=c(Variance.est,var(Data[,1])/n,Variance.est[min.ind]) # bind the variance of minimum 
  # variance estimator
  St.error=sqrt(Variance.est)
  Estimate.est=c(Estimate.est,mean(Data[,1]),Estimate.est[min.ind]) # bind the minimum variance estiamtor
  Lower.Limit=Estimate.est-qt(1-alpha/2,n-1)*sqrt(Variance.est)
  Upper.Limit=Estimate.est+qt(1-alpha/2,n-1)*sqrt(Variance.est)
  Lower.Limit=round(Lower.Limit,digits=3)
  Upper.Limit=round(Upper.Limit, digits=3)
 # test=(LL-popmean)*(UL-popmean)
  #coef=1*(test <0)
  CI=paste(Lower.Limit,Upper.Limit,sep=',')
  Estimator=c("UnWeighted","Sd.Weighted","Aggregate Weight","JPS Estimate","SRS estimate","Minimum")
 # Summary.ret=data.frame(Estimator,Estimate.est,Variance.est,Lower.Limit,Upper.Limit)
  Summary.ret=data.frame(Estimator,round(Estimate.est,digits=3),round(St.error,digits=3),CI)
  colnames(Summary.ret)=c("Estimator","Estimate","Standard Error", paste((1-alpha)*100,"% Confidence intervals",sep=''))
  #  Summary.ret=round(Summary.ret,digits=3)
   return(Summary.ret)
  
  #eturn(list(Estimate.est,Variance.est,LL,UL))
}





N=150
Setsize=3
H=Setsize
Replace=0 #( if Replace=0, Design is D_0, if Replace =1, Desing= D_2  )
Model=0  #( if Model=0, Design based inference, if Model=1, super population model is used)
sig=4
K=6
n=100
sim=50000
alpha=0.05
rhoM=matrix( c(rep(0.8,K),
               rep(0.8,K/2),rep(0.3,K/2),
               rep(0.5,K),
               rep(0.8,K/3),rep(0.5,K/3),rep(0.2,K/3),
               1,rep(0.8,(K-1))),nrow=5,ncol=K,byrow=TRUE)
rhoV=rhoM[2,]
#rhoV=0.9
tauV=sig*sqrt(1/rhoV^2-1)
#tau=rep(0.5,K)
pop=qnorm((1:N)/(N+1),50,3)
popmean=mean(pop)
storE=matrix(0, ncol=6,nrow=sim)
storV=matrix(0,ncol=6,nrow=sim)
EstimatorID=c("UnWeighted","Sd.Weighted","Aggregate Weight","JPS Estimate","SRS estimate","Minimum")
colnames(storE)=EstimatorID
colnames(storV)=EstimatorID
#( iter in (1:sim)){
Data0=JPSD0F(pop,n,Setsize,tauV,N,K) # this function genereta JPS data from design D_0
Data0=JPSD2F(pop,n,Setsize,tauV,N,K) # this function genereta JPS data from design D_2

ESTREP0=JPSLF(Data0,Setsize,Replace,N,Model=0) # this function computes the estimators
#storE[iter,]=ESTREP0[,2]
#storV[iter,]=ESTREP0[,3]
#}
#print(apply(storE,2,var))
#print(apply(storV,2,mean))

print(ESTREP0)















