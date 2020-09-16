OneSample=function(Data,Setsize,Method="JPS",Confidence=0.95,Replace=TRUE,Model=0,N=NULL){
################################################################
###  Judgment-post stratifed sample  ###########################
## Check if the sample is Judgment-post stratified sample (JPS)
if(Method=="JPS"){if(!Replace && is.null(N)){
                   print("The population size N must be provided with without replacement sampling" );
                   return()          }
                   if(Model==1 && is.null(N)) {print("The population size N must be provided for superpopulation model");
                   return()   }

  alpha=1-Confidence
  Results=JPSEF(Data,Setsize, Replace, Model,N,alpha)
  if( Model==1) { COLname=   c("Predictor","Prediction","Pred. Error", paste((1-alpha)*100,"% Prediction intervals",sep=''))
                colnames(Results)=COLname}
 return(Results)
}
#################################################################
#
#################################################################
### Ranked set sample ###########################################

if(Method=="RSS"){if(!Replace && is.null(N)){
  print("The population size N must be provided with without replacement sampling" );
  return()}
  if(Model==1&& is.null(N)) {print("The population size N must be provided for superpopulation model");
    return() }
    RV=Data[,2]
    GSV=aggregate(RV,list(RV),length)$x
 #   if(length(unique(GSV))!=1 && !Replace) {print("First ranking method should be balanced for wthout replaclement sampling");
  #                          return()  }
    if(length(GSV)!=Setsize | min(GSV) <= 1 ){print("In Ranked set sampling design,first ranking method should not have less than two observations in any judgment ranking group");
                               return()  }
   # print("Here")

    alpha=1-Confidence
    RSS.Return=RSSEF(Data,Setsize,Replace,Model,N,alpha)
    if( Model==1) { COLname=c("Predictor","Prediction","Pred. Error", paste((1-alpha)*100,"% Prediction intervals",sep=''))
                   colnames(RSS.Return)=COLname}
    return(RSS.Return)

}
}
##################################################################
#




###################################################################################
###################################################################################
###################################################################################
#######   Functions below are required for Judgment Post-stratified  sampling
##################################################################################
###################################################################################
###################################################################################
###################################################################################




JPSEF=function(Data,Setsize, Replace, Model,N,alpha){
 # print(Model)
#  print(N)
  K=dim(Data)[2]-1
 # print(K)
 # print(Replace)
  if(is.null(N) ){print("Population size N must be provided for without replacement sampling"); return()}
  if(!Replace && is.null(N)) {print("Population size N must be provided for without replacement sampling"); return()}
  H=Setsize
  n=dim(Data)[1]
  Coefn=CoefF(H,n)
  if( K !=1) Coef.del1=CoefF(H,n-1) else  Coef.del1=NULL
  if(!Replace){
    coef1D2=Coefn[1]
    coef2D2=1/(H*(H-1))+Coefn[3]+Coefn[2]-(Coefn[2]+1/H^2)*H/(H-1)
    coef3D2=Coefn[2]-1/(N-1)*(1/H-(Coefn[1]+1/H^2))
    CoefD=c(coef1D2,coef2D2,coef3D2)
    #
    coef1D2.del=Coef.del1[1]
    coef2D2.del=1/(H*(H-1))+Coef.del1[3]+Coef.del1[2]-(Coef.del1[2]+1/H^2)*H/(H-1)
    coef3D2.del=Coef.del1[2]-1/(N-1)*(1/H-(Coef.del1[1]+1/H^2))
    Coef.Del=c(coef1D2.del,coef2D2.del,coef3D2.del)
  } else {CoefD=Coefn; Coef.Del=Coef.del1}

  if(Model==1) {CoefD=Coefn;Coef.Del=Coef.del1;
  }

  ###########################################3333333
  #  if there is only one ranker
  if(K==1){
    EST.EqSd=JPSEDF(Data[,-1], Y=Data[,1], H=Setsize,N=N,Coef=CoefD,CoefDel=Coef.Del,Replace=Replace,Model=Model,K)
    #print(EST.EqSd)
    Estimator=c("JPS","SRS")
    Estimate=round(c(EST.EqSd[1],mean(Data[,1])),digits=3)
    St.error=round(c(sqrt(EST.EqSd[2]),sd(Data[,1])/sqrt(n)),digits=3)
    Lower.Limit=round(Estimate-qt(1-alpha/2,n-1)*St.error, digits=3)
    Upper.Limit=round(Estimate+qt(1-alpha/2,n-1)*St.error,digits=3)
    CI=paste(Lower.Limit,Upper.Limit,sep=',')
    Summary.return=data.frame(Estimator,Estimate,St.error,CI)
    colnames(Summary.return)=c("Estimator","Estimate","Standard Error", paste((1-alpha)*100,"% Confidence intervals",sep=''))
    return(Summary.return)}


  EST.EqSd=apply(Data[,-1], 2, JPSEDF,Y=Data[,1], H=Setsize,N=N,Coef=CoefD,CoefDel=Coef.Del,Replace=Replace,Model=Model,K)

  JPSE.Fulln=EST.EqSd[1,] #  JPS mean estimate for each ranker using all n data
  JPSV.Fulln=EST.EqSd[2,] # Variance estiamte of JPS mean  for each ranker using all n data
  V.Sd=EST.EqSd[c(3:(n+2)),] #  Variance of JPS mean estiamte for each ranker when the i-th observation is deleted
  # the i-th row corresponds to variance estimate when the i-th observation
  # is deleted
  EST.Equal=EST.EqSd[-c(1:(n+2)),] #  JPS mean estiamte for each ranker when the i-th observation is deleted
  # the i-th row corresponds to JPS estimate when the i-th observation
  # is deleted
  if(Replace) fc=1 else fc=1-n/N  # finite population correction factor
  JPSE.Kn=sum((1/JPSV.Fulln)*JPSE.Fulln)/sum(1/JPSV.Fulln) #  varince weighted JPS estimate

  Prec.Weight=t(apply(V.Sd,1,function(u){(1/u)/sum(1/u)})) # variance  weights
  JACk.Repl.Sd=diag(Prec.Weight%*%t(EST.Equal))  # Jackknife feplicate of variance weighted  estimator
  Jack.VEST.Sd=fc*(n-1)*var(JACk.Repl.Sd)*((n-1)/n)^2 # Jackknife varince estiamte of variabce weighted JPS estimator
  JPS1= JPSE.Fulln[1] # JPS estimator based on best ranker
  JPS1.VEST=JPSV.Fulln[1] #Variance estimate of JPS estimator of best ranker
  EqWeight.Est=mean(JPSE.Fulln)  # Equal weight estimator
  Jack.Repl.EqWeight=apply(EST.Equal,1,mean) # Jackknife replicates for equalk weight JPS estimator
  Jack.EST.EqWeight=fc*(n-1)*var(Jack.Repl.EqWeight)*((n-1)/n)^2 #Jackknife variance estiamte  for equal weight JPS estimator



  ####################################33
  # agreement weight estimator
  AW=Data[,-1] #Ranks
  AW=t(apply(data.frame(Data[,-1]),1,WEIGHTF,H=Setsize)) # agreemeent weights
  eff.SS=apply(AW,2,sum)
  Crosprod=Data[,1]%*%AW
  EST.Agree.Weight=mean(Crosprod[eff.SS>0]/eff.SS[eff.SS>0]) # JPS agreement weight estiamtor
  AWY=cbind(Data[,1],AW)
  Jack.Repl.AWi=apply(matrix(1:n,ncol=1),1,FWDel1,AWY=AWY) #Aggrement weight estimator
  #when the i-th obseervation is deleted
  Jack.Est.AW=fc*(n-1)*var(Jack.Repl.AWi)*((n-1)/n)^2 #Jackknife variance estiamte  for aggreement weight JPS estimator
  ##############################################################
  #print(cbind(Jack.Repl.EqWeight,JACk.Repl.Sd,Jack.Repl.AWi))


  Estimate.est=c(EqWeight.Est,JPSE.Kn,EST.Agree.Weight,JPS1,mean(Data[,1]))
  Variance.est=c(Jack.EST.EqWeight,Jack.VEST.Sd,Jack.Est.AW,JPS1.VEST,var(Data[,1])/n)

  min.ind=which(Variance.est==min(Variance.est)) # Find the estiamtor having minimum variance
  # among four estimator
  Variance.est=c(Variance.est,Variance.est[min.ind]) # bind the variance of minimum
  # variance estimator
  St.error=sqrt(Variance.est)
  Estimate.est=c(Estimate.est,Estimate.est[min.ind]) # bind the minimum variance estiamtor
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
  #print(Summary.ret)
  return(Summary.ret)
}



###########################################################
# This function Computes JPS estimator and its variance for each ranker  ##
###########################################################
# Y:  Response measurements
# RV: Ranks for Y
# H: Set Size
# N: Finite population size
# Coef: Coefficients used in vaiance comupation when sample size is n
# CoefDel: Coefficients used in vaiance comupation
#          when the i-th unit is deleted
# Replace: Replace =1 if the sampling is with replacement.
# Model: Model =1 if  super ppopulation modle is used
JPSEDF=function(RV, Y, H,N,Coef,CoefDel,Replace,Model,K){
  n=length(Y)
  M.est=mean(aggregate(Y,data.frame(RV),mean)$x)  # JPS estimate
  Y.ij=expand.grid(Y,Y)

  GSV=rep(0,H) #Group sample size vector
  TemSS=aggregate(RV,data.frame(RV),length)
  GSV[TemSS[,1]]=TemSS$x  # Judgment group sample sizes. Some would be zero.
  dn=length(GSV[GSV>0]) # the nonempty judgment groups
  dn.star=length(GSV[GSV>1]) # the number of judgment groups having at least two observations
  R.hhp=expand.grid(RV,RV)
  Y.ij2=(Y.ij[,1]-Y.ij[,2])^2 #  squared differences of (Yi-Yj)^2
  G.sum= aggregate(Y.ij2,R.hhp,sum) # group  squared differences based on judgment classes
  GS.size=cbind(GSV[G.sum[,1]], GSV[G.sum[,2]]) # attach  judmgent group sample sizes
  # to each group
  denT2=GS.size[G.sum[,1]==G.sum[,2],1] # determine nh from the judgment group h
  denT1=GS.size[G.sum[,1]!=G.sum[,2],]  # determine sample sizes  n_h and n_h'for judgment groups h and h'
  den2=denT2*(denT2-1) # determien the denumerator nh(nh-1) for T2
  numT2=G.sum[G.sum[,1]==G.sum[,2],3] # sum_h sum_i sum_j ( Y_i=Y_j)^2I(R_i=h)I(R_j=h)
  TT2=sum(numT2[den2>0]/den2[den2>0]) #sum_h sum_i sum_j ( Y_i=Y_j)^2I(R_i=h)I(R_j=h')/(nh(nh-1))
  # Line below #sum_h sum_h'sum_i sum_j ( Y_i=Y_j)^2I(R_i=h)I(R_j=h')/(nh nh'))
  if(dim(denT1)[1]!=0) TT1=sum(G.sum[G.sum[,1]!=G.sum[,2],3]/(denT1[,1]*(denT1[,2]))) else TT1=0
  ############################################################################
  # Variance estimate with full data
  M.Est=mean(aggregate(Y,list(RV),mean)$x)  # JPS estiamte with full data
  T2s=H*TT2/(2*dn.star)
  T1s=TT1/(2*Coef[1]*dn^2)
  # VestD0=Coef[2]*T1s/(H-1)+Coef[3]*T2s
  if(!Replace)  {VEST=Coef[2]*T2s+Coef[3]*(N-1)*(T1s+T2s)/(N*(H-1));
  if(VEST <= 0) VEST=Coef[2]*T2s/2} else VEST= Coef[2]*T1s/(H-1)+Coef[3]*T2s
  if(Model==1) {VEST=(T1s+T2s)/H^2*((-1/N)+Coef[2]*H^2/(H-1))+T2s*((Coef[3]+Coef[2])-Coef[2]*H/(H-1))
  if(VEST <= 0 ) VEST=T2s*((Coef[3]+Coef[2])-Coef[2]*H/(H-1))
  }
  if(K==1) { ret=c(M.Est,VEST)
  return(ret)
  }


  ################################################################


  ################################################################
  ######### This part is new for Jackknife replication, delete one observations
  ########  reduces the computation time
  ID=1:n  # index to determine which observation is to be deleted
  Ind.ij=expand.grid(ID,ID)  # Index of observations to be deleted
  Y.ij2N=Y.ij2[Ind.ij[,1]-Ind.ij[,2]!=0]  # Remove Y_i=Y_ii
  R.hhpN=R.hhp[Ind.ij[,1]-Ind.ij[,2]!=0,] #  Remove R_i=R_i
  Ind.ij=Ind.ij[Ind.ij[,1]!=Ind.ij[,2],] # Remove i=i
  #  deltM=matrix(0,ncol=6,nrow=n) # This stores the contribtuion of each
  # deleted observation to
  # T1 and T2

  INDM=matrix(1:n,ncol=1) # This is used in apply function  below
  PASS=list(Y,RV,Ind.ij,GSV,Y.ij2N,R.hhpN,G.sum,TT2,TT1) # compile additional variables in list
  DeltM=t(apply(INDM,1,DELETi,PASS=PASS)) # This computes TT2, TT1, dn, dn-star
  # for each  deleted unit "i".
  # deltM=DeltM
  ##################################################
  T2v.Del=H*DeltM[,4]/(2*DeltM[,3]) # T2 when we delete the i-th unit
  # deltM[,4]: TT2i, deltM[,3]= dn_str, the number
  # of groups having at least two observations after deleting
  # the it-th unit
  T1v.Del=DeltM[,5]/(2*CoefDel[1]*(DeltM[,2])^2)# T1 when we delete the i-th unit
  # deltM[,5]: TT1i, deltM[,2]= dn_str, the number
  # of groups having at least one observations after deleting
  # the it-th unit
  #T2s=H*sum(Y2hhT2*GSV1^2/(GSV1*(GSV1-1)))/(2*dn.star)
  # Y2hhT1=group.mean[group.mean[,1]- group.mean[,2]!=0,]$x
  # T1s=sum(Y2hhT1)/(2*Coef[1]*dn^2)
  #  VestD0.del=CoefDel[2]*T1v.Del/(H-1)+CoefDel[3]*T2v.Del
  if(!Replace)  {
    VEST.Del=CoefDel[2]*T2v.Del+CoefDel[3]*(N-1)*(T1v.Del+T2v.Del)/(N*(H-1));
    # if(VEST.Del <= 0) VEST.Del=CoefDel[2]*T2v.del/2
    VEST.Del[VEST.Del <=0]= CoefDel[2]*T2v.Del[VEST.Del <=0]/2
  }  else {
    VEST.Del= CoefDel[2]*T1v.Del/(H-1)+CoefDel[3]*T2v.Del
  }
  if(Model==1) {
    VEST.Del=(T1v.Del+T2v.Del)/H^2*((-1/N)+CoefDel[2]*H^2/(H-1))+T2v.Del*((CoefDel[3]+CoefDel[2])-CoefDel[2]*H/(H-1))
    #    if(VEST.Del <= 0 ) VEST.Del=T2v.del*((CoefDel[3]+CoefDel[2])+CoefDel[2]*H/(H-1))
    VEST.Del[VEST.Del <=0]=T2v.Del[VEST.Del <=0]*((CoefDel[3]+CoefDel[2])-CoefDel[2]*H/(H-1))
  }

  Est=c(M.Est,VEST) # M.Est: JPS estimate with sample size n
  # VEST:  varaince estimate of JPS estimator with sample size n
  Est.Del=VEST.Del # n-dimentional vector containing
  #varince estimate of JPS estimator for each deleted observation
  ret=c(Est,Est.Del,DeltM[,6])
  # DeltM[,6] n-dimentional vector containing JPS estimate for
  # each deleted observation
  return(ret)
}




DELETi=function(i,PASS){
  #  PASS=list(Y,RV,Ind.ij,GSV,Y.ij2N,R.hhpN,G.sum,TT2,TT1)
  Y=PASS[[1]]
  RV=PASS[[2]]
  Ind.ij=PASS[[3]]
  GSV=PASS[[4]]
  Y.ij2N=PASS[[5]]
  R.hhpN=PASS[[6]]
  G.sum=PASS[[7]]
  TT2=PASS[[8]]
  TT1=PASS[[9]]
  n=length(Y)
  #print(c(TT2,TT1))
  #print(deltM)
  ################################################################
  ######### This part is new for Jackknife replication, delete one observations
  ########  reduces the computation time
  #Ind.ij=expand.grid(ID,ID)  # Index of observations to be deleted
  #Y.ij2N=Y.ij2[Ind.ij[,1]-Ind.ij[,2]!=0]  # Remove Y_i=Y_ii
  #R.hhpN=R.hhp[Ind.ij[,1]-Ind.ij[,2]!=0,] #  Remove R_i=R_i
  #Ind.ij=Ind.ij[Ind.ij[,1]!=Ind.ij[,2],] # Remove i=i
  Ri=RV[i]             # Rank of the removed observation
  nh=GSV[Ri]  # Judgmnt group sample size of removed observaion
  RGS= GSV
  RGS[Ri]=RGS[Ri]-1  # Judgmnet group sample size vector after i-th unit is removed
  JPSi=mean(aggregate(Y[-i],list(RV[-i]),mean)$x) # JPS estimate after emoving the i-th unit
  dni=length(RGS[RGS>0])  # The number of non-empy judgment groups after removing the i-th unit
  dni.star=length(RGS[RGS>1]) # The number of judgment classes having atlest two observations
  # after we emoved the i-th unit
  icont1=Y.ij2N[Ind.ij[,2]==i |Ind.ij[,1]==i] # All squared differences (Y_r-Y_j)^2 that
  # contains Y_i in it
  rankVi.1=R.hhpN[Ind.ij[,2]==i |Ind.ij[,1]==i,]  # Ranks of R_r and R_j where  either r=i or j=i
  # or both
  T2iV=icont1[rankVi.1[,1]==rankVi.1[,2]] # Extract squared differences where R_i=R_i
  if(nh==1) TT2i=TT2 else { ContT2Ful=sum(T2iV)
  # If nh=1 no contribution from the i-th unit since it is deleted
  TOT2=G.sum[as.logical((G.sum[,1]==Ri)*(G.sum[,2]==Ri)),] # Find the contribution of
  # all squared differences in ranking group R_i
  }
  if(nh>2) TT2i= TT2-TOT2[,3]/(nh*(nh-1))+(TOT2[,3]-ContT2Ful)/((nh-1)*(nh-2))
  if( nh==2)TT2i= TT2-TOT2[,3]/(nh*(nh-1))

  #####################
  #  T1 contribtuion
  if(nh==n) TT1i=0 else {
    T1iV=icont1[rankVi.1[,1]!=rankVi.1[,2]] # Extract All (Y_i-Y_j)^2 that contains Y_i from h and h'
    GRind=rankVi.1[rankVi.1[,1]!=rankVi.1[,2],] # Exctract the ranks R_i and R_j of Y_i and Y_j
    GRind=GRind[,1]*GRind[,2]  # Multiply the ranks of R_i and R_j
    T1iV=aggregate(T1iV,list(GRind), sum) #Add all squared differences in the same ranking groups
    ContT1Ful=sum(T1iV/(nh*GSV[T1iV[,1]/Ri])) # The contribution of the i-th unit to T1
    LOGIC=as.logical((G.sum[,1]==Ri)*(G.sum[,2]!=Ri) |as.logical((G.sum[,1]!=Ri)*(G.sum[,2]==Ri)))
    TOT1=G.sum[LOGIC,] # Extract contribution of the i-th unit in ranking groups R_i=h and R_j=h'
    TGRind=TOT1[,1]*TOT1[,2]
    TOT1=aggregate(TOT1[,3],list(TGRind), sum) # sum over all R_j=h'
    sumT1=sum(TOT1[,2]/(nh*GSV[TOT1[,1]/Ri])) # Find he contribtuion all squared differences
    #containing Y_i  over all ranking grpoups
    if( nh>1) TT1i=TT1-sumT1+sum(((TOT1[,2])-(T1iV[,2]))/((nh-1)*GSV[TOT1[,1]/Ri])) else TT1i=TT1-sumT1
  }
  return(c(i,dni,dni.star,TT2i,TT1i,JPSi))
  # i: id of the deleted observation
  # dni: d_n after the i-th unit is deleted
  # dni.star: d_n^star after the i-th unit is deleted
  # TT2i: T_2 after the i-th unit is deleted
  # TT1i: T_1 after the i-th unit is deleted
  # JPSi: JPS estiamte after the i-th unit is delelted
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

FWDel1=function(u,AWY){
  AWY1=AWY[-u,]
  eff.SamSiz=apply(AWY1[,-1],2,sum)
  Crosprod=AWY1[,1]%*%AWY1[,-1]
  W.est.del1= mean(Crosprod[eff.SamSiz>0]/eff.SamSiz[eff.SamSiz>0])
  return(W.est.del1)
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

###################################################################################
###################################################################################
###################################################################################
#######   Functions below are requred for Ranked set sampling
##################################################################################
###################################################################################
###################################################################################
###################################################################################




###########################################################
# This function Computes RSS estimator and its variance without replacement sampling  ##
###########################################################
#RV: ranks
#Y: response
# H: Set size
# N: population size
RSSED2F=function(RV, Y, H,N){
  n=length(Y)  # Sample size
# d=n/H
  RVD=data.frame(RV)
  ind=1:n
  ij=expand.grid(ind,ind) # creates i,j index for pairevwise differences
  M.est=mean(aggregate(Y,RVD,mean)$x)  # JPS estimate
  YIYJ=expand.grid(Y,Y) #  create Y_i, Y_j pairs
  YIYJ=YIYJ[ij[,1]!=ij[,2],] # remove  pairs Y_i=Y_j
  GSample.Size=aggregate(RV,data.frame(RV),length)$x  # ranking group sample sizes
#  dn=length(GSample.Size)
  #print(dn)
 # GSample.Size1=GSample.Size[GSample.Size>1]
#  dn.star=length(GSample.Size1)
  RhRhp=expand.grid(RV,RV) # create the pairs of ranks h, h' that corresponds to Y_{[h]i} and Y_{[h']j}
  RhRhp=RhRhp[ij[,1]!=ij[,2],] # remove pairs that Y_{[h]i}=Y_{[h]i}
  YIYJ2=(YIYJ[,1]-YIYJ[,2])^2  # Computes sum_i Sum_j (Y_{[h]i}- Y_{[h']j})^2
  group.mean= aggregate(YIYJ2,RhRhp,mean) # Computes sum_i Sum_j (Y_{[h]i}- Y_{[h']j})^2/nh nh' if h !=h'
                                          # Computes sum_i Sum_j (Y_{[h]i}- Y_{[h']j})^2/nh(nh-1) if h=h'
  aggreq.Ssiz= cbind(GSample.Size[group.mean[,1]],GSample.Size[group.mean[,2]]) # create  group sample size vectors
                                                            # that corresponds to h and h' in sum_i Sum_j (Y_{[h]i}- Y_{[h']j})^2
  Y2hhT2=group.mean[group.mean[,1]- group.mean[,2]==0,]$x # Computes sum_i Sum_j (Y_{[h]i}- Y_{[h]j})^2/nh(nh-1), h=1,,,H
  nhnh=aggreq.Ssiz[group.mean[,1]==group.mean[,2],][,1]   #### compute (n_1,n_2, ... n_h)
  #nhnh= nh.nhV[,1]
  T2ss=sum(Y2hhT2/(nhnh))/(2*H^2)
  T2s=sum(Y2hhT2)/(2*H^2) # \sum_h \sum_i \sum_j( Y_{[h]i]}- Y_{[h]j})^2/(nh(nh-1))
  Y2hhT1=group.mean[group.mean[,1]- group.mean[,2]!=0,]$x #\sum_h\sum_h'' \sum_i \sum_j( Y_{[h]i]}- Y_{[h'']j})^2/(nh(nh))
  T1s=sum(Y2hhT1)/(2*H^2)
  VEST=(T2ss-(T2s+T1s)/N)
  if(VEST <0 ) VEST=(T2ss)/2
  return(c(M.est,VEST))
}


###########################################33333
#  This function provides estimator for RSS data
#  RSSK: n by (K+1) dimensional data matrix, the first column is Y-values,
#  the next K coulumns are the ranks of K-ranking methods
#  H: set Size
# N: population size
# Model: if Modle=0 design based inference, if Model=1, superpopulation model
# Replace: if Replace=TRUE with replacmenet selection is used.
# If Replace=FALSE without replacement selection is used
# B: Bootstrap replication
RSSEF=function(Data,H,Replace,Model,N,alpha){
  RM=Data[,-1]
  RV=Data[,2]
  Y=Data[,1]
  n=length(Y)
  K=dim(Data)[2]-1
  #########################################################################
  ########## Single ranker RSS estimator without replacement design
  if(!Replace){
    RSS.oneE=mean(aggregate(Y, list(RV),FUN=mean)$x) # Single ranker RSS estimate
    EST=RSSED2F(RV, Y, H,N)
    ### Confidence interval balanced RSS
    LC=EST[1]-qt(1-alpha/2,n-1)*sqrt(EST[2])
    UC=EST[1]+qt(1-alpha/2,n-1)*sqrt(EST[2])
  }
  #############################################################################
  ###############################################################################

  ####################################################################################
  ########### Single ranker RSS estimator with replacement design
  if(Replace){RSS.oneE=mean(aggregate(Y, list(RV),FUN=mean)$x) # Single ranker RSS estimate
  RSS.oneCount=aggregate(RV, list(RV),FUN=length)$x
  RSS.oneVar=aggregate(Y, list(RV),FUN=var)$x
  Hd=length(RSS.oneVar[RSS.oneCount>1])
  RSS.oneVE=sum(RSS.oneVar[RSS.oneCount>1]/RSS.oneCount[RSS.oneCount>1])/Hd^2
  EST=c(RSS.oneE,RSS.oneVE)
  LC=EST[1]-qt(1-alpha/2,n-1)*sqrt(EST[2])
  UC=EST[1]+qt(1-alpha/2,n-1)*sqrt(EST[2])
  }
  ######################################################################################
  ########################################################################################

  ##########################################################################################
  ######## Single ranker RSS estimator with under super population model ##################
  if (Model==1){ RSS.oneE=mean(aggregate(Y, list(RV),FUN=mean)$x) # Single ranker RSS estimate
  EST=RSSED2F(RV, Y, H,N)
  LC=EST[1]-qt(1-alpha/2,n-1)*sqrt(EST[2])
  UC=EST[1]+qt(1-alpha/2,n-1)*sqrt(EST[2])
  }

  Lower.limit=round(c(LC),digits=3)
  Upper.limit=round(c(UC),digits=3)
  ST.error=sqrt(EST[2])
  Point.est=EST[1]
  if(K==1){
    Confinterval=paste(Lower.limit,Upper.limit, sep=',')
    Estimator=c("RSS-1")
    summary.table=data.frame(Estimator,round(Point.est,digits=3),round(ST.error,digits=3),Confinterval)
    colnames(summary.table)=c("Estimator","point.est", "St.error",paste((1-alpha)*100,"% Confidence Interval",sep=''))
    return(summary.table)
  }


  ####################################33
  # agreement weight estimator
  AW=Data[,-1] #Ranks
  AW=t(apply(data.frame(Data[,-1]),1,WEIGHTF,H)) # agreemeent weights
  eff.SS=apply(AW,2,sum)
  Crosprod=Data[,1]%*%AW
  W.est=mean(Crosprod[eff.SS>0]/eff.SS[eff.SS>0]) # RSS agreement weight estiamtor
  AWY=cbind(Data[,1],AW)
  Jack.Repl.AWi=apply(matrix(1:n,ncol=1),1,FWDel1,AWY=AWY) #Aggrement weight estimator
  #when the i-th obseervation is deleted
  if(Replace) fc=1 else fc=1-n/(N-1)
  J.var=fc*(n-1)*var(Jack.Repl.AWi)*((n-1)/n)^2 #Jackknife variance estiamte  for aggreement weight JPS estimator
  ##############################################################



  # Jackknife confidence interval based on weighted estimator
  LWC=W.est-qt(1-alpha/2,n-1)*sqrt(J.var)
  UWC=W.est+qt(1-alpha/2,n-1)*sqrt(J.var)


  ############################################################################################
  #############################################################################################
  Lower.limit=round(c(LC,LWC),digits=3)
  Upper.limit=round(c(UC,UWC),digits=3)
  ST.error=c(sqrt(EST[2]),sqrt(J.var))
  Point.est=c(EST[1],W.est)
  Confinterval=paste(Lower.limit,Upper.limit, sep=',')
  Estimator=c("RSS-1", " Aggregate Weighted")
  summary.table=data.frame(Estimator,round(Point.est,digits=3),round(ST.error,digits=3),Confinterval)
  colnames(summary.table)=c("Estimator","point.est", "St.error",paste((1-alpha)*100,"% Confidence Interval",sep=''))
  return(summary.table)
}















####################################################################################
####################################################################################
#####################################################################################
# Functions below are not part of the packege
# They are used to generate RSS and JPS samples to test the functions
####################################################################################
####################################################################################
#####################################################################################

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


##############################3
# This function genrates RSS sample with K ranking methods with replacemet
# Pop: has two variables popY, variable of interest
#  popAux: Auxiliary variable
# We assume pop and popAux are correlated
# n: sample size n=Hd, H: set size, d: cycle size
RSSDF=function(pop,n,H,K){
  d=n/H
  popY=pop[,1]
  popAux=pop[,2]
  N=length(popY)
  RSSM=matrix(0,ncol=(K+1),nrow=n)
  ic=1
  for (j in (1:d)){
    for(h in (1:H)){
      setid=sample(1:N,H)
      setY=popY[setid]
      setX=popAux[setid]
      orAux=order(setX)
      orY=setY[orAux]
      osetX=setX[orAux]
      setidO=setid[orAux]
      #oset=DELLF(set,tauV[1])
      RSSM[ic,c(1,2)]=c(orY[h],h)
      k1obs=osetX[h]
      redAux=popAux[-setidO[h]]
      if(K >1){
      for(k in (2:K)){
        kset=c(k1obs,sample(redAux, (H-1)))
        OKS= order(kset)
        koset=kset[OKS]
        kR=which(koset==k1obs)
        if(length(kR)>1) kR=sample(kR,1)
        RSSM[ic,(k+1)]=kR
      }
      }
      ic=ic+1
    }
  }
  return(RSSM)
}



#################################
# This function orders the units in comparison set
# in balanced ran ked set sample
# u[1]: is the  judgment rank to be measured
# u[2:(H+1)]: Y values
# u[H+2:(2H+1)]: auxilairy value
# u[2H+2:3H+1]: population index of the units in comparion set
ORD=function(u){
  h=u[1]
  u=u[-1]
  H=length(u)/3
  #print(H)
  H1=H+1
  H2=2*H
  H3=H2+1
  HH3=3*H
  ys=u[1:H]
  xs=u[H1:H2]
  indv=u[H3:HH3]
  Orderx=order(xs)
  Orxset=xs[Orderx]
  ordind=indv[Orderx]
  Yorder=ys[Orderx]
  return(c(Yorder[h],Orxset[h],ordind[h]))
}


# This function orders the units in comparison set
# for ranker k
# u[1]: is the  judgment rank to measured in balanced ranked-set sample
# u[2:(H+1)]: Xvalues
# u[H+2:(2H+1)]:population index of the units in comparion set
#
ORDk=function(u){
  hind=u[1]
  u=u[-1]
  H=length(u)/2
  # print(H)
  H1=H+1
  H2=2*H
  xs=u[1:H]
  indv=u[H1:H2]
  Orderx=order(xs)
  Orxset=xs[Orderx]
  ordind=indv[Orderx]
  Rh=which(Orxset==hind)
  if(length(Rh) >1) Rh=sample(Rh,1)
  return(Rh)
}



##############################3
# This function genrates RSS sample with K ranking methods without replacemet
# Pop: has two variables popY, variable of interest
#  popAux: Auxiliary variable
# We assume pop and popAux are correlated
# n: sample size n=Hd, H: set size, d: cycle size
RSSNRF=function(pop,n,H,K){
  d=n/H
  K1=K+1
  rseq=rep((1:H),times=d)
  popY=pop[,1]
  N=length(popY)
  popAux=pop[,2]
  popind=1:N
  RSSM=matrix(0,ncol=(K1),nrow=n)
  ic=1
  ind=sample(popind,n*H)
  setY=matrix(popY[ind],ncol=H,nrow=n)
  setX=matrix(popAux[ind],ncol=H,nrow=n)
  indexM=matrix(ind,ncol=H,nrow=n)
  setYX=cbind(rseq,setY,setX,indexM)
  ordYX=t(apply(setYX,1,ORD))
  RSSM[,c(1,2)]=c(ordYX[,1],rseq)
  # redpop=popAux[-ordYX[,3]]
  Yind=ordYX[,3]
  redpopind=popind[-Yind]
  Xh=ordYX[,2]
  #Nk=length(redpop)
  if(K1 >2){
  for(k in (3:K1)){
    indk=sample(redpopind,n*(H-1))
    setX=matrix(popAux[indk],ncol=(H-1),nrow=n)
    indexM=matrix(indk,ncol=(H-1),nrow=n)
    indexM=cbind(Yind,indexM)
    setX=cbind(Xh,setX)
    setYX=cbind(Xh,setX,indexM)
    RankK=apply(setYX,1,ORDk)
    RSSM[,k]=RankK
  }
  }

  return(RSSM)
}








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
if(Replace)  Data=JPSD0F(pop,n,Setsize,tauV,N,K) # This function generetas JPS data sampling with replacement
if(!Replace) Data=JPSD2F(pop,n,Setsize,tauV,N,K) # This function generetas JPS data sampling without replacement
}
##############################################################
#####  RSS
if(Method=="RSS"){
if(Replace)  Data=RSSDF(pop,n,H,K) # This function generetas RSS data sampling  with replacement
if(!Replace) Data= RSSNRF(pop,n,H,K) # This function generetas RSS data sampling without replacement
Data=Data[order(Data[,2]),]
}

  ONE=OneSample(Data,Setsize,Method,0.80, Replace,Model,N)
  if(Replace) print(paste(Method, "Sample with number of rankers K=", K, "Sampling with replacement")) else
    print(paste(Method, "Sample with number of rankers K=", K, "Sampling without replacement"))
  if(Model==0) print("Design based inference is developed") else print("Super-population  model is used")
  print(ONE)
