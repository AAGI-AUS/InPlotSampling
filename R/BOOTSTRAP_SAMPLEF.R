

BootF=function(EMPIR_POP,S.size,D){
  #D:  Botstrap sample size
  XM=EMPIR_POP[,3]  # imputed size measurements
  N=length(XM) # populatiuon size
  HB=0:(N-1)   # Halton numbers
  Boot.stor=matrix(0,ncol=1,nrow=D)  # store the bootsrap estimates
  colnames(Boot.stor)=c("SBS1-PPS1")
  for (d in (1:D)){
    Bsample=SAMPLE_BootF(EMPIR_POP,S.size)  # Selects bootstrap samples
    ############################### PPS-SBS Estimator #########
    YPPS.SBS=Bsample[,2] # Bootstrap sample of Y
    PI.Sample=Bsample[,3] # inmclusiuon probabilite of slected Y
    PPS.SBS.BE=sum(YPPS.SBS/PI.Sample)/N # Bootstrap estimates
    Boot.stor[d,]=PPS.SBS.BE  # Store bootstrap estimates
    ######################################
  }
  Bootstrap.var.est=apply(Boot.stor,2,var) #  bootstrap variance estimates
  
  return(Bootstrap.var.est)
  
}






##################################################
##  This function generates Bootstrap  SBS-PPS sample
##################################################
SAMPLE_BootF=function(POP,SSVec){
  #  n=sum(SSVec)  # total sample size
  n1=SSVec[1] # sample size for pps sample
  n2=SSVec[2] # sample size for SBS sample
  XMR=POP[,3]  # Size variable used  for selection probabilities
  HB=POP[,1]  # Halton number is  Halton sequence
  B=length(XMR)  # length of the size variabl
  
  if( n1>0){
    ############################################
    #  Selects SBS1 bootstrap sample from Empirical population
    sbs1.init=sample(HB,1) 
    SBS1=(sbs1.init:(sbs1.init+n1-1))%%B +1 #  we add 1 to match Halton index
    # k=0,...,B-1 to  integers 1:B
    XMR1=XMR[-SBS1]
    HBRPPS1=HB[-SBS1]} else {SBS1=NULL;XMR1=XMR;HBRPPS1=HB}
  ###########################################################
  
  ################ Selects PPS1 bootstrap sample from Empirical population ####
  ##############################################################################
  if( n2>0){PPS1=lahiri.design(XMR1,n2) # select a PPS  bootstrap sample of size n2
  # From empirical  population
  PPS1.sample=HBRPPS1[PPS1]+1;PPS1.sample=unique(PPS1.sample)} else PPS1.sample=NULL
  #######################################################################
  
  ##################################################################
  SBS1_PPS1=c(SBS1,PPS1.sample)  # Combine SBS1 anbd PPS1
  #weights=c(rep(0,n1),XMR[PPS1.sample]/sum(XMR1))  # Weight of sampled units
  pps_sbs_Bootstrap_sample=data.frame(SBS1_PPS1,Y=POP[SBS1_PPS1,2],inc=POP[SBS1_PPS1,4])
  # return Bootstrap sample 
  # contains three columns
  # Column 1:  Sample population id
  # Column 2: sampled Y (response)
  # Column 3: Inclusion probability
  return(pps_sbs_Bootstrap_sample)
}

