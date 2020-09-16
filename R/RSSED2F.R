
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

