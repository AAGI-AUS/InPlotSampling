#############################################################
#  This function computes the empirical population by imputing 
#  the unobserved units using nearest neighbors
#############################################################
# Sampleids: Identification number of sample units in the population
# POPREMY  : Population data frame:
#            First Column: k, Halton numbers
#            Second column: X_1 coordinate of population units
#            Third column: X_2 coordinate of population units
#            Fourth column: Z size measurements of population units
# Y        :  Response measurements of sample units   
EmpirPop_partitionF=function(Sampleids,POPREMY,Y){
  
  ############## Impute the unobserved response (Y) #################
  ##############  and size variables from observed  #################
  ############    sample units #####################################
  QueryP.sam=POPREMY[-Sampleids,c(1,2,3)] # Halton number, X1, X2 coordinates of unobserved units
  InputS.sam=POPREMY[Sampleids,c(1,2,3)]# Halton number, X1, X2 coordinates of observed sample units
  Zsize.sam=POPREMY[Sampleids,4] # Values of the Size Variable of sample units
  NearestNeigh.sam=get.knnx(InputS.sam[,-1],QueryP.sam[,-1],4)# Find 4 nearest 
  # neighbor for QueryP (for each unobserved poipulation unit) 
  Nindex.sam<- NearestNeigh.sam$nn.index  # indexes of nearest neighbors
  Ndist.sam<-NearestNeigh.sam$nn.dist  # distances of nearest neighbors
  Nindex.dist.sam=cbind(Nindex.sam,Ndist.sam) # Combine indexes and distances in a matrix
  imputed_resp.sam=apply(Nindex.dist.sam,1,imputF,Y) # impute unobserved 
  #  population value from the observed  responses
  imputed_Size.sam=apply(Nindex.dist.sam,1,imputF,Zsize.sam) # impute unobserved size variable
  #  from  the size measurements of sample units
  ##############################################################################################
  
  
  ################ Response measurements of empirical population from ####################
  ################   imputed reponses                               ####################
  #print(head(PopM))
  observed.pop.values_resp.sam=cbind(POPREMY[Sampleids,1],Y) #Combine Halton numbers and 
  # Sample response values of Y
  colnames(observed.pop.values_resp.sam)= c("k","imputed.responses") # name the columns of matrix
  imputed.responses.sam=cbind(POPREMY[-Sampleids,1],imputed_resp.sam) # Combine the Halton
  # numbers and imputed reponse measurements of Y 
  colnames(imputed.responses.sam)=c("k","imputed.responses") # name the columns
  Emppop_resp.sam=rbind(observed.pop.values_resp.sam,imputed.responses.sam) 
  # Create empirical population containing two columns
  # Columns 1: Halton numbers
  # Columns 2: Imputed and observed response measurements
  roworder.sam=order(Emppop_resp.sam[,1]) # order the empirical population based on Halton numbers
  Emppop_resp.sam=Emppop_resp.sam[roworder.sam,] # empirical reponses are ordered based on Halton numbers
  #######################################################################
  
  
  
  ######### Size Variable measurements of Empirical population from imputed #######
  ######### size measurements    #############################################                         
  observed.pop.values_Size.sam=POPREMY[Sampleids,c(1,4)] # Observed size measurements
  # from the sample
  colnames(observed.pop.values_Size.sam)= c("k","imputed.Size") # Combine
  # the Halton numbers and observed size
  # measurements
  imputed.SizesM.sam=cbind(POPREMY[-Sampleids,1],imputed_Size.sam) #Combine
  #Halton  numbers with im,puted size measurements
  # in two columns
  colnames(imputed.SizesM.sam)=c("k","imputed.Size") # Name the two columns
  Emppop_Size.sam=rbind(observed.pop.values_Size.sam,imputed.SizesM.sam) 
  # Create empirical population of size measurements contain ing two columns
  # Column 1: Halton numbers
  # Column 2: imputed and observed size measurements
  roworder.sam=order(Emppop_Size.sam[,1]) # order the empirical population based on Halton numbers
  Emppop_Size.sam=Emppop_Size.sam[roworder.sam,] # Empirical size measurements are ordered based on Halton numbers
  ####################################################################
  
  #######################################################
  ############# Combine imputed response Y and imputed Size variable  ###########
  ############# in empirical  population containing three columns   ########
  ############# Column 1: Halton numbers                             #######
  ############# Column 2: Imputed response measurements              #######
  #############  Column 3: impujted Size measurments                 ########
  Empirical_pop=cbind(Emppop_resp.sam,Emppop_Size.sam[,2])
  return(Empirical_pop)
  
}






#########################################################
#  This function computes the mean of nearest neighbors
#  of  an  unobserved point in the3 population
#  YS: Sample values
#  u: a vector containing
#      indexes of the 4 nearest neighbors
#      distances of the four nearest neighbors 
imputF=function(u,YS){
  KK=length(u)/2
  indk=u[1:KK]
  distk=u[(KK+1):(2*KK)]
  eqdist=indk[distk==min(distk)] # find all neighbors with smallest distance
  imputY=mean(YS[eqdist]) # compute the average of all neighbors 
  #with smallest distance
  return(imputY)
}
######################################################################
