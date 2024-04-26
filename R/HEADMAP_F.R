#################################
#  construcnting headmap using ggplot
##################
GGplotF=function(POP,sample.M){#
  ############Sample.M ####################
  # First column: Halton numbers
  # Second Column: X_1 coordinates 
  # Third Column: X_2: Coordinates
  # Fourth column: Size variable
  # Fifth Columns: Weights
  # Sixth Column: Inclusion probabilities  
  ############################################
  
  ##############################################
  ##  POP: Population matrix ###############################3
  #  First column: Halton numbers for the entire population
  # Second Column: X_1 Coordinates for entire population   
  # Third Column: X_2 Coordinates for entire population  
  
  Size.variable=POP[,4]
  SBS_pps.Ind=rep(0,length(Size.variable))
  SBS1.ind=sample.M[,1][sample.M[,5]==0]
  PPS1.ind=sample.M[,1][sample.M[,5]!=0]
  SBS_pps.Ind[(SBS1.ind)]="SBS"
  SBS_pps.Ind[(PPS1.ind)]="PPS"
  
  Data.F=data.frame(Size=Size.variable,X1=POP[,2],X2=POP[,3],SBS_PPS=SBS_pps.Ind)
  AA<-ggplot(Data.F, aes(X2, X1,  fill=Size)) + 
    geom_tile()+
    scale_fill_distiller(palette = "RdPu")+  #  color the responses
    
    geom_point(filter(Data.F,SBS_PPS !=0),mapping=aes(shape=factor(SBS_PPS)),size=2)
  AA+geom_jitter()
  
  AA+  labs(x="X coordinate")+labs(y="Y coordinates")+
    labs(shape='Samples')+labs(fill="Size")
  return(AA)
}

