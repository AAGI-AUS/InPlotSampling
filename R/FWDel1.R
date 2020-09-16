FWDel1=function(u,AWY){
    AWY1=AWY[-u,]
    eff.SamSiz=apply(AWY1[,-1],2,sum)
    Crosprod=AWY1[,1]%*%AWY1[,-1]
    W.est.del1= mean(Crosprod[eff.SamSiz>0]/eff.SamSiz[eff.SamSiz>0])
    return(W.est.del1)
}
