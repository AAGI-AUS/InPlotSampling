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
