#Read in the data
stx = read.csv("selectivity.csv")

#Selectivity array
SEL_pjta = array(0,
               c(nPred,nAreas, nTime, nAges), 
               dimnames = list(predNames,areaNames, paste0("mon",1:nTime), paste0("oceanAge",0:(nAges-1))))

#REad in the age selectivity of the predators
fa = function(mu,var){
  return(mu*((mu*(1-mu))/var-1))}
fb = function(mu,var){
  return((1-mu)*((mu*(1-mu))/var-1))}

for(i in 1:nrow(stx))
{
  pi = stx[i,1] #pred species
  ji = stx[i,2] #area
  ti = stx[i,3] #time
  SEL_pjta[pi,ji,ti,] = t(stx[i,4:9])
  if(pi!=1 & runMCChain==TRUE & iSim>1){
    mymu = min(max((SEL_pjta[pi,ji,ti,1]+1e-4),SEL_pjta[pi,ji,ti,1]),0.9999) 
    myvar = (MC_SELcv*mymu)^2
    if(myvar<(mymu*(1-mymu))){
      SEL_pjta[pi,ji,ti,1] = rbeta(1,fa(mymu,myvar),fb(mymu,myvar))
      print(paste(pi,ji,ti,myvar,mymu,mymu*(1-mymu),SEL_pjta[pi,ji,ti,1],rbeta(1,fa(mymu,myvar),fb(mymu,myvar)),fa(mymu,myvar),fb(mymu,myvar)))
      SEL_pjta[pi,ji,ti,2:nAges] =  (1-mymu) * SEL_pjta[pi,ji,ti,2:nAges]/sum(SEL_pjta[pi,ji,ti,2:nAges])   
    }
  }
}

#Just assume that the age selectivity of the other killer whales is the same as Salish Sea
for(ji in c(1,2,3,4,6,7,8))
{
  SEL_pjta[1,ji,,] = SEL_pjta[1,5,,]
}

#Just assume that the age selectivity of the other killer whales is the same as Salish Sea
#for(ji in c(1,2,3,4,6,7,8))
#{
#  for(pi in 2:4){
#    for(ti in 1:12){
#      for(ai in 2:nAges){
#        if(ai==2)
#        SEL_pjta[pi,ji,ti,ai] = prod(1-colMeans(MAT_hra[ji,c(1:4,9),1:ai])) 
#      }
#    }
#  }
#}

#Now we have to assume that the selectivity of the adult age classes is 
#proportion to the salmon that are likely to be proportion to the adundance of the
#different age in the area where consumption occurs.
#Rather than building a dynamic model that where the selectivity of adult changes over time
#We simply asuume that the selectivity is a function of the adult Chinook who origin is in the
#area where the Chinook occur.
#Other wise you have to decide which predator eats first and then recalculate the selectivities
#at each time-step.  

