#Read in the data
stx = read.csv("selectivity.csv")

#Selectivity array
SEL_pjta = array(0,
               c(nPred,nAreas, nTime, nAges), 
               dimnames = list(predNames,areaNames, paste0("mon",1:nTime), paste0("oceanAge",0:(nAges-1))))

#sensitivity error
err = exp(rnorm(1,-0.5*MC_SELcv^2,MC_SELcv))
print(paste("sel", err))

#REad in the age selectivity of the predators
for(i in 1:nrow(stx))
{
  pi = stx[i,1] #pred species
  ji = stx[i,2] #area
  ti = stx[i,3] #time
  SEL_pjta[pi,ji,ti,] = t(stx[i,4:9])
  if(pi!=1 & runMCChain==TRUE & iSim>1){
    
    #mymu = min(max((SEL_pjta[pi,ji,ti,1]+1e-4),SEL_pjta[pi,ji,ti,1]),0.9999) 
    #myvar = (MC_SELcv[cvCnt]*mymu)^2
    #if(myvar<(mymu*(1-mymu))){
    #  SEL_pjta[pi,ji,ti,1] = rbeta(1,fa(mymu,myvar),fb(mymu,myvar))
    #  SEL_pjta[pi,ji,ti,2:nAges] =  (1-mymu) * SEL_pjta[pi,ji,ti,2:nAges]/sum(SEL_pjta[pi,ji,ti,2:nAges])   
    #  SEL_pjta[pi,ji,ti,1] = rbeta(1,fa(mymu,myvar),fb(mymu,myvar))
    #  SEL_pjta[pi,ji,ti,2:nAges] =  (1-mymu) * SEL_pjta[pi,ji,ti,2:nAges]/sum(SEL_pjta[pi,ji,ti,2:nAges])   
    #}
    SEL_pjta[pi,ji,ti,1] = max(min(SEL_pjta[pi,ji,ti,1] * err,1),0)
    SEL_pjta[pi,ji,ti,2:nAges] =  (1-SEL_pjta[pi,ji,ti,1]) * SEL_pjta[pi,ji,ti,2:nAges]/sum(SEL_pjta[pi,ji,ti,2:nAges])   
  }
}

#Just assume that the age selectivity of the other killer whales is the same as Salish Sea
for(ji in c(1,2,3,4,6,7,8))
{
  SEL_pjta[1,ji,,] = SEL_pjta[1,5,,]
}

SEL_pjta2 = SEL_pjta
#For the pinniped we need to make the age selectivity of the adult fraction proportional to the maturity of schedules
#Just assume that the age selectivity of the other killer whales is the same as Salish Sea
for(ji in c(1,2,3,4,5,6,7,8))
{
  for(pi in 2:4){
    for(ti in 1:12){
      esc = rep(0,nAges)
      nn = 1
      for(ai in 2:nAges){
        esc[ai] = colMeans(MAT_hra[ji,c(1:4,9),])[ai] * nn
        nn = nn - esc[ai]
      }
      SEL_pjta2[pi,ji,ti,2:nAges] = esc[2:nAges] * sum(SEL_pjta[pi,ji,ti,2:nAges])
    }
  }
}

SEL_pjta = SEL_pjta2

