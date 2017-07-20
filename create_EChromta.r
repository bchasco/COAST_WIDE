#Energy of a Chinook salmon by stock and age
EC_hromta = array(0,dim=c(nAreas,nRun,nOrigin,2,nTime,nAges))

err = exp(rnorm(1,0,MC_CCONDcv))
print(paste("cond", err))
for(hi in 1:nAreas)
  for(ri in 1:nRun)
    for(oi in 1:nOrigin)
      for(mi in 1:2)
        for(ti in 1:nTime)
          for(ai in 1:nAges)
          {
            #Sandra oneill's paper, see figure 6
            EC_hromta[hi,ri,oi,mi,ti,ai] = (0.000011 * (CLA_hromta[hi,ri,oi,mi,ti,ai])^3.122)            
            if(runMCChain==TRUE & iSim>1)
              EC_hromta[hi,ri,oi,mi,ti,ai] = (0.000011 * err * (CLA_hromta[hi,ri,oi,mi,ti,ai])^3.122)            
          }

