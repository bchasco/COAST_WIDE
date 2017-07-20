#Energy of a Chinook salmon by stock and age
EC_hromta = array(0,dim=c(nAreas,nRun,nOrigin,2,nTime,nAges))


for(hi in 1:nAreas)
  for(ri in 1:nRun)
    for(oi in 1:nOrigin)
      for(mi in 1:2)
        for(ti in 1:nTime)
          for(ai in 1:nAges) #energy by length from sandie's paper, see Fig. 6.
            EC_hromta[hi,ri,oi,mi,ti,ai] = 0.000011 * CLA_hromta[hi,ri,oi,mi,ti,ai]^3.122            

#par(mfrow=c(8,5), mai=c(0.5,0.5,0,0))
#for(hi in 1:8)
#  for(ri in c(1:4,9))
#  {
#    plot(c(apply(EC_hromta[hi,ri,,1,,],c(2,3),mean)), pch = 16, col=ri, lty=1, las=1, type="l")
#    lines(c(apply(EC_hromta[hi,ri,,2,,],c(2,3),mean)), pch = 16, col=ri, lty=2, las=1, type="l")
#  }
