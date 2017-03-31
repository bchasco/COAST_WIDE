#Temporal and spatial distribution Chinook salmon stocks.
S_hjromyta = array(0,dim=c(nAreas,nAreas,nRun,nOrigin,2,nYear,nTime,nAges))

#Temp variable
S1_hjromyta = array(0,dim=c(nAreas,nAreas,nRun,nOrigin,2,nYear,nTime,nAges))

#Temporal and spatial agrregation of salmon within an area
FSTotal_jyta = array(0,dim=c(nAreas,nYear,nTime,nAges),
                    dimnames = c(areaNames,1:nYear,1:nTime,paste0("ocean",1:nAges)))
#Temporal and spatial stock composition fractions
FS_hjromyta = array(0,dim=c(nAreas,nAreas,nRun,nOrigin,2,nYear,nTime,nAges),
                    dimnames = c(areaNames,areaNames,runNames,originNames,c("migrating", "staying"), 1:nYear,1:nTime,paste0("ocean",1:nAges)))

#Escapement
ESC_hroyta = array(0,dim=c(nAreas,nRun,nOrigin,nYear,nTime,nAges),
                    dimnames = c(areaNames,runNames,originNames,1:nYear,1:nTime,paste0("ocean",1:nAges)))

#Number of Chinook consumed
NC_phjromyta = array(0,dim=c(nPred,nAreas,nAreas,nRun,nOrigin,2,nYear,nTime,nAges))

#Deficit in the number of Chinook consumed
DefC_phjromyta = array(0,dim=c(nPred,nAreas,nAreas,nRun,nOrigin,2,nYear,nTime,nAges))

#Survival rates from Appendix table 5-5 in FRAM documentation
stx = as.matrix(read.csv("FRAM_survivalRates.csv",header=FALSE))
sv_ta = array(stx, dim=c(nTime,nAges),dimnames=c(1:nTime,paste0("ocean",1:nAges)))


Mat = array(0,c(2,nAges))
Mat[1,] =  1- Mat[1,] #Conditional probability of staying in ocean.
Mat[2,] = c(0.0,0.05,0.5,0.8,1)  #Conditional probability of migrating back to stream

ptm = proc.time()

for(yi in 1:nYear)
{
  for(ti in 1:nTime)
  {
    #First calculate movement and natural mortality
    for(hi in 1:nAreas)
    {
      for(ri in 1:nRun)
      {
        for(oi in 1:nOrigin)
        {
          #print(paste(hi,hi,ri,oi,2,ti,1))
          #The recruitment
          S_hjromyta[hi,hi,ri,oi,2,yi,ti,1] = R_hroy[hi,ri,oi,yi] * SMT_hryt[hi,ri,yi,ti]
          if(ti>1)
          {
            S_hjromyta[hi,nAreas,ri,oi,2,yi,ti,1] = S_hjromyta[hi,nAreas,ri,oi,2,yi,ti-1,1] + S_hjromyta[hi,hi,ri,oi,2,yi,ti-1,1]
            S_hjromyta[hi,hi,ri,oi,2,yi,ti-1,1] = 0
          }
          
          if(yi>1)
          {
            for(mi in 1:2)
            {
              #All smolt are transient
              for(ai in 2:nAges)
              {
                #Redistribute the adults to the appropriate areas based on Ole Shelton's model
                if(ti==1){
                  #Redistribute just the ones that stayed m = 2
                  S_hjromyta[hi,,ri,oi,mi,yi,ti,ai] = sum(S_hjromyta[hi,,ri,oi,2,yi-1,nTime,ai-1]) * 1/nAreas * Mat[mi,ai]
                }
                if(ti>1)
                  S_hjromyta[hi,,ri,oi,mi,yi,ti,ai] = sum(S_hjromyta[hi,,ri,oi,mi,yi,ti-1,ai]) * 1/nAreas
                
                #Survival  
                S_hjromyta[hi,,ri,oi,mi,yi,ti,ai] = (1-sv_ta[ti,ai]) * S_hjromyta[hi,,ri,oi,mi,yi,ti,ai]
              }
            }
          }#end (yi>1)
        }#end oi  
      }#end ri
    }# end hi
    
    
    #Now calculate the total number of individuals of a certain age in each area.
    FSTotal_jyta[,yi,ti,] = apply(S_hjromyta[,,,,,yi,ti,],c(2,6),sum)
    
    for(ji in 1:nAreas)
      for(ai in 1:nAges)
      {
        #Estimate the stock-, run-, origin-, and migration-specific fractions in each area 
        #so that you can distribute the predation mortality proportionally
        FS_hjromyta[,ji,,,,yi,ti,ai] = S_hjromyta[,ji,,,,yi,ti,ai]/(FSTotal_jyta[ji,yi,ti,ai]+0.0000000001)
      }#end ai 
   
    #Estimate the number of Chinook consumed. 
    for(hi in 1:nAreas){
      for(ji in 1:nAreas){
        for(ai in 1:nAges){
          for(ri in 1:nRun){
            for(oi in 1:nOrigin){
              for(mi in 1:2){
                
                #Number of Chinook consumed
                NC_phjromyta[,hi,ji,ri,oi,mi,yi,ti,ai] = apply(ED_phjytias[,hi,ji,yi,ti,,ai,], c(1), sum) / 
                  (EC_hromta[hi,ri,oi,mi,ti,ai]+0.00000000001) * 
                  FS_hjromyta[hi,ji,ri,oi,mi,yi,ti,ai]
                
                #Keep track of the deficit in Chinook abundance
                if(S_hjromyta[hi,ji,ri,oi,mi,yi,ti,ai]<sum(NC_phjromyta[,hi,ji,ri,oi,mi,yi,ti,ai]))
                {
                  DefC_phjromyta[,hi,ji,ri,oi,mi,yi,ti,ai] = (S_hjromyta[hi,ji,ri,oi,mi,yi,ti,ai] - sum(NC_phjromyta[,hi,ji,ri,oi,mi,yi,ti,ai])) * 
                    NC_phjromyta[,hi,ji,ri,oi,mi,yi,ti,ai] / 
                    sum(NC_phjromyta[,hi,ji,ri,oi,mi,yi,ti,ai])
                }
                
                #Subtract the consumption
                S1_hjromyta[hi,ji,ri,oi,mi,yi,ti,ai] = S_hjromyta[hi,ji,ri,oi,mi,yi,ti,ai]
                S_hjromyta[hi,ji,ri,oi,mi,yi,ti,ai] = S_hjromyta[hi,ji,ri,oi,mi,yi,ti,ai] - min(S_hjromyta[hi,ji,ri,oi,mi,yi,ti,ai],sum(NC_phjromyta[,hi,ji,ri,oi,mi,yi,ti,ai]))

                
              }#end mi
              #Subtract the escapement after consumption has occurred
              if(hi==ji & ai>1)
              {
                ESC_hroyta[hi,ri,oi,yi,ti,ai] = S_hjromyta[hi,ji,ri,oi,2,yi,ti,ai] 
                S_hjromyta[hi,ji,ri,oi,2,yi,ti,ai] = 0
              }
            }#end oi
          }#end ri
        }#end ai
      }#end ji
    }#end hi
  }#end time step
}#end year step

print(proc.time() - ptm)