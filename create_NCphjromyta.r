
#Temporal and spatial distribution Chinook salmon stocks.
S_hjromyta = array(0,dim=c(nAreas,nAreas,nRun,nOrigin,2,nYear,nTime,nAges))

#Temp variable
S1_hjromyta = array(0,dim=c(nAreas,nAreas,nRun,nOrigin,2,nYear,nTime,nAges))

#Temporal and spatial agrregation of salmon within an area
FSTotal_jyta = array(0,dim=c(nAreas,nYear,nTime,nAges),
                     dimnames = list(areaNames,1:nYear,1:nTime,paste0("ocean",1:nAges)))
#Temporal and spatial stock composition fractions
FS_hjromyta = array(0,dim=c(nAreas,nAreas,nRun,nOrigin,2,nYear,nTime,nAges),
                    dimnames = list(areaNames,areaNames,runNames,originNames,c( "staying","migrating"), 1:nYear,1:nTime,paste0("ocean",1:nAges)))

#Escapement
ESC_hjroyta = array(0,dim=c(nAreas,nAreas, nRun,nOrigin,nYear,nTime,nAges),
                    dimnames = list(areaNames,areaNames,runNames,originNames,1:nYear,1:nTime,paste0("ocean",1:nAges)))

#Number of Chinook consumed
NC_phjromyta = array(0,
                     dim=c(nPred,nAreas,nAreas,nRun,nOrigin,2,nYear,nTime,nAges),
                     dimnames = list(predNames,areaNames,areaNames,runNames,originNames,c("staying","migrating"), 1:nYear,1:nTime,paste0("ocean",1:nAges)))
#Biomass of Chinook consumed
BC_phjromyta = array(0,
                     dim=c(nPred,nAreas,nAreas,nRun,nOrigin,2,nYear,nTime,nAges),
                     dimnames = list(predNames,areaNames,areaNames,runNames,originNames,c( "staying","migrating"), 1:nYear,1:nTime,paste0("ocean",1:nAges)))

#Temporary array
tmpED_pjyta = array(0,
                     dim=c(nPred,nAreas,nYear,nTime,nAges),
                     dimnames = list(predNames,areaNames, 1:nYear,1:nTime,paste0("ocean",1:nAges)))

#Deficit in the number and energy of Chinook consumed
DefC_phjromyta = array(0,dim=c(nPred,nAreas,nAreas,nRun,nOrigin,2,nYear,nTime,nAges))
#DefB_phjromyta = array(0,dim=c(nPred,nAreas,nAreas,nRun,nOrigin,2,nYear,nTime,nAges))
#DefE_phjromyta = array(0,dim=c(nPred,nAreas,nAreas,nRun,nOrigin,2,nYear,nTime,nAges))

#Survival rates from Appendix table 5-5 in FRAM documentation
stx = as.matrix(read.csv("FRAM_survivalRates.csv",header=FALSE))
sv_ta = array(stx, dim=c(nTime,nAges),dimnames=list(1:nTime,paste0("ocean",1:nAges)))

myRun = c(1:4,9)

tmpYears = 1:46
if(runMCChain==TRUE)
  tmpYears = simYears

for(yi in tmpYears)
{
  for(ti in 1:nTime)
  {
    for(hi in 1:(nAreas-1)){
      for(ri in myRun){
        for(oi in 1:nOrigin){ #1 is hatchery
          #The recruitment
          S_hjromyta[hi,hi,ri,oi,2,yi,ti,1] = R_hroy[hi,ri,oi,yi] * SMT_hryt[hi,ri,ti]
          #S_hjromyta[hi,hi,ri,oi,2,yi,ti,1] = 10000 #* SMT_hryt[hi,ri,ti]
          if(ti>1){
            #Move the smolts from the previous time-setp to the black box and accumulate them with the Smolts
            S_hjromyta[hi,nAreas,ri,oi,2,yi,ti,1] = (S_hjromyta[hi,nAreas,ri,oi,2,yi,ti-1,1] + 
                                                       S_hjromyta[hi,hi,ri,oi,2,yi,ti-1,1]) *
                                                      (1-sv_ta[ti,1])
            
            #Make sure there are zero smolts remaining in their area of origin in the previous time-step
            S_hjromyta[hi,hi,ri,oi,2,yi,ti-1,1] = 0
          }
          
          if(yi>1){
            for(mi in 1:2){
              #All smolt are transient
              for(ai in 2:nAges){
                #Redistribute the adults to the appropriate areas based on Ole Shelton's model
                if(ti==1){
                    #Redistribute just the ones that stayed m = 1
                    S_hjromyta[hi,,ri,oi,1,yi,ti,ai] = sum(S_hjromyta[hi,,ri,oi,1,yi-1,nTime,ai-1]) * 
                      interpWeitTHETA_hjromta[hi,,ri,oi,1,ti,ai] * 
                      (1-MAT_hra[hi,ri,ai])

                    #Redistribute just the ones that are going back home
                    S_hjromyta[hi,,ri,oi,2,yi,ti,ai] = sum(S_hjromyta[hi,,ri,oi,1,yi-1,nTime,ai-1]) * 
                      interpWeitTHETA_hjromta[hi,,ri,oi,mi,ti,ai] * 
                      (MAT_hra[hi,ri,ai])
                } 
                
                if(ti>1)
                  S_hjromyta[hi,,ri,oi,mi,yi,ti,ai] = sum(S_hjromyta[hi,,ri,oi,mi,yi,ti-1,ai]) * 
                    interpWeitTHETA_hjromta[hi,,ri,oi,mi,ti,ai]
                
#                #Survival  
                S_hjromyta[hi,,ri,oi,mi,yi,ti,ai] =  S_hjromyta[hi,,ri,oi,mi,yi,ti,ai] *
                  (1-sv_ta[ti,ai])
              }# end ai
            }#end mi
          }#end (yi>1)
        }#end oi  
      }#end ri
    }# end hi
#    
#    
#    #Now calculate the total number of individuals of a certain age in each location.
    #This is all of the fish of a certain age in each location.
    FSTotal_jyta[,yi,ti,] = apply(S_hjromyta[,,,,,yi,ti,],c(2,6),sum)
#    
    for(ji in 1:nAreas)
      for(ai in 1:nAges)
      {
#        #Estimate the stock-, run-, origin-, and migration-specific fractions in each area 
#        #so that you can distribute the predation mortality proportionally
        
        #What you want is the distribution of 
        if(FSTotal_jyta[ji,yi,ti,ai]>0)
          FS_hjromyta[,ji,,,,yi,ti,ai] = S_hjromyta[,ji,,,,yi,ti,ai]/FSTotal_jyta[ji,yi,ti,ai]
      }#end ai 
#    
#    #Estimate the number of Chinook consumed. 
    for(ji in 1:nAreas){
      for(ai in 1:nAges){
        #Total energetic demand for each predator
        tmpED_pjyta[,ji,yi,ti,ai] = apply(ED_phjytias[,,ji,yi,ti,,ai,], c(1), sum)
        for(ri in myRun){
          for(oi in 1:nOrigin){
            for(mi in 1:2){
              for(hi in 1:nAreas){
#                
#                #Number of Chinook consumed
#                #This is calories for predator divided by calories for prey.
                #Divided the predation in that location ji that are age ai, 
                NC_phjromyta[,hi,ji,ri,oi,mi,yi,ti,ai] =  tmpED_pjyta[,ji,yi,ti,ai] /
                  (EC_hromta[hi,ri,oi,mi,ti,ai]) *
                  (FS_hjromyta[hi,ji,ri,oi,mi,yi,ti,ai])#
                
                #Sandra Oneill formula - reverse calculate that weights
                #BC_phjromyta[,hi,ji,ri,oi,mi,yi,ti,ai] = exp((log(tmpED_pjyta[,ji,yi,ti,ai]) - 7.56)/0.94) * (FS_hjromyta[hi,ji,ri,oi,mi,yi,ti,ai]) 
                BC_phjromyta[,hi,ji,ri,oi,mi,yi,ti,ai] = NC_phjromyta[,hi,ji,ri,oi,mi,yi,ti,ai] * 
                  exp((log(0.000011 * CLA_hromta[hi,ri,oi,mi,ti,ai] ^ 3.12) - 7.54)/0.94)  #Sandra Oneill formula
                
#                #Keep track of the deficit in Chinook abundance
                if(S_hjromyta[hi,ji,ri,oi,mi,yi,ti,ai]<sum(NC_phjromyta[,hi,ji,ri,oi,mi,yi,ti,ai]))
                {
                  DefC_phjromyta[,hi,ji,ri,oi,mi,yi,ti,ai] = (S_hjromyta[hi,ji,ri,oi,mi,yi,ti,ai] - sum(NC_phjromyta[,hi,ji,ri,oi,mi,yi,ti,ai])) * 
                    NC_phjromyta[,hi,ji,ri,oi,mi,yi,ti,ai] / 
                    sum(NC_phjromyta[,hi,ji,ri,oi,mi,yi,ti,ai])
#                  DefB_phjromyta[,hi,ji,ri,oi,mi,yi,ti,ai] = DefC_phjromyta[,hi,ji,ri,oi,mi,yi,ti,ai] * 
#                    exp((log(0.000011 * CLA_hromta[hi,ri,oi,mi,ti,ai] ^ 3.12) - 7.54)/0.94)  #Sandra Oneill formula
#                  DefE_phjromyta[,hi,ji,ri,oi,mi,yi,ti,ai] = DefC_phjromyta[,hi,ji,ri,oi,mi,yi,ti,ai] * EC_hromta[hi,ri,oi,mi,ti,ai]
#                
                }
#                
#                #Subtract the consumption - S1 is just a temporary variable for doing some plotting calculations and checks
                S1_hjromyta[hi,ji,ri,oi,mi,yi,ti,ai] = S_hjromyta[hi,ji,ri,oi,mi,yi,ti,ai]
                S_hjromyta[hi,ji,ri,oi,mi,yi,ti,ai] = S_hjromyta[hi,ji,ri,oi,mi,yi,ti,ai] - #sum(NC_phjromyta[,hi,ji,ri,oi,mi,yi,ti,ai])
                  min(S_hjromyta[hi,ji,ri,oi,mi,yi,ti,ai] * 0.95,sum(NC_phjromyta[,hi,ji,ri,oi,mi,yi,ti,ai]))
                  
              }#end hi
            }#end mi
          }#end oi
        }#end ri
      }#end ai
    }#end ji
    
      
    #Subtract the escapement after consumption has occurred
    #This can only be done after all of the consumption has occured 
    for(hi in 1:nAreas){
      for(ai in 2:nAges){ #smolt are not part of the escapement
        for(ri in myRun){
          for(oi in 1:nOrigin){
            if(ti<nTime)
            {
#              #ESC_hjroyta[hi,,ri,oi,yi,ti,ai] = S_hjromyta[hi,,ri,oi,2,yi,ti,ai] * #area specific abundances
#              #  ESCD_hj[hi,] / #fraction of escapement coming from each area
#              #  (sum(S_hjromyta[hi,,ri,oi,2,yi,ti,ai]*ESCD_hj[hi,]) + 1) * #total weighted abundances using the temporary variable S1
#              #  ESCT_hrt[hi,ri,ti] * #fraction escaping
#              #  sum(S_hjromyta[hi,,ri,oi,2,yi,ti,ai]) #Total abundance using the temporary variable S1
#              
              ESC_hjroyta[hi,,ri,oi,yi,ti,ai] = ESCT_hrt[hi,ri,ti] * #fraction escaping
                S_hjromyta[hi,,ri,oi,2,yi,ti,ai] #Total abundance using the temporary variable S1
#              
              S_hjromyta[hi,,ri,oi,2,yi,ti,ai] = S_hjromyta[hi,,ri,oi,2,yi,ti,ai] - 
                ESC_hjroyta[hi,,ri,oi,yi,ti,ai]
            }
          
            #just make sure there migrant fish left.
            if(ti==nTime)
            {
              ESC_hjroyta[hi,,ri,oi,yi,ti,ai] = S_hjromyta[hi,,ri,oi,2,yi,ti,ai]
              S_hjromyta[hi,,ri,oi,2,yi,ti,ai] = 0
            }
          }#oi
        }#ri
      }#ai
    }#hi
  }#end time step
  
  
  #You had to send the rest of the smolts from time-step 12 to black box at the end of the year
  #And change them from mgiratory fish to fish that are residents
  for(hi in 1:(nAreas-1)){
    
    S_hjromyta[hi,nAreas,,,1,yi,12,1] = S_hjromyta[hi,nAreas,,,2,yi,12,1] + 
      S_hjromyta[hi,hi,,,2,yi,12,1] *
      (1-sv_ta[nTime,1]) 
    
    #You have to zero out the migratory smolts since they become stationary smolts at the very end of the year.
    S_hjromyta[hi,nAreas,,,2,yi,12,1] = 0
    #This is just to make sure there are no smolts left in the area origin at the end of the year
    S_hjromyta[hi,hi,,,2,yi,12,1] = 0

    #Ignore the fact that I've replaced the ji subscript with the hi subscript,
    #it doesn't matter.  I'm loooping over areas
    #RatioC_jyta[hi,yi,,] = apply(S1_hjromyta[,hi,,,,yi,,],c(5,6),sum)/ apply(NC_phjromyta[,,hi,,,,yi,,],c(6,7),sum)
    
  }# end hi area
}#end year step


