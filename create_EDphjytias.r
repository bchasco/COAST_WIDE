
#Energetic demands array
ED_phjytias = array(0,
                  c(nPred,nAreas,nAreas,nYear,nTime,predAges,nAges,nSex), 
                  dimnames = list(predNames,areaNames,areaNames, 1:nYear,paste0("mon",1:nTime), 1:predAges,0:(nAges-1), sexNames))

#Diet fraction array
corrected_FEC_pjt = array(0,
                c(nPred,nAreas, nTime), 
                dimnames = list(predNames,areaNames, paste0("mon",1:nTime)))

#This FO 2 SSFO tranformation is need for all pinnipeds that are not in the Salish Sea.
#This is based on Austen Thomas's 2016 work.
FO_2_SSFO = c(0.25,0.16,0.16,0.16,0.16,0.16)

tmpYears = 1:46
if(runMCChain==TRUE)
  tmpYears = simYears

for(pi in 1:nPred){
  for(hi in 1:nAreas){
    for(ji in 1:nAreas){
      for(yi in tmpYears){
        for(ti in 1:nTime){
            for(ai in 1:nAges){
                #tmpFO_2_SSFO = 1
                #if(pi>1 & ji!=5) #basically if you're not a killer whale or a pinniped in the Salish Sea
                #  tmpFO_2_SSFO = FO_2_SSFO[ai]
                
                if(pi ==1)
                {
                  ED_phjytias[pi,hi,ji,yi,ti,,ai,] = t(PHI_phjts[pi,hi,ji,ti,] * #Fraction of the pop 'hi' in area 'ji'
                                                         t(SEL_pjta[pi,ji,ti,ai] * #Age selectivity
                                                             FEC_pjt[pi,hi,ti] * #Fraction of energy from Chinook
                                                             EP_phis[pi,hi,,] * #Energetic demands of the predator
                                                             N_phyis[pi,hi,yi,,])) * #Abundance
                    nDays[ti] #number of days in a month
                  
                }

                if(pi !=1)
                {
                  #all of the FEC transformation could be moved 
                  #out of this loop and into the create_FEC wrapper.
                  #However, there was original some consideration about other factors 
                  #such as year, predator age and location affecting the 
                  #diet fraction
                  if(FEC_FO_correction_pjt[pi,ji,ti]==0){
                    corrected_FEC_pjt[pi,ji,ti] = FEC_pjt[pi,ji,ti] #Fraction of energy from Chinook
                  }
                  if(FEC_FO_correction_pjt[pi,ji,ti]==1){
                    corrected_FEC_pjt[pi,ji,ti] = FEC_pjt[pi,ji,ti] * #Fraction of energy from Chinook
                      FO_2_SSFO[ai]  #This is transformation from FO to SSFO based on Thomas et al. 
                    if(pi==3 & ji==3) #This is a special correction factor for the California Sea lions in the COlumbia River
                      corrected_FEC_pjt[pi,ji,ti] = corrected_FEC_pjt[pi,ji,ti] * 0.93 + 0.07 * 0.72
                    if(pi==4 & ji==3) #This is a special correction factor for the California Sea lions in the COlumbia River
                      corrected_FEC_pjt[pi,ji,ti] = corrected_FEC_pjt[pi,ji,ti] * 0.93 + 0.07 * 0.39
                  }
                  
                  #if(yi==46 & pi==3 & ji==3)
                  #  print(paste(pi,ji,ti,FEC,FEC_pjt[pi,ji,ti],FEC_FO_correction_pjt[pi,ji,ti],FO_2_SSFO[ai]))
                  ED_phjytias[pi,hi,ji,yi,ti,,ai,] = t(PHI_phjts[pi,hi,ji,ti,] * #Fraction of the pop 'hi' in area 'ji'
                                                         t(SEL_pjta[pi,ji,ti,ai] * #Age selectivity
                                                          corrected_FEC_pjt[pi,ji,ti] *  #This is transformation from FO to SSFO based on Thomas et al. 
                                                          EP_phis[pi,hi,,] * #Energetic demands of the predator
                                                          N_phyis[pi,hi,yi,,])) * #Abundance
                                                          nDays[ti] #number of days in a month
                  
                }
              }
          }
      }
    }
  }
}


