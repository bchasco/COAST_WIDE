#Kleiber energetic demands
EP_phis = array(0,
                 c(nPred,nAreas,predAges,nSex), 
                 dimnames = list(predNames,areaNames, paste0("age",1:predAges),sexNames))

#Digestive Efficiency of the predator, from different papers.  See the appendix.
Ef_p = c(0.845,0.825,0.875,0.875)

#REad in the age selectivity of the predators
for(pi in 1:nPred)
  for(hi in 1:nAreas)
    for(ii in 1:predAges)
      for(si in 1:nSex)
      {
        EP_phis[pi,hi,ii,si] = (ALPHA_pis[pi,hi,si]*M_phis[pi,hi,ii,si]^0.75)/Ef_p[pi]
      }

