#Read in the data
ptx = read.csv("predatorTemporalSpatialDistribution.csv")


#Diet fraction/selectivity array
PHI_phjts = array(0,
                 c(nPred,nAreas,nAreas,nTime,nSex), 
                 dimnames = list(predNames,areaNames,areaNames, paste0("mon",1:nTime),sexNames))

#REad in the age selectivity of the predators
for(i in 1:nrow(ptx))
{
  pi = ptx[i,1] #pred species
  hi = ptx[i,2] #area
  ti = ptx[i,3] #time
  si = ptx[i,4] #time
  PHI_phjts[pi,hi,,ti,si] = t(ptx[i,5:13])
}
