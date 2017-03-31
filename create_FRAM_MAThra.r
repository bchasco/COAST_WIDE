mtx = read.csv("MaturationRatesFromFRAMwithLookup.csv", header=TRUE) #This is data from Meyers et al. 1998

phra = array(0,c(nAreas,nRun,nAges))
MAT_hra = array(0,c(nAreas,nRun,nAges))
tmpMAT_hra = array(0,c(nAreas,nRun,nAges))
aveMATha = array(0,c(nAreas,nAges))
nMAThr = array(0,c(nAreas,nRun))

#Import the FRAM data.  This uses the new base period - not sure what that means, but there are 3533 rows of observations
#in this spreadsheet as opposed to ~390 in the other one.
for(hi in 1:nAreas)
  for(ri in c(1:4,9)){
  tmp = mtx[mtx$areaID==hi & mtx$runID==ri,]
  if(nrow(tmp)>0){
    agx = aggregate(list(m=tmp$MaturationRate),list(a=tmp$Age), mean)
    MAT_hra[hi,ri,agx$a] = agx$m  
  }
}

#You have to fill on the missing information that was not in the FRAM input sheet.
for(hi in 1:nAreas)
  for(ri in c(1:4,9)){
    if(sum(MAT_hra[hi,ri,])==0){
      if(sum(rowSums(MAT_hra[hi,,])>0)>1)
        MAT_hra[hi,ri,] = colMeans(MAT_hra[hi,rowSums(MAT_hra[hi,,])>0,])  
      if(sum(rowSums(MAT_hra[hi,,])>0)==1)
        MAT_hra[hi,ri,] = MAT_hra[hi,rowSums(MAT_hra[hi,,])>0,]  
      if(sum(rowSums(MAT_hra[hi,,])>0)==0)
        MAT_hra[hi,ri,] = apply(MAT_hra[rowSums(MAT_hra[,ri,])>0,ri,],c(2),mean)
    }
  }
MAT_hra[,,6] = 1  
