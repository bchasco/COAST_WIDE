#mtx = read.csv("Salmon_maturityAtAge.csv", header=TRUE)

mtx = read.csv("FRAM_maturityAtAge.csv", header=TRUE)

phra = array(0,c(nAreas,nRun,nAges))
MAT_hra = array(0,c(nAreas,nRun,nAges))
tmpMAT_hra = array(0,c(nAreas,nRun,nAges))
aveMATha = array(0,c(nAreas,nAges))
nMAThr = array(0,c(nAreas,nRun))

for(hi in 1:8)
{
  if(nrow(mtx[mtx$AreaID==hi,4:8])>0)
    aveMATha[hi,2:nAges] = colMeans(mtx[mtx$AreaID==hi,4:8])
  
  for(ri in c(1:4,9))
  {
    if(nrow(mtx[mtx$AreaID==hi & mtx$run==ri,4:8])>0)
    {
      MAT_hra[hi,ri,2:nAges] = colMeans(mtx[mtx$AreaID==hi & mtx$run==ri,4:8])
    }
    
    if(nrow(mtx[mtx$AreaID==hi & mtx$run==ri,4:8])==0)
      MAT_hra[hi,ri,2:nAges] = aveMATha[hi,2:nAges]
  }
}

#Since there are no Alaska observations,
#make them equal to teh BC observations
MAT_hra[7,,] = MAT_hra[6,,]
MAT_hra[8,,] = MAT_hra[6,,]
