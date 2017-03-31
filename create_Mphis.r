#Read in the data
wtx = read.csv("predatorWeightAtAge.csv")

#Predator weight
M_phis = array(0,
               c(nPred,nAreas, predAges,nSex), 
               dimnames = list(predNames,areaNames, paste0("age",1:predAges),c("m","f")))

#Read in the weight at age data
for(i in 1:nrow(wtx))
{
  pi = wtx[i,1] #pred species
  si = wtx[i,2] #sex
    for(hi in 1:nAreas)
      M_phis[pi,hi,,si] = t(wtx[i,3:ncol(wtx)])
}

