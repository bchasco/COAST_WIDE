#Read in the data
alphatx = read.csv("kleiberAlpha.csv")

#Kleiber alpha array
ALPHA_pis = array(0,
               c(nPred,predAges,nSex), 
               dimnames = list(predNames,paste0("age",1:predAges),sexNames))

#Read in the Kleiber multipliers
err = exp(rnorm(1,0,MC_ALPHAcv))
print(paste("alpha", err))

for(i in 1:nrow(alphatx))
{
  pi = alphatx[i,1] #pred species
  si = alphatx[i,3] #sex
  ii = alphatx[i,2] #age
  ALPHA_pis[pi,ii,si] = alphatx[i,4]
  if(runMCChain==TRUE & iSim>1)
    #ALPHA_pis[pi,ii,si] = ALPHA_pis[pi,ii,si]*exp(rnorm(1,0,MC_ALPHAcv[cvCnt]))
    ALPHA_pis[pi,ii,si] = ALPHA_pis[pi,ii,si]*err
}

