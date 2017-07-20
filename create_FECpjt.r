#Read in the data
ftx = read.csv("predatorDietFraction.csv")

#Diet fraction array
FEC_pjt = array(0,
                 c(nPred,nAreas, nTime), 
                 dimnames = list(predNames,areaNames, paste0("mon",1:nTime)))

#This is the FO correction factor
FEC_FO_correction_pjt = array(0,
                c(nPred,nAreas, nTime), 
                dimnames = list(predNames,areaNames, paste0("mon",1:nTime)))

#REad in the age selectivity of the predators
fa = function(mu,var){
  return(mu*((mu*(1-mu))/var-1))}
fb = function(mu,var){
  return((1-mu)*((mu*(1-mu))/var-1))}



#REad in the age selectivity of the predators
err = exp(rnorm(1,0,MC_FECcv))
print(paste("FEC", err))
for(i in 1:nrow(ftx))
{
  pi = ftx[i,1] #pred species
  ji = ftx[i,2] #area
  ti = ftx[i,3] #time
  cr = ftx[i,7] #correction factor for aggregated salmon stocks.
  agg = ftx[i,6]
  
  if(ftx[i,4]=="FO")
    FEC_FO_correction_pjt[pi,ji,ti] = 1
  
  FEC_pjt[pi,ji,ti] = ftx[i,5]

  if(agg==1) #rescale aggregated salmon FO by the Chinook ratio
    FEC_pjt[pi,ji,ti] = FEC_pjt[pi,ji,ti] * cr
  
  if(runMCChain==FALSE & iSim>1){
    #mymu = FEC_pjt[pi,ji,ti]
    #myvar = (MC_FECcv[cvCnt]*mymu)^2
    #if(myvar>(mymu*(1-mymu)))
    #{
    #  myvar = (mymu*(1-mymu)) * 0.5
    #}
    
    #if(myvar<(mymu*(1-mymu))){
    #  FEC_pjt[pi,ji,ti] = rbeta(1,fa(mymu,myvar),fb(mymu,myvar))
    #}
    FEC_pjt[pi,ji,ti] = max(min(FEC_pjt[pi,ji,ti]*err,1),0)
  }

}

#The cosumption rate for California and Steller sea lion in the Columbia River consists of 
#the 7% of sea lions at the dam where there are viusal observation of consumption.
#Those estimates are about ~72% Chinook salmon for CSL and 39% for SSL.

#The other estimates for the other 93% of sea lions is assuming assumed to be 18.1% based on an aggregated salmon estimate
#, although Jefferies says it's zero.  So, we first calculate the
#energy fraction based on the FO data - with correction for aggregated salmon in the scat, and 
#then add to that 0.07 * 0.72.  This represents the consumption of salmon at the dam.
