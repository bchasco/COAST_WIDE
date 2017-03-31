#To create the array of the number of predator by sex and age
#We need to read in three sources of information 
#the annual abundance of the predators, the sex ratio of the predators
#and the age ratio of the predators.  Not all of the calculations for 
#these predators are the same.  See the appendix spreadsheet for 
#details for each species and area.

#Read in the data
atx = read.csv("predAnnualTotals.csv")
sx = read.csv("predatorFemaleRatio.csv")
aRx = read.csv("predatorAgeRatios.csv")

#The home location where each population begins
N_phy = array(0,
                             c(nPred,nAreas,nYear), 
                             dimnames = list(predNames,paste0("home",1:nAreas),paste0("year",1:nYear)))

#Predator sex ratio
predFemaleRatio_phyi = array(0,
                             c(nPred,nAreas,nYear,predAges), 
                             dimnames = list(predNames,paste0("home",1:nAreas),paste0("year",1:nYear),paste0("age",1:predAges)))


#Predator age ratio
predAgeRatio_phyi = array(0,
                          c(nPred,nAreas,nYear,predAges), 
                          dimnames = list(predNames,paste0("home",1:nAreas),paste0("year",1:nYear),paste0("age",1:predAges)))


#The number at age, by predator, area, and year
N_phyis = array(0,
                c(nPred,nAreas,nYear,predAges,nSex), 
                dimnames = list(predNames,paste0("home",1:nAreas),paste0("year",1:nYear),paste0("age",1:predAges), sexNames))

#sensitivity error
err = exp(rnorm(1,-0.5*MC_Ncv[cvCnt]^2,MC_Ncv[cvCnt]))
print(paste("N", err))

#Annual predator totals
for(i in 1:nrow(atx))
{
  pi = atx[i,2] #pred species
  yi = atx[i,4] #year
  hi = atx[i,1] #home area
  N_phy[pi,hi,yi] = atx[i,6]
  
  if(runMCChain==TRUE & iSim>1){
    if(pi!=1) 
    {
      #N_phy[pi,hi,yi] = N_phy[pi,hi,yi]*exp(rnorm(1,0,MC_Ncv[cvCnt]))
      N_phy[pi,hi,yi] = N_phy[pi,hi,yi]*err
    }
    
  }
    
}

#You need to subtract the abundance of adult in the Oregon, Columbia River, 
#Washington and Salish Sea from the SoCal total.
N_phy[3,1,] = N_phy[3,1,] - colSums(N_phy[3,2:6,])


#Read in the sex ratio data
#This should really be the female sex ratio
for(i in 1:nrow(sx))
{
  hi = sx[i,1] #home area
  pi = sx[i,3] #pred species
  yi = sx[i,4] #year
  predFemaleRatio_phyi[pi,hi,yi,] = t(sx[i,5:ncol(sx)])
}
#Because we do not have annual sex ratios for many of these populations
#We need to fill in all of the subsequent years
for(pi in 1:nPred)
  for(hi in 1:nAreas)
    for(yi in 2:nYear)
    {
      if(pi!=1 | hi!=5)
        predFemaleRatio_phyi[pi,hi,yi,] = predFemaleRatio_phyi[pi,hi,1,]
    }

#Read in the age ratio data
for(i in 1:nrow(aRx))
{
  hi = aRx[i,1] #home area
  pi = aRx[i,3] #pred species
  yi = aRx[i,4] #year
  predAgeRatio_phyi[pi,hi,yi,] = t(aRx[i,5:ncol(aRx)])
}
#Because we do not have annual age ratios for many of these populations
#We need to fill in all of the subsequent years
for(pi in 2:nPred)
  for(hi in 1:nAreas)
    for(yi in 2:nYear)
    {
      predAgeRatio_phyi[pi,hi,yi,] = predAgeRatio_phyi[pi,hi,1,]
    }

#There is no time-varying age composition for the killer whales in areas 7 and 8
for(pi in 1:1)
  for(hi in 7:8)
    for(yi in 2:nYear)
    {
      predAgeRatio_phyi[pi,hi,yi,] = predAgeRatio_phyi[pi,hi,1,]
    }

#Fill in the numbers-at-age, by predator, area, and year.
for(pi in 1:nPred)
  for(hi in 1:nAreas)
    for(yi in 1:nYear)
      for(ii in 1:predAges)
      {
        N_phyis[pi,hi,yi,ii,1]  = N_phy[pi,hi,yi] * (1-predFemaleRatio_phyi[pi,hi,yi,ii]) * predAgeRatio_phyi[pi,hi,yi,ii]
        N_phyis[pi,hi,yi,ii,2]  = N_phy[pi,hi,yi] * (predFemaleRatio_phyi[pi,hi,yi,ii]) * predAgeRatio_phyi[pi,hi,yi,ii]
      }
        



