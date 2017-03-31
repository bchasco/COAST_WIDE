stx = read.csv("Smolt_AllData.csv")

#Smolt length
SMTL_hryt = array(0,
                  dim=c(nAreas,nRun,nYear,nTime),
                  dimnames = list(areaNames,runNames,1:nYear,1:nTime))

#interpolated values
iSMTL_hryt = array(0,
                  dim=c(nAreas,nRun,nYear,nTime),
                  dimnames = list(areaNames,runNames,1:nYear,1:nTime))
#Data weighting for calculating smolt length
SMTLN_hryt = array(0,
                  dim=c(nAreas,nRun,nYear,nTime),
                  dimnames = list(areaNames,runNames,1:nYear,1:nTime))

for(i in 1:nrow(stx))
{
  year = stx$year[i]
  mon = stx$mon[i]
  area = stx$areaID[i]
  run = stx$run2[i] #spring, summer, fall,winter, other

  if(is.na(stx$avg_length[i])==FALSE)
  {
    SMTLN_hryt[area,run,year-1970+1,mon] = SMTLN_hryt[area,run,year-1970+1,mon] + stx$total[i]
    SMTL_hryt[area,run,year-1970+1,mon] = SMTL_hryt[area,run,year-1970+1,mon] + stx$avg_length[i]  * stx$total[i]
  }
}


#Determine the weight average for the lengths
for(hi in 1:nAreas)
  for(ri in 1:nRun)
    for(yi in 1:nYear)
    {
      SMTL_hryt[hi,ri,yi,] = SMTL_hryt[hi,ri,yi,]/(SMTLN_hryt[hi,ri,yi,]+1)
    }

#Now fill the missing months with averages across species within an area
for(hi in 1:nAreas)
  for(ri in 1:nRun)
    for(yi in 1:nYear)
      for(ti in 1:nTime)
      {
        if((SMTL_hryt[hi,ri,yi,ti])==0)
        {
          #First average across runs and years for a particular month
          tmp = c(SMTL_hryt[hi,,,ti])
          iSMTL_hryt[hi,ri,yi,ti] = mean(tmp[tmp>0])
          
          #If there is no data average across runs and years, average across areas, runs and years
          if(length(tmp[tmp>0])==0)
          {
            tmp = c(SMTL_hryt[,,,ti])
            iSMTL_hryt[hi,ri,yi,ti] = mean(tmp[tmp>0])
          }            
        }
      }


#Fill in the interpolated values after the plot
for(hi in 1:nAreas)
  for(ri in 1:nRun)
    for(yi in 1:nYear)
      for(ti in 1:nTime)
      {
        if((SMTL_hryt[hi,ri,yi,ti])==0)
        {
          SMTL_hryt[hi,ri,yi,ti] = iSMTL_hryt[hi,ri,yi,ti]
        }
      }

#This is part of Brian Burke suggestion
#40 days
#1.3 mm/day growth rate.
SMTL_hryt = SMTL_hryt + 40 * 1.0 

err = exp(rnorm(1,-0.5*MC_SMTLcv^2,MC_SMTLcv))
print(paste("SMTL", err))

if(runMCChain==TRUE & iSim>1){
  #Smolt length
  SMTL_hryt = SMTL_hryt*err

}