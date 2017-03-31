stx = read.csv("Smolt_AllData.csv")

#Smolt timing
SMT_hryt = array(0,
                dim=c(nAreas,nRun,nYear,nTime),
                dimnames = c(areaNames,runNames,1:nYear,1:nTime))


for(i in 1:nrow(stx))
{
  year = stx$year[i]
  mon = stx$mon[i]
  area = stx$areaID[i]
  run = stx$run2[i] #spring, summer, fall,winter, other
  
  SMT_hryt[area,run,year-1970+1,mon] = SMT_hryt[area,run,year-1970+1,mon] + stx$total[i]
}

for(hi in 1:nAreas)
  for(ri in 1:nRun)
    for(yi in 1:nYear)
    {
      SMT_hryt[hi,ri,yi,] = SMT_hryt[hi,ri,yi,]/sum(SMT_hryt[hi,ri,yi,]+0.001)
    }

#Fill in missing years with the average timing
for(hi in 1:nAreas)
  for(ri in 1:nRun)
    for(yi in 1:nYear)
      if(sum(SMT_hryt[hi,ri,yi,])==0)
        SMT_hryt[hi,ri,yi,] = colSums(SMT_hryt[hi,ri,,])/(sum(SMT_hryt[hi,ri,,])+0.0001)

#Fill in missing runs.  Some areas have release information but no run timing information for a particular run
#This loop uses the run timing information from the previous area (hi -1) to infer what the run timing of the 
#current area is.
for(hi in 1:nAreas)
  for(ri in 1:nRun)
    for(yi in 1:nYear)
      for(oi in 1:nOrigin)
      if(sum(R_hroy[hi,ri,oi,yi])>0 & sum(SMT_hryt[hi,ri,yi,])==0)
        SMT_hryt[hi,ri,yi,] = colSums(SMT_hryt[hi-1,ri,,])/(sum(SMT_hryt[hi-1,ri,,])+0.0001)


par(mfrow=c(9,5), mai=c(0.5,0.5,0,0))
for(hi in 1:nAreas)
  for(ri in c(1:4,9))
    plot(colMeans(SMT_hryt[hi,ri,,]), type="l", col=ri, lty=1, las=1)