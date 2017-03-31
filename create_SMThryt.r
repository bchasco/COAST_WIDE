stx = read.csv("Smolt_AllData.csv")

#Smolt timing
SMT_hryt = array(0,
                dim=c(nAreas,nRun,nTime),
                dimnames = list(areaNames,runNames,1:nTime))


for(i in 1:nrow(stx))
{
  year = stx$year[i]
  mon = stx$mon[i]
  area = stx$areaID[i]
  run = stx$run2[i] #spring, summer, fall,winter, other
  
  SMT_hryt[area,run,mon] = SMT_hryt[area,run,mon] + stx$total[i]
}

for(hi in 1:nAreas)
  for(ri in 1:nRun)
    for(yi in 1:nYear)
    {
      SMT_hryt[hi,ri,] = SMT_hryt[hi,ri,]/sum(SMT_hryt[hi,ri,]+1e-10)
    }

#Fill in missing runs.  Some areas have release information but no run timing information for a particular run
#This loop uses the run timing information from the previous area (hi -1) to infer what the run timing of the 
#current area is.
for(hi in 1:nAreas)
  for(ri in 1:nRun)
        if(sum(SMT_hryt[hi,ri,])==0)
          SMT_hryt[hi,ri,] = colSums(SMT_hryt[,ri,])/(sum(SMT_hryt[,ri,])+1e-10)

for(hi in 1:nAreas)
  for(ri in 1:nRun)
      SMT_hryt[hi,ri,] = SMT_hryt[hi,ri,]/sum(SMT_hryt[hi,ri,])



