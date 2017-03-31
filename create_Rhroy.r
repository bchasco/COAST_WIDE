wtx = read.csv("WildProduction.csv")
tmp = read.csv("RMIS_adclip_database.csv")
tmp = tmp[tmp$last_release_date_year>=1970,]
tmp = tmp[tmp$last_release_date_year<=2015,]

#Smolt abundance
R_hroy = array(0,
                 dim=c(nAreas,nRun,nOrigin,nYear),
                 dimnames = list(areaNames,runNames,originNames,1:nYear))

#Wild Escapement estimates
WildEsc_hrvy = array(0,
               dim=c(nAreas,nRun,max(wtx$rivID),nYear),
               dimnames = list(areaNames,runNames,1:max(wtx$rivID),1:nYear))

for(i in 1:nrow(tmp))
{
  year = tmp$last_release_date_year[i]
  area = tmp$areaID[i]
  run = tmp$run[i] #spring, summer, fall,winter, other
  oi = 1 #for hatchery
  R_hroy[area,run,1,year-1970+1] = R_hroy[area,run,1,year-1970+1] + tmp$total[i]
}

#Read in escapement
for(i in 1:nrow(wtx))
{
  year = wtx$Year[i]
  area = wtx$areaID[i]
  run = wtx$runID[i] #spring, summer, fall,winter, other
  river = wtx$rivID[i] #spring, summer, fall,winter, other
  if((year-1970+1)>=1)
    WildEsc_hrvy[area,run,river,year-1970+1] = WildEsc_hrvy[area,run,river,year-1970+1] + wtx$escapement[i]
}

#Fill in missing years
for(hi in 1:nAreas)
  for(ri in 1:nRun)
    for(rivi in 1:max(wtx$rivID))
      if(sum(WildEsc_hrvy[hi,ri,rivi,])>0) #only interpolate is the river has any data
        WildEsc_hrvy[hi,ri,rivi,WildEsc_hrvy[hi,ri,rivi,]==0] = 
          mean(WildEsc_hrvy[hi,ri,rivi,WildEsc_hrvy[hi,ri,rivi,]!=0])

#Smolts per escapement
R_hroy[,,2,] = apply(WildEsc_hrvy, c(1,2,4),sum)/2*200
