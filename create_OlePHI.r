attach("OleDistributionData.RData")

#This is the second slot in Ole's array
#Origin
OleAreaID = c(1,2,2,2,2,3,3,4,5,5,6)

#This is the third slot in Ole's array
#Location
OleLocID = c(1,1,1,2,2,2,2,3,4,5,5,6,6,6,6,7,7)

#Map the ole times to the monthly array
timeID = c(1,1,1,1,1,2,2,2,3,3,1,1)

OlePHI_hjromta = array(0,c(nAreas,nAreas,nRun,nOrigin,2,nTime,nAges))

hcnt = 1
for(hi in OleAreaID)
{
  jcnt = 1
  for(ji in OleLocID)
  {
    for(ti in 1:12)
    {
      for(ai in 2:nAges)
      {
        for(oi in 1:nOrigin)
          for(mi in 1:2)
            OlePHI_hjromta[hi,ji,3,oi,mi,ti,ai] = OlePHI_hjromta[hi,ji,3,oi,mi,ti,ai] + Data$origin_by_loc[timeID[ti],hcnt,jcnt]
      }  
    }
    jcnt = jcnt + 1
  }
  hcnt = hcnt + 1
}

for(hi in 1:nAreas)
  for(ti in 1:12)
    for(ai in 2:nAges)
      for(oi in 1:nOrigin)
        for(mi in 1:2)
          OlePHI_hjromta[hi,,3,oi,mi,ti,ai] = OlePHI_hjromta[hi,,3,oi,mi,ti,ai]/sum(OlePHI_hjromta[hi,,3,oi,mi,ti,ai])

par(mfrow=c(4,2), mai=c(0,0,0,0))
for(hi in 1:8)
{
  image(OlePHI_hjromta[hi,,3,1,1,c(1,6,9),2], col=terrain.colors(100))
}

