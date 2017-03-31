#attach("OleDistributionData.RData")

#This is the second slot in Ole's array
#Origin
OleAreaID = c(1,2,2,2,2,3,3,4,5,5,6)

#This is the third slot in Ole's array
#Location
OleLocID = c(1,1,1,2,2,2,2,3,4,5,5,6,6,6,6,7,7)

#Map the ole times to the monthly array
timeID = c(1,1,1,1,1,2,2,2,3,3,1,1)

stx = read.csv("WeitkampDistribution.csv", header=TRUE)
#The are the area locations for the columns
areaIDWeitkamp = c(8,	8,	8,	7,	7,	6,	6,	6,	6,	6,	5,	5,	4,	3,	2,	2,	2,	2,	1,	1,	1)

OleTHETA_hjromta = array(0,c(nAreas,nAreas,nRun,nOrigin,2,nTime,nAges))
WeitTHETA_hjromta = array(0,c(nAreas,nAreas,nRun,nOrigin,2,nTime,nAges))

hcnt = 1
#for(hi in OleAreaID)
#{
#  jcnt = 1
#  for(ji in OleLocID)
#  {
#    for(ti in 1:12)
#    {
#      for(ai in 2:nAges)
#      {
#        for(oi in 1:nOrigin)
#          for(mi in 1:2)
#            OleTHETA_hjromta[hi,ji,3,oi,mi,ti,ai] = OleTHETA_hjromta[hi,ji,3,oi,mi,ti,ai] + Data$origin_by_loc[timeID[ti],hcnt,jcnt]
#      }  
#    }
#    jcnt = jcnt + 1
#  }
#  hcnt = hcnt + 1
#}

#for(hi in 1:nAreas)
#  for(ti in 1:12)
#    for(ai in 2:nAges)
#      for(oi in 1:nOrigin)
#        for(mi in 1:2)
#          OleTHETA_hjromta[hi,,3,oi,mi,ti,ai] = OleTHETA_hjromta[hi,,3,oi,mi,ti,ai]/sum(OleTHETA_hjromta[hi,,3,oi,mi,ti,ai])

#Read in the Weitkamp data
for(i in 1:nrow(stx))
{
  hi = stx$areaID[i]
  ri = stx$runID[i]
  
  for(ji in 1:length(areaIDWeitkamp))
  {
    jii = areaIDWeitkamp[ji]
    for(oi in 1:nOrigin)
      for(mi in 1:2)
        for(ti in 1:nTime)
          for(ai in 2:nAges)
            WeitTHETA_hjromta[hi,jii,ri,oi,mi,ti,ai] = WeitTHETA_hjromta[hi,jii,ri,oi,mi,ti,ai] + stx[i, 6 + ji]
  }
}

#NOrmalize everything 
for(hi in 1:8)
  for(ri in c(1:4,9))
    for(oi in 1:nOrigin)
      for(mi in 1:2)
        for(ti in 1:nTime)
          for(ai in 2:nAges)
            WeitTHETA_hjromta[hi,,ri,oi,mi,ti,ai] = WeitTHETA_hjromta[hi,,ri,oi,mi,ti,ai]/sum(WeitTHETA_hjromta[hi,,ri,oi,mi,ti,ai]+1e-6)


#Weitkamp interpolated - fill in the missing runs in each area.
interpWeitTHETA_hjromta = WeitTHETA_hjromta
#Area averaging
  for(hi in 1:8)
    for(ri in c(1:3)){
      if(sum(WeitTHETA_hjromta[hi,,ri,oi,mi,ti,ai])==0){
        for(oi in 1:nOrigin)
          for(mi in 1:2)
            for(ti in 1:nTime)
              for(ai in 2:nAges)
              {
                tmpRun = c(1:4,9)[c(1:4,9)!=ri]
                if(hi<8 & hi>1)
                  interpWeitTHETA_hjromta[hi,,ri,oi,mi,ti,ai] = interpWeitTHETA_hjromta[hi+1,,ri,oi,mi,ti,ai] + apply(interpWeitTHETA_hjromta[hi,,tmpRun,oi,mi,ti,ai],1,sum) # + interpWeitTHETA_hjromta[hi-1,,ri,oi,mi,ti,ai] #
                if(hi==8)
                  interpWeitTHETA_hjromta[hi,,ri,oi,mi,ti,ai] = interpWeitTHETA_hjromta[hi-1,,ri,oi,mi,ti,ai] + apply(interpWeitTHETA_hjromta[hi,,tmpRun,oi,mi,ti,ai],1,sum)
                if(hi==1)
                  interpWeitTHETA_hjromta[hi,,ri,oi,mi,ti,ai] = interpWeitTHETA_hjromta[hi+1,,ri,oi,mi,ti,ai] + apply(interpWeitTHETA_hjromta[hi,,tmpRun,oi,mi,ti,ai],1,sum)
                
              }
      }        
    }

interpWeitTHETA_hjromta[,,4,,,,] = interpWeitTHETA_hjromta[,,3,,,,]
for(ri in 1:4)
  interpWeitTHETA_hjromta[,,9,,,,] = interpWeitTHETA_hjromta[,,9,,,,] + interpWeitTHETA_hjromta[,,ri,,,,]


#normalize everything again so that columns sum to one
for(hi in 1:8)
  for(ri in c(1:4,9))
    if(sum(WeitTHETA_hjromta[hi,,ri,oi,mi,ti,ai])==0){
      for(oi in 1:nOrigin)
        for(mi in 1:2)
          for(ti in 1:nTime)
            for(ai in 2:nAges)
              interpWeitTHETA_hjromta[hi,,ri,oi,mi,ti,ai] = interpWeitTHETA_hjromta[hi,,ri,oi,mi,ti,ai]/sum(interpWeitTHETA_hjromta[hi,,ri,oi,mi,ti,ai])
    }              

#get rid of the NAs
interpWeitTHETA_hjromta[is.na(interpWeitTHETA_hjromta)==TRUE] = 0

#These are the run types I no nothing about.  They are really small fraction of the population
for(ri in 5:8)
  interpWeitTHETA_hjromta[,,ri,,,,] = interpWeitTHETA_hjromta[,,1,,,,]
