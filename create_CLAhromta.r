
#Length at age
CLA_hromta = array(0,dim=c(nAreas,nRun,nOrigin,2,nTime,nAges))

#The energetic content of salmon is a function of their length
#The length of smolts is known from hatchery releases and we assume
#that wild fish of the same run and origin are the same size.
#The length of CHinook greater than ocean age 0 are based on FRAM estimates.

ltx = read.csv("ChinookLengthAtAge.csv")

for(hi in 1:nAreas)
  for(ri in 1:nRun)
    for(oi in 1:nOrigin)
      for(mi in 1:2)
        for(ti in 1:nTime)
          for(ai in 1:nAges)
          {
            myLen = ltx$length[ltx$runID == ri & ltx$oceanAge == ai & ltx$month==ti & ltx$areaID == hi  & ltx$Mat==mi & ltx$originID == oi]
            if(length(myLen)>=1) 
            {
              CLA_hromta[hi,ri,oi,mi,ti,ai+1] = mean(myLen)
            }
          }


for(hi in 1:nAreas)
  for(ri in 1:nRun)
    for(oi in 1:nOrigin)
      for(mi in 1:2)
        for(ti in 1:nTime)
          for(ai in 2:nAges)
          {
            if(CLA_hromta[hi,ri,oi,mi,ti,ai] == 0 | is.nan(CLA_hromta[hi,ri,oi,mi,ti,ai]))
            {
              CLA_hromta[hi,ri,oi,mi,ti,ai] = mean(ltx$length[ltx$oceanAge == (ai - 1) & ltx$month==ti])                
            }
            
            #The four ocean fish do not have any observations after month 9
            if(ai==(nAges-1) & is.nan(CLA_hromta[hi,ri,oi,mi,ti,ai])==TRUE)
              CLA_hromta[hi,ri,oi,mi,ti,ai] = 860 #max(CLA_hromta[hi,ri,oi,mi,is.nan(CLA_hromta[hi,ri,oi,mi,,ai])==FALSE,ai])
            #There are no observation in the FRAM database for 5 ocean fish.
            if(ai==nAges & is.nan(CLA_hromta[hi,ri,oi,mi,ti,ai])==TRUE)
              CLA_hromta[hi,ri,oi,mi,ti,ai] = 900 #max(CLA_hromta[hi,ri,oi,mi,is.nan(CLA_hromta[hi,ri,oi,mi,,ai])==FALSE,ai])
          }

#Add the smolt lengths
for(hi in 1:nAreas)
  for(ri in 1:nRun)
    for(oi in 1:nOrigin)
      for(mi in 2:2)
      {
        #smolts
        CLA_hromta[hi,ri,oi,mi,,1] = apply(SMTL_hryt[hi,ri,,],2,mean)
        #Adults
        CLA_hromta[hi,ri,oi,mi,CLA_hromta[hi,ri,oi,mi,,1]==0,1] = mean(SMTL_hryt[hi,,,])
        #North bound smolts
        CLA_hromta[hi,ri,oi,1,,1] = CLA_hromta[hi,ri,oi,2,,1]
      }

