etx = read.csv("EscTiming.csv", header=TRUE)

ESCT_hrt = array(0,c(nAreas,nRun,nTime))

for(i in 1:nrow(etx))
{
  hi = etx[i,1] #area id
  ri = etx[i,2] #run id
  ESCT_hrt[hi,ri,] = t(etx[i,3:14])
}

for(ri in c(5:8))
{
  ESCT_hrt[,ri,] = ESCT_hrt[,1,]
}