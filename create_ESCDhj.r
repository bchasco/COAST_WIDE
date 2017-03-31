#These are the weights given to the different areas,
#for the amount of escapement originating from those areas 

ESCD_hj = array(0,c(nAreas,nAreas))
for(hi in 1:nAreas)
{
  ESCD_hj[hi,] = 1/(1+(1:nAreas-hi)^2)
}
