rm(list=ls())

makepng = FALSE

#Dimensions of the model
nAreas = 9 #Number of areas
nTime = 12 #Number of time steps - 12 months
nDays = c(31,28.25,31,30,31,30,31,31,30,31,30,31) #Number of days in each time-step (i.e., month)
nYear = 46 #Number of years in the study
nAges = 6 #the number of ages
nOrigin = 2 #Hatchery or wild 
nRun = 9 #Spring, Summer, Fall, Winter, 9 is the junk category with mixed runs and unknowns
nPred = 4 #The number of predators
nSex = 2 #1=Male, 2=Female
predAges = 25

#How many time-step are smolts allowed to stay in natal areas
smoltLAG = 2


#Names of the array dimensions
tNames = c('May', 'June', 'July', 'Aug', 'Sept', 'Oct', 'Nov', 'Dec', 'Jan', 'Feb', 'Mar','Apr')
originNames = c("Hatchery", "Wild")
runNames = c("Spring","Summmer","Fall","Winter","","","","","Other")
predNames = c("KW", "HS", "CSL", "SSL")
predNamesLong = c("Resident \n killer whales",'Harbor seals (000)', 'California \n sea lions (000)', 'Steller \n sea lions (000)')
sexNames = c("Male","Female")
areaNames = c("Cen.CA", "N.CA/OR", "Col.Riv.", "WA", "Sal.Sea", "WVI/N.BC", "SEAK", "GoA", "Bk.Box")
areaNamesLong = c("Central California", "NorCal/Oregon", "Columbia River", "Washington Coast", "Salish Sea", "WCVI/NorBC", "Southeast Alaska", "Gulf of Alaska", "Black box")

ptm <- proc.time()

runMCChain = TRUE
if(runMCChain == TRUE )
  nSim = 100
if(runMCChain ==FALSE)
  nSim = 1

mycv= 1 #deprecated
cvCnt = 1 #deprecated

MC_Ncv = 0.1 #Based on Eric's best guess
MC_SELcv = 0.5  #No they all very by the same cv
MC_FECcv = 0.5 #Simply a large cv to represent our uncertainty in the fraction of Chinook in the diets
MC_ALPHAcv = 0.05 #Based on Noren
MC_SMTLcv = 0.1 #Based on Weitkamp.
MC_CCONDcv = 0.1 #Fish condition, change in create_EChromta, rough estimate based Sandra Oneill figure 6.

simBC_cspjya = array(NA,c(length(mycv),nSim,nPred,nAreas,nYear,nAges))
simNC_cspjya = array(NA,c(length(mycv),nSim,nPred,nAreas,nYear,nAges))
simYears = c(1:46)

for(iSim in 1:nSim)
{
  print(paste(iSim))
  #Create the predator abundance by sex and age
  source("Create_Nphyis.r") #Abundance
  
  source("Create_Mphis.r") #Mass
  source("Create_ALPHApis.r") #KleiberMultiplier
  
  source("Create_EPphis.r") #Energetic demands of predator by weight
  
  #Read in the smolt timing and size data
  source("create_FRAM_MAThra.r") #The conditional probability of maturing-at-age a
  source("Create_SELpjta_basedOnmaturationSchedule.r") #Selectivity of different age classes
  
  source("Create_FECpjt.r") #Fraction from Chinook energy based on diets.
  source("Create_PHIphjts.r") #Spatial and temporal distribution
  source("Create_EDphjytias.r") #Total energetics demands of the predators by home area, area, time, predator age, age, and sex.
  
  source("create_ESCThrt.r")  #Create the escapement timing
  source("create_ESCDhj.r")  #Create the fraction of the escapement coming from the different areas
  
  source("create_THETAhjromta.r") #Chinook salmon distribution model
  source("create_Rhroy.r") #Smolt abundance in each year
  source("create_SMThryt.r") #Smolt timing
  source("create_SMTLhryt.r") #Smolt lengths
  source("create_CLAhromta.r") #Chinook length at age based on the FRAM model input
  source("create_EChromta.r") #Chinook energy model
  source("create_NCphjromyta.r") #Estimate the number of salmon consumption
  
  simBC_cspjya[cvCnt,iSim,,,,] = apply(BC_phjromyta[,,,,,,,,],c(1,3,7,9),sum)  
  simNC_cspjya[cvCnt,iSim,,,,] = apply(NC_phjromyta[,,,,,,,,], c(1,3,7,9),sum)  
}

print(proc.time() - ptm)

if(runMCChain==TRUE)
  save(simBC_cspjya,simNC_cspjya, file="simOutput.RData")

