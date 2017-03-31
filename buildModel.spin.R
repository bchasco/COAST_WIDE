rm(list=ls())

ls()
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
areaNames = c("Cen.CA", "N.CA/OR", "Col.Riv.", "WA", "Sal.Sea", "WVI/N.BC", "SEAK", "WAK", "Bk.Box")
areaNamesLong = c("Central \n California", "NorCal \n Oregon", "Columbia \n River", "Washington", "Salish Sea", "WCVI \n NorBC", "Southeast \n Alaska", "Western \n Alaska", "Black box")

ptm <- proc.time()

runMCChain = FALSE
if(runMCChain == TRUE )
  nSim = 50
if(runMCChain ==FALSE)
  nSim = 1

mycv = c(0.2)
MC_Ncv = 0.15
MC_SELcv = 0.5
MC_FECcv = 0.5
MC_ALPHAcv = 0.2
MC_SMTLcv = 0.3
MC_CCONDcv = 0.2 #Fish condition, change in create_EChromta

simBC_cspjya = array(NA,c(length(mycv),nSim,nPred,nAreas,nYear,nAges))
simNC_cspjya = array(NA,c(length(mycv),nSim,nPred,nAreas,nYear,nAges))
simYears = c(41:46)

cvCnt = 1
for(icv in mycv){
  for(iSim in 1:nSim)
  {
    print(paste(icv,iSim))
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
    
    simBC_cspjya[cvCnt,iSim,,,c(6,46),] = apply(BC_phjromyta[,,,,,,c(6,46),,],c(1,3,7,9),sum)  
    simNC_cspjya[cvCnt,iSim,,,c(6,46),] = apply(NC_phjromyta[,,,,,,c(6,46),,], c(1,3,7,9),sum)  
  }
  cvCnt = cvCnt + 1
}

print(proc.time() - ptm)

#Create plots for paper
#source("results_plot_RatioC.r")
#source("results_plot_deficitSummary.r")
#source("results_plot_predatorSummary.r")
#source("results_plot_killerWhalesConsumption.r")
#source("plotsalmonProduction.r")
