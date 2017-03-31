
#recruitment occurs in the first time-step within a year
recruits = array(0,c(nRun,nAreas,nTime,nYear))
for(y in 1:5)
  for(r in 1:nRun)
    for(t in 1:1)
      recruits[r,1:(nAreas-1),t,y] = rep(1000,nAreas-1)
    
    #Chinook movement matrices
    eJuvMat = array(0,c(nTime,nAreas,nAreas))
    eAdMat = array(0,c(nTime,nAreas,nAreas))
    
    #emigration matrix
    eMat = array(0,c(nTime,nAreas,nAreas))
    #Jan start
    for(t in 1:5)
      diag(eJuvMat[t,,]) = c(rep(0.5,nAreas-1),0)
    for(t in 4:7)
      diag(eJuvMat[t,,]) = c(rep(0.9,nAreas-1),0)
    for(t in 8:12)
      diag(eJuvMat[t,,]) = c(rep(1,nAreas-1),0)
    
    #Apr start
    eAdMat = array(0,c(nTime,nAreas,nAreas))
    for(t in 1:3)
      eAdMat[t,,][upper.tri(eAdMat[t,,])] = 0.5 
    for(t in 4:4)
      eAdMat[t,,][upper.tri(eAdMat[t,,])] = 0.9 
    for(t in 5:12)
      eAdMat[t,,][upper.tri(eAdMat[t,,])] = 1 
    
    #Age composition of the returns
    ageComp = c(0,0.05,0.5,0.85,1.0)
    
    #evenly distribute the emigrants so the immigrate to the different cells
    #but fish only travel north, thus the (i+1):nAreas
    iMat = matrix(0,nAreas,nAreas)
    iAdMat = matrix(0,nAreas,nAreas)
    for(i in 1:(nAreas-1))
    {
      iMat[i,(i+1):nAreas] = 1/length((i+1):nAreas)
      iAdMat[nAreas-i+1,1:(nAreas-i)] = 1/length(i:(nAreas-1))
    }
    
    maxPopInArea = 0
    
    for(y in 1:5)
    {
      for(r in 1:nRun)
      {
        #time 1
        #recruitment
        for(a in 1:nAreas)
        {
          spop[r,age=1,a,a,t=1,y] = recruits[r,a,t=1,y]*exp(rnorm(1,0,rec_Cv))
          
          if(max(spop[r,,a,a,,y])>maxPopInArea) 
            maxPopInArea = max(spop[r,,a,a,,y])
        }
        
        #age the salmon by one year
        if(y>1)
          for(age in 2:nAges)
          {
            spop[r,age,,,t=1,y] = spop[r,age-1,,,t=12,y-1]
          }
        
        #these are the salmon migrating this year
        stmp = array(0,
                     c(nRun,nAges,nAreas,nAreas), 
                     dimnames = list(paste0('run',1:nRun),paste0("age",1:nAges),paste0("Area",1:nAreas),paste0("nextArea",1:nAreas)))
        
        #store in a temporary array the number of fish of each area that are migrating back home based on the age composition of the 
        #area and run in this year
        for(age in 2:nAges)
          stmp[r,age,,] = spop[r,age,,,1,y] * ageComp[age] 
      }  
      #time 2+
      for(t in 2:(nTime))
      {
        for(r in 1:nRun)
        {
          #The age 1 only travel north
          #determine the amount of fish leaving all of the areas
          em[r,age=1,,,t,y] = spop[r,age=1,,,t-1,y] %*% eJuvMat[t,,] 

          #determine where those emigrants go
          im[r,age=1,,,t,y] = em[r,age=1,,,t,y] %*% iMat
          
          #All of the other ones travel south
          #determine the amount of fish leaving all of the areas
          for(age in 2:nAges)
          {
            #The number of fish leaving each area, based on the area they are going to
            em[r,age,,,t,y] = stmp[r,age,,] * eAdMat[t,,] 
            
            #Now you have to distribute where the emigrants are imigrating to
            for(ii in 1:nAreas)
              for(jj in 2:nAreas)
              {
                if(jj>ii)
                {   	
                  #The immigrants accumulate
                  im[r,age,ii,ii:(jj-1),t,y] = im[r,age,ii,ii:(jj-1),t,y] + em[r,age,ii,jj,t,y] * (1/length(ii:(jj-1)))
                }
              }
          }
          
          #update the population with the emigration
          for(age in 1:nAges)
          {
            #subtract from the area population the emigrants, and add to it the imigrants
            spop[r,age,,,t,y] = spop[r,age,,,t-1,y] - em[r,age,,,t,y] +  im[r,age,,,t,y]
            stmp[r,age,,] = stmp[r,age,,] - em[r,age,,,t,y] + im[r,age,,,t,y]
          }
          
          #add recruitment at each time step
          for(i in 1:nAreas)
            spop[r,age=1,i,i,t,y] = spop[r,age=1,i,i,t,y] + recruits[r,i,t,y]     
        }#end run
        
        #determine the number of predators in each area
        
      }#end time
      
      #remove the individuals older than age 1 to the spawning population
      for(i in 1:nAreas)
      {
        if(i<nAreas)
          spawners[r,2:nAges,i,y] = spop[r,2:nAges,i,i,12,y] #all other areas only have adult spawners
        if(i==nAreas)  #The last area is a mixture of those that stay and those that spawner
          spawners[r,2:nAges,i,y] = spop[r,2:nAges,i,i,12,y] * ageComp[2:nAges]
        
        if(y<nYear & y>nAges)
          recruits[r,i,t=1,y+1] =  alpha * sum(spawners[r,2:nAges,i,y]) * exp(-beta * sum(spawners[r,2:nAges,i,y]))
        
        spop[r,2:nAges,i,i,12,y] = spop[r,2:nAges,i,i,12,y] - spawners[r,2:nAges,i,y]
      }#end area
      
    }#end year
    
    
    