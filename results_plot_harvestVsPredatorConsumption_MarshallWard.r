png("results_plot_harvestVsPredatorConsumption_MarhsallWard.png", width = 800, height = 800, pointsize = 14)

par(mfrow=c(3,3),mai=c(0.5,0.8,0.1,0.7))
library(viridis)

aveMat = (apply(MAT_hra[,c(1:4,9),],c(1,3), mean))
aveAgeComp = aveMat
for(i in 1:9)
{
  for(j in 2:nAges)
  {
    aveAgeComp[i,j] = aveMat[i,j] * (1-sum(aveAgeComp[i,1:(j-1)]))
  }
  aveAgeComp[,1:2] = 0
  aveAgeComp[i,] = aveAgeComp[i,]/sum(aveAgeComp[i,])
}
aveLen = apply(CLA_hromta[,c(1:4,9),,,6,], c(1,5), mean)

#Harvest
harv = read.csv("HarvestFromPMFC.csv")
harv = harv[harv[,1]>1974,]
#Table of harvested
tharv = xtabs(harv$catch~harv$Year + harv$area)

AB_hy = array(0,c(8,nYear-5))
AN_hy = array(0,c(8,nYear-5))

lm1 = mean(apply(NC_phjromyta[,,,,,,6:46,,3:6],c(7),sum)/apply(BC_phjromyta[,,,,,,6:46,,3:6],c(7),sum))


#Aggregate plot Figure 4a

  #Biomass and numbers consumed
  BC_hy = apply(BC_phjromyta[,,,,,,,,3:6],c(5,7),sum)
  BC_hy = BC_hy[,6:46]/1000000
  NC_hy = apply(NC_phjromyta[,,,,,,,,3:6],c(5,7), sum)
  NC_hy = NC_hy[,6:46]/1000000
  
  #bar plot of Numbers consumed
  barplot(NC_hy,
          space=0,
          las=1,
          xaxs="i",
          yaxs="i",
          axes=FALSE,
          ylim=c(0,4),
          ylab="",
          xlab="",
          names.arg = rep("",41),
          col=c(grey(0.85),"darkgrey"),
          border=c(grey(0.85),"darkgrey")
  )
  axis(2,las=1, cex.axis=1.3)
  
  par(new=TRUE)
  plot(1:41,
       rowSums(tharv)/1000000, 
       type="l",
       xaxs="i",
       yaxs="i",
       ylab="",
       xlab="",
       lwd=3,
       bg="transparent",
       axes=FALSE,
       ylim=c(0,4))
  
  axB = seq(0,18,3)*lm1
  axis(1, at=seq(1,45,5)-0.5, labels=seq(1,45,5)+1974, line=0, cex.axis=1.3)
  axis(4, at = axB, labels = seq(0,18,3), las=1, cex.axis=1.3)
  
  text(25,3.1,"Agg.", pos=4, cex=1.5)
  box()

#Individual areas
for(i in 1:8){
  #AB_hy[i,] = t(tharv[,i]) * 6.05
  #Biomass and numbers harvested
  AB_hy[i,] = colSums(t(tharv[,i]%o%aveAgeComp[i,]) * exp((log(0.000011 * aveLen^3.12)-7.56)/0.94)[i,])
  AN_hy[i,] = tharv[,i]/1000000

  #Biomass and numbers consumed
  BC_hy = apply(BC_phjromyta[,i,,,,,,,3:6],c(4,6),sum)
  BC_hy = BC_hy[,6:46]/1000000
  NC_hy = apply(NC_phjromyta[,i,,,,,,,3:6],c(4,6), sum)
  NC_hy = NC_hy[,6:46]/1000000

  #bar plot of Numbers consumed
  barplot(NC_hy,
          space=0,
          las=1,
          xaxs="i",
          yaxs="i",
          axes=FALSE,
          ylim=c(0,1.3),
          ylab="",
          xlab="",
          names.arg = rep("",41),
          col=c(grey(0.85),"darkgrey"),
          border=c(grey(0.85),"darkgrey")
  )
  if(i==1)
  {
    legend(2, 0.9, 
         legend=c("Fisheries", "Wild origin", "Hatchery origin"),
         lty=c(1,0,0),
         pch=c(-1,15,15),
         lwd=3,
         col=c("black", "darkgrey",grey(0.85)), 
         bty="n",
         cex=1.3,
         pt.cex = 2)
  legend(2, 0.9, 
         legend=rep("",3),
         lty=c(0,0,0),
         pch=c(-1,0,0),
         lwd=1,
         col="black", 
         bty="n",
         cex=1.3,
         pt.cex = 2)
  }
  axis(2,las=1, cex.axis=1.3)
  
  par(new=TRUE)
  plot(1:41,
       (AN_hy[i,]), 
       type="l",
       xaxs="i",
       yaxs="i",
       ylab="",
       xlab="",
       lwd=3,
       bg="transparent",
       axes=FALSE,
       ylim=c(0,1.3))

  
  axB = seq(0,6,1)*lm1
  axis(1, at=seq(1,45,5)-0.5, labels=seq(1,45,5)+1974, line=0, cex.axis=1.3)
  axis(4, at = axB, labels = seq(0,6,1), las=1, cex.axis=1.3)
  box()
  
  text(22,1.1,areaNames[i], pos=4, cex=1.5)
  #text(6,3,"Hatchery origin", pos=4, cex=1.5)
  #axis(4, at=exp(seq(1:5)*0.95+7.54)), labels=1:5,line=-0.9)
  
}

par(fig=c(0,1,0,1),mai=c(0,0,0,0), new=TRUE)
plot(1, ylim=c(0,1), xlim=c(0,1), xaxs="i", yaxs="i", xlab="", ylab="", axes=FALSE)
text(0.02,0.5,"Ocean age 2+ consumed (millions)", srt=90, cex=1.3)
text(0.98,0.5,"Biomass consumed (1000s of tons)", srt=90, cex=1.3)



dev.off()
