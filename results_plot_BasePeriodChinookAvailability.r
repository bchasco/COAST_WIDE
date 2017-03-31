op <- par()

  png("results_plot_BasePeriodChinookAvailability.png",width = 480, height = 480, pointsize = 14)

myCol=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677","#AA4499")
myCol = rep(myCol[1:8])

meanWt = apply(exp((log(0.000011 * CLA_hromta[,,,,6,] ^ 3.12) - 7.54)/0.94), c(5), mean)[3:6]

baseNbyAge = apply(S1_hjromyta[,,,,,10:13,6,3:6],c(2,6,7),sum)
baseBbyAge = baseNbyAge
NbyAge = (apply(S1_hjromyta[,,,,,,6,3:6],c(2,6,7),sum))
BbyAge = NbyAge

for(i in 1:4){
  baseBbyAge[,,i] = baseNbyAge[,,i] * meanWt[i]
  BbyAge[,,i] = NbyAge[,,i] * meanWt[i]  
}

ratio2015 = round(t(apply(BbyAge,c(1,2),sum)/rowMeans(apply(baseBbyAge,c(1,2),sum)))[46,1:8],2)
myLabels = paste(areaNamesLong,"   (", ratio2015, ")", sep="")

matplot(1970:2015, 
        t(apply(BbyAge,c(1,2),sum)/rowMeans(apply(baseBbyAge,c(1,2),sum)))[,1:8], 
        type="n", 
        lty=1, 
        lwd=3,
        ylab="Ratio (Biomass : Base period biomass)",
        xlab="",
        las=1,
        col=myCol,
        ylim=c(0.5,3.0),
        yaxs="i",
        xaxs="i",
        xlim=c(1975,2015))
rect(1979,0,1982,4.2, 
     col="lightgrey",
     border="lightgrey")
box()
matlines(1970:2015, 
        t(apply(BbyAge,c(1,2),sum)/rowMeans(apply(baseBbyAge,c(1,2),sum)))[,1:8], 
        lty=1, 
        lwd=3,
        col=myCol)
abline(h=1,lwd=2,lty=2)
legend(1982,3, 
       legend = myLabels[order(-1*ratio2015)][1:8],
       col=myCol[order(-1*ratio2015)][1:8],
       lty=1,
       lwd=3,
       bty="n")

  dev.off()
par(op)
