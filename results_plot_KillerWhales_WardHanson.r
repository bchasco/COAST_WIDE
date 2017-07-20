#Temporal and spatial distribution Chinook salmon stocks.
png("results_plot_KillerWhales_WardHanson.png", width = 600, height = 600, pointsize = 12)

myMonths = 6
RatioC_jy = array(0,dim=c(nAreas,nYear-5))

par(mfrow=c(3,2), mai=c(0.5,1,0.1,0.12))

myCol = rep(c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677","#AA4499"))


for(ji in 1:8){
  myS = apply(S1_hjromyta[,ji,,,,6:nYear,myMonths,3:6], c(5), sum)
  myN = apply(NC_phjromyta[,,ji,,,,6:nYear,myMonths,3:6], c(6), sum)
  RatioC_jy[ji,] = (myS/myN)
}

RatioC_jy = RatioC_jy/(RatioC_jy[5,1])
matplot(1975:2015,
        t(RatioC_jy[c(5:8),]), 
        col = myCol[5:8], 
        ylab="",
        type="l", 
        lwd=2, 
        las=1,
        xlab="",
        lty=1,
        xlim=c(1974.9,2015),
        yaxs="i",
        xaxs="i",
        cex.axis=1.3,
        cex.lab=1.3,
        axes=FALSE)
axis(1, cex.axis=1.3,
     cex.lab=1.3,
     )
axis(2,cex.lab=1.3, 
     cex.axis=1.3, las=1)
abline(v=1975)
abline(h=1)
text(2011,10,
     "(a)", cex=1.3)
mtext("Relative prey availability",2, line=4.5, cex=1.)

yl = max(apply(BC_phjromyta[1,,,,,,,,2:nAges], c(6), sum)[6:46]/1000)

barplot(apply(BC_phjromyta[1,,,,,,,,2:nAges], c(2,6), sum)[1:8,6:46]/1000, 
        ylim=c(0,yl),
        space=0,
        las=1,
        yaxs="i",
        xaxs="i",
        ylab="",
        cex.lab=1.3,
        cex.axis=1.3,
        names.arg = rep("",length(1975:2015)),
        border=myCol,
        col=myCol)
axis(1, cex.lab=1.3, 
     at=seq(1980,2010,10)-1975+0.5, labels=seq(1980,2010,10), cex.axis=1.3)
text(32,10200,
     cex=1.3,
     "(b)")
mtext("Biomass consumed (tons)",2, line=4.5, cex=1.)

for(ji in 5:8)
{
  barplot(apply(BC_phjromyta[1,,ji,,,,,,2:nAges], c(1,5), sum)[1:8,6:46]/1000, 
          space=0,
          yaxs="i",
          las=1,
          xaxs="i",
          cex.axis = 1.3,
          cex.lab=1.3,
          ylim=c(0,9500),
          names.arg = rep("",length(1975:2015)),
          border=myCol,
          col=myCol,
  )
  mtext("Biomass consumed (tons)",2, line=4.5, cex=1.)
  
  axis(1, cex.lab=1.3, 
       at=seq(1980,2010,10)-1975+0.5, labels=seq(1980,2010,10), cex.axis=1.3)
  text(39,9000,
       cex=1.3,
       paste("(",letters[ji-2],")"))
  if(ji==5){
    legend(5,8000, 
           legend=areaNames[1:4],
           bty="n",
           pch=15,
           cex=1.3,
           pt.cex=1.5,
           col=myCol[1:4])
    legend(17,8000, 
           legend=areaNames[5:8],
           bty="n",
           pch=15,
           cex=1.3,
           pt.cex=1.5,
           col=myCol[5:8])
    
  }
}

dev.off()