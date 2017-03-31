#if(makepng==TRUE)
  png("plotSalmonProduction.png", width = 480, height = 480, pointsize = 12)

library(viridis)

par(mai=c(1.0,1,0.1,0.1))

mSeq = rep(1:8,2)
mSeq2 = 1:(8*2)
mySeq3 = mSeq2[order(mSeq)]

hatchery = apply(R_hroy[1:8,,1,6:46], c(1,3), sum)
wild = apply(R_hroy[1:8,,2,6:46], c(1,3), sum)
all = rbind(hatchery,wild)
#all = all[mySeq3,]
myHue = rep(c(1,0.5),8)
myCol = rep(c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677","#AA4499"),times=2)
myCol2 = c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677","#AA4499")
#myCol = rep(viridis(8), each=2)
barplot(all/1000000,
        las=1,
        ylim=c(0,max(colSums(all)))/1000000, 
        yaxs="i", 
        xaxs="i",
        xlim=c(0,60),
        col=myCol, #rep(rainbow(9)[1:8],each=2), 
        border=myCol, #rep(rainbow(9)[1:8],each=2), 
        space=0, 
        names.arg = rep("",ncol(all)),
        density=rep(c(-1,25),each=8),
        angle=c(90))

barplot(all/1000000,
        add=TRUE,
        las=1,
        xaxs="i",
        yaxs="i",
        ylim=c(0,max(colSums(all)))/1000000, 
        col=myCol, #rep(rainbow(9)[1:8],each=2), 
        border=myCol, #rep(rainbow(9)[1:8],each=2), 
        space=0, 
        lwd=0.5,
        axes=FALSE,
        names.arg = rep("",ncol(all)),
        density=rep(c(-1,25),each=8),
        angle=c(180),
        xlim=c(0,60))

axis(1, at=seq(5,45,10)-4.5, labels = seq(1975,2015,10))

legend(40.5, max(colSums(all))/1000000, 
       legend=rep("",8), 
       col = c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677","#AA4499")[8:1],
       pch=12,
       #density = rep(50,7),
       bty="n",
       pt.cex = 2)

legend(43, max(colSums(all))/1000000, 
       areaNames[8:1], 
       col = c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677","#AA4499")[8:1],
       pch=15,
       bty="n",
       pt.cex = 2)

mtext("Smolts production (millions)", side=2, line=3)

#if(makepng==TRUE)
  dev.off()
