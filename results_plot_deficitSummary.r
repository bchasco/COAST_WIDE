png("results_plot_deficitSummary.png", width = 600, height = 600, pointsize = 12)


par(mfrow=c(2,2), mai=c(1.2,1.0,0.1,0.1))

xx = apply(DefC_phjromyta[,,,,,,,,], c(9), sum)
xx = abs(xx+1)/1000000

barplot(xx,
        col=c("grey", rep("white",5)),
        border=c("grey", rep("white",5)),
        log="y",
        ylim = c(0.0001,5000),
        axes=FALSE,
        las=2,
        space=0.25,
        xlab="Ocean age",
        #names.arg = areaNamesLong[1:8],
        yaxs="i", las=1)

axis(2, at=c(0.001,1,10,100,1000), labels=c(0.001,1,10,100,1000), las=1)
axis(1, at= 1:6-0.5 + 0.25*1:6, labels=0:5, las=1)
box()
text(max(1:6-0.5 + 0.25*1:6)*0.8, 101, paste("(", letters[1], ")"))
     
mtext("Deficit (millions)", 2, line=4)

xx = apply(DefC_phjromyta[,,,,,,,,], c(1), sum)
xx = abs(xx+1)/1000000

barplot(xx,
        col=c("white", rep("grey",3)),
        border=c("white", rep("black",3)),
        log="y",
        ylim = c(0.0001,5000),
        axes=FALSE,
        las=2,
        space=0.25,
        xlab="Predator",
        #names.arg = areaNamesLong[1:8],
        yaxs="i", las=1)

axis(2, at=c(0.001,1,10,100,1000), labels=c(0.001,1,10,100,1000), las=1)
axis(1, at= 1:4-0.5 + 0.25*1:4, labels=predNames, las=2)
text(max(1:4-0.5 + 0.25*1:4)*0.8, 101,paste("(", letters[2], ")"))
box()
mtext("Deficit (millions)", 2, line=4)

xx = apply(DefC_phjromyta[,,,,,,,,], c(2,9), sum)
xx = abs(xx[1:8,1]+1)/1000000


barplot(xx,
        col=c("grey", "white", "white", rep("grey",4), rep("white",1)),
        border=c("black", "white", "white", rep("black",4), rep("white",1)),
        log="y",
        ylim = c(0.0001,5000),
        axes=FALSE,
        las=2,
        space=0.25,
        xlab="",
        #names.arg = areaNamesLong[1:8],
        yaxs="i", las=1)

axis(2, at=c(0.001,1,10,100,1000), labels=c(0.001,1,10,100,1000), las=1)
axis(1, at= 1:8-0.5 + 0.25*1:8, labels=areaNamesLong[1:8], las=2)
box()
text(max(1:8-0.5 + 0.25*1:8)*0.8, 101,paste("(", letters[3], ")"))

mtext("Deficit (millions)", 2, line=4)

xx = apply(DefC_phjromyta[,,,,,,,,], c(7,8), sum)
xx = abs(xx)/rowSums(abs(xx))

matplot(1:nYear + 1969, 
        xx, 
        type="b", 
        las=1,
        ylab="",
        xlim=c(1976,2015), 
        lty=1, 
        xlab="Year",
        ylim=c(0,0.8), 
        col="black")
mtext("Proportion of deficit", 2, line=4)
text(2008, 0.75, paste("(", letters[4], ")"))
dev.off()
