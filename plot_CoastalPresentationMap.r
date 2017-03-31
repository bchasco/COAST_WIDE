library(maps)
library(mapdata)
makeMap = TRUE

op <- par()

  png("plotCoastalPresentationMap.png", height=480, width=480)
if(makeMap==TRUE)
{
  x = read.table("areaCoords.dat", header=TRUE)
  GoA = x[x$Area=="GoA",]
  x = x[x$Area!="GoA",]
  library(maps)
  library(mapproj)
  library(mapdata)
  
#png("CoastalPresentationMap.png", width=720, height=720)
areasCol = viridis(8)#c("brown", "red", "green", "darkblue", "darkgreen", "pink", "yellow", "orange", "grey")
areasCol=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677","#AA4499")
for(y in 6:6)
{
  for(tt in 7:7)
  {
    par(mai=c(0,0,0,0), fig=c(0.,1,0,1), bg="transparent")
    map('world', 'usa', xlim=c(-165,-120), ylim=c(35,62), xaxs="i", yaxs="i", fill=TRUE, col="lightgrey", mar=c(0,0,0,0))
    
    #polygon(GoA[,2:3], col="black")  
    icnt = 1
    for(i in 1:8)
    {
        polygon(x[x$Area==LETTERS[icnt],2:3], 
                col=areasCol[icnt],#areasCol[icnt], 
                border=areasCol[icnt],)#border = areasCol[icnt])  
        icnt = icnt + 1
    }
    polygon(x[x$Area=="C",2:3], col=3)  

    map('world', 'usa', border="grey", xlim=c(-160,-120), ylim=c(30,75), xaxs="i", yaxs="i", fill=TRUE, add=TRUE, col="lightgrey")
    
    map('world', 'canada', add=TRUE, border="grey", xlim=c(-150,-110), ylim=c(30,75), xaxs="i", yaxs="i", col="lightgrey", fill=TRUE)
    
    
    map('state', region=c('washington'), border="grey", add=TRUE, xlim=c(-150,-120), ylim=c(30,65), fill=TRUE, col=c("lightgrey"), xaxs="i", yaxs="i")
    map('state', region=c('oregon'), border = "grey", add=TRUE, xlim=c(-150,-120), ylim=c(30,65), fill=TRUE, col=c("lightgrey"), xaxs="i", yaxs="i")
    map('state', region=c('california'), border = 0, add=TRUE, xlim=c(-150,-120), ylim=c(30,65), fill=TRUE, col=c("lightgrey"), xaxs="i", yaxs="i")

    icnt = 1
    latx = c(-124,-126,-123,-127,-123,-129,-137,-142,-150)
    longy = c(37,42,46.4,46.4,49,51,57,57,57)
    for(i in c(1:7,9))
    {
        text(latx[i],longy[i], levels(x$Area)[i],cex=1.5)
        icnt = icnt + 1
    }
  
    icnt = 1
    par(mgp=c(0,0,0))

    par(fig=c(0,1,0,1), bg="lightgrey",new=TRUE)
    #plot(0,1,type="n", bg="lightgrey",ylim=c(0,1), xlim=c(0,1), xaxs="i", yaxs="i")
    #text(tt/length(tNames) - 1/length(tNames)*0.5,0.95,tNames[tt], cex=1.5)
    #text(0.1,0.9,paste('Year',y), cex=1.5)
  }
  
}
  dev.off()
}

par(op)

