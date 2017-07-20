load("simOutput.Rdata", verbose = FALSE)

png("results_plot_sensitivity.png", height=680,width=680)

par(mfrow=c(2,2), mai=c(0.7,0.9,0.1,0.9))
nr = length(na.omit(simBC_cspjya[1,,pi,1,1,1]))

for(pi in 1:4)
{
  bx = (apply(simBC_cspjya[1,1:nr,pi,,,],c(1,3),sum)/1000000)
  nx = (apply(simNC_cspjya[1,1:nr,pi,,,],c(1,3),sum)/1000000)
  
  qbx = t(apply(bx,2,function(xx){return(quantile(xx,probs=c(0.025,0.5,0.975)))}))
  qnx = t(apply(nx,2,function(xx){return(quantile(xx,probs=c(0.025,0.5,0.975)))}))
  
  matplot(1975:2015,
          qbx[6:46,], 
          #ylim=c(0,30),
          type="l",
          lty=1,
          ylab="",
          xlab="",
          xaxs="i",
          yaxs="i",
          axes=FALSE,
          lwd=c(1,4,1),
          col=c(rgb(0,0,1,0.5),"blue",rgb(0,0,1,0.5))
          
  )
  polygon(c(1975:2015,2015:1975),
          c(qbx[6:46,1],qbx[46:6,3]),
          col=rgb(0,0,1,0.5),
          border=FALSE)
  
  axis(2,
       cex.axis=1.5,
       las=1)
  par(new=TRUE)
  matplot(1975:2015,
          qnx[6:46,],
          type="l",
          lty=1,
          xaxs="i",
          yaxs="i",
          ylab="",
          xlab="",
          lwd=c(1,4,1),
          col=c(rgb(1,0,0,0.5),"darkred",rgb(1,0,0,0.5)),
          axes=FALSE)
  text(1980,max(qnx[6:46,])*0.9,paste("(",letters[pi],")"),
       cex=1.5)  
  polygon(c(1975:2015,2015:1975),
          c(qnx[6:46,1],qnx[46:6,3]),
          col=rgb(1,0,0,0.5),
          border=FALSE)
  axis(4,         
       cex.axis=1.5,
       las=1)
  axis(1,
       cex.axis=1.5
)
  
  if(pi==2)
    legend(1980,max(qnx[6:46,])*0.9,
           legend=c("Biomass", "Numbers"),
           col=c(rgb(0,0,1,0.5),rgb(1,0,0,0.5)),
           pch=15,
           bty="n",
           pt.cex=2.5,
           cex=1.8)
  box()
  
}
par(new=TRUE, mfrow=c(1,1))
plot(1, type="n",
     xlab="",
     ylab="",
     axes=FALSE)

mtext("Biomass consumed (thousand of tons)",2, cex=1.5,line=3)
mtext("Numbers consumed (millions)",4, cex=1.5, line=3)

dev.off()
