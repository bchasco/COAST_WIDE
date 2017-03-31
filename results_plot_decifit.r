png("results_plot_deficit.png", width = 600, height = 600, pointsize = 12)

par(mfrow=c(2,2), mai=c(0.7,1,0.5,0.5))
col = c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677","#AA4499")

icnt = 1
for(pi in 1:4)
{
  barplot(abs(apply(DefC_phjromyta[pi,,1:8,,,,6:46,,], c(2,6), sum))/1000000,
          las=1,
          ylim=c(0,15), 
          yaxs="i", 
          xaxs="i",
          col=myCol, 
          border=myCol, 
          space=0)
  axis(1, at=1:41-0.5, 1975:2015)
  if(icnt ==1)
    legend(0,15, legend=areaNames[1:8], bty="n", pch=15, col = myCol, pt.cex = 1.2)
  text(35,14,paste("(",letters[icnt],")"))
  icnt = icnt + 1
}

par(mfrow=c(1,1), fig=c(0,1,0,1), mai=c(0,0,0,0), new=TRUE)
plot(1, type='n', xlim=c(0,1), ylim=c(0,1), xaxs="i", yaxs="i", axes=FALSE, bty="n")
text(0.02,0.5,"Deficit (millions)", srt=90)



dev.off()
