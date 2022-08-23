# Make power spectrum graphs from Praat data.
# The function aggregates samples into bins of frequency (default is 25hz wide)

library(ggplot2)
library(ggpubr)

# Set the working directory for Sean's computer
try(setwd("~/OneDrive - Cardiff University/Supervision/UGDissertations/Havill/experiment/analysis/"))

# Function to summarise one data file
summariseData = function(d,upperRange = 8000, samplingWindow = 25){
  # Cut data above upper range
  d = d[d$freq.Hz.<=upperRange,]
  
  sampleBoundaries = seq(0,upperRange,by=samplingWindow)
  sampleMidpoints = sampleBoundaries + samplingWindow/2
  
  d$sampleBin = cut(d$freq.Hz., sampleBoundaries)
  
  d.win = data.frame(
    freq.Hz. = sampleMidpoints[1:(length(sampleMidpoints))-1],
    pow.dB.Hz. = tapply(d$pow.dB.Hz.,d$sampleBin,mean)
  )
  return(d.win)
}


# JND
# https://www.sciencedirect.com/science/article/pii/B9780123982582000039

jndHz = c(35,70,200,1000,4000,8000,10000)
# at 5db
jndDB = c(9.3,5.7,4.7,3.0,2.5,4.0,4.7)
# at 10db
jndDB10db = c(7.8, 4.2, 3.4, 2.3, 1.7, 2.8, 3.3)
# TODO: DB is higher at lower freq, so adjust?
#  Or just use 5db, since it's more conservative
jndSpline = smooth.spline(jndHz,jndDB,df = 6,all.knots = T)

makePowerDifferenceGraph = function(maskedFile, unmaskedFile,mainTitle="",plotXLAB=TRUE,plotYLAB=TRUE){
  # Load and summarise data
  d.masked = summariseData(read.csv(maskedFile))
  d.unmasked = summariseData(read.csv(unmaskedFile))
  
  # Plot raw power
  plot(d.unmasked$freq.Hz., d.unmasked$pow.dB.Hz., type='l', 
       xlab="Frequency (Hz)", ylab="Power (dB)",
       main=mainTitle, ylim=c(-32,35))
  points(d.masked$freq.Hz., d.masked$pow.dB.Hz., type='l', col='2')
  legend(5000,32,legend=c("Unmasked","Masked"),col=1:2,lty=1)
  
  # Plot difference in power
  d.masked$diff = d.masked$pow.dB.Hz. - d.unmasked$pow.dB.Hz.
  
  d.masked$dbMax = predict(jndSpline, x = d.masked$freq.Hz.)$y
  d.masked$dbMin = - d.masked$dbMax
  
  plot(d.masked$freq.Hz., d.masked$diff, type='l',
       xlab="Frequency (Hz)", ylab= "Difference in power (dB)",
       main = mainTitle, ylim=c(-25,10))
  abline(h=0,col=rgb(0,0,0,0.4))
  #points(jndHzFullScale, jndDBFullScale,type = "l",col='light gray')
  #points(jndHzFullScale, -jndDBFullScale,type = "l",col='light gray')
  
  gx = ggplot(d.masked, aes(x=freq.Hz., y = diff)) + 
    geom_ribbon(aes(x=freq.Hz., ymin=dbMin, ymax=dbMax),
                alpha = 0.3) +
    geom_hline(yintercept = 0,colour="dark gray") +
    geom_line(alpha=0.9) +
    ylim(c(-24,10))
  
  if(plotXLAB){
    gx = gx + xlab("Frequency (Hz)")
  } else{
    gx = gx + theme(axis.title.x = element_blank())
  }
  if(plotYLAB){
    gx = gx + ylab("PD (db)")
  } else{
    gx = gx + theme(axis.title.y = element_blank())
  }
  
  return(gx)

}

# Apply the functions to all the files

#pdf("PowerAnalyses.pdf", width=6, height=5)
dx = summariseData(read.csv("Data/Masked_D.Table.csv"))
dx$dbMax = predict(jndSpline, x = dx$freq.Hz.)$y
dx$dbMin = - dx$dbMax

guideGraph =   ggplot(dx,aes(x = freq.Hz., y=20)) +
  geom_ribbon(aes(x=freq.Hz., ymin=dbMin, ymax=dbMax),
              alpha = 0.3) +
  #geom_hline(yintercept = 0) +
  geom_line(alpha=0.9) +
  coord_cartesian(ylim=c(-24,10)) +
  ylab("PD (db)") + theme(axis.title.x = element_blank()) +
  annotate("text",x = 500,y=8,label="Fascemask increases intensity",hjust="left") +
  annotate("text",x = 500, y = -10, label="Fascemask decreases intensity",hjust="left") +
  annotate("text",x = 500, y = 0, label="No Noticable Difference",colour="#525252",hjust="left")

gD = makePowerDifferenceGraph("../data/AcousticAnalysis/Masked_D.Table.csv", "../data/AcousticAnalysis/Unmasked_D.Table.csv", "/d/",F,F) +
  annotate("text", x = 500,y=-15,label="/d/",size =8)
gT = makePowerDifferenceGraph("../data/AcousticAnalysis/Masked_T.Table.csv", "../data/AcousticAnalysis/Unmasked_T.Table.csv", "/t/",F,T) +
  annotate("text", x = 500,y=-15,label="/t/",size =8)
gZ = makePowerDifferenceGraph("../data/AcousticAnalysis/Masked_Z.Table.csv", "../data/AcousticAnalysis/Unmasked_Z.Table.csv", "/z/",F,F) +
  annotate("text", x = 500,y=-15,label="/z/",size =8)
gS = makePowerDifferenceGraph("../data/AcousticAnalysis/Masked_S.Table.csv", "../data/AcousticAnalysis/Unmasked_S.Table.csv", "/s/",T,T) +
  annotate("text", x = 500,y=-15,label="/s/",size =8)
gSH = makePowerDifferenceGraph("../data/AcousticAnalysis/Masked_SH.Table.csv", "../data/AcousticAnalysis/Unmasked_SH.Table.csv", "/sh/",T,F) +
  annotate("text", x = 500,y=-15,label="/ʃ/",size =8)
#dev.off()

gx = ggarrange(guideGraph, gD, gT, gZ, gS, gSH,
          nrow = 3,ncol=2,heights = c(1,1,1.2))

gx

grDevices::cairo_pdf("../results/AcousticAnalysis.pdf",width=6,height = 6)
gx
dev.off()
