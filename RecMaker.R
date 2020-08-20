#Script to generate Proportional Recruitment timeseries (FEA - Aug 2020)


# IntLog function - interpolates values between pair of extremes, inputs are:
# Datin: dataframe with columns Y (Year) and R (Recruitment) over two rows (1 per extreme)
# sl:    slope of all logistics
# wmax:  weight put on the year of max recruitment - used to determine the midpoint as a weighted mean between years of extremes
# res:   temporal resolution as a fraction of years
IntLog=function(Datin,sl,wmax,yres=1){
  if(Datin$R[2]>Datin$R[1]){ #Set slope and midpoint depending on if this is low-to-high or high-to-low
    sl=sl
    MidP=(wmax*Datin$Y[2]+Datin$Y[1])/(wmax+1)
  }else{
    sl=-sl
    MidP=(wmax*Datin$Y[1]+Datin$Y[2])/(wmax+1)
  } 
  ys=seq(Datin$Y[1],Datin$Y[2],by=yres) #Create vector of years for which logistic will be computed
  Rlog=min(Datin$R)+(max(Datin$R)-min(Datin$R))/(1+exp(-sl*(ys-MidP)))
  Rlog=data.frame(Y=ys,R=Rlog)
  return(Rlog)
}


# RecMaker function - Generates time-series of annual recruitment, inputs are:
# Mins:    Range of minimum recruitment values
# Maxs:    Range of maximum recruitment values
# Periods: Range of periods (years) between recruitment events
# Ys:      Number of years to generate
# Slope:   Slope parameter for logistic functions
# wmax:    Weight to put on years of high recruitment when computing the midpoint of logistics
RecMaker=function(Mins,Maxs,Periods,Ys,Slope,wmax){
  
  #Generate a vector of years with high recruitment
  Yhi=cumsum(sample(seq(Periods[1],Periods[2]),size=ceiling(Ys/Periods[1])+1,replace = T))
  
  #Compute years of low recruitment as midpoints between years of high recruitment
  Ylo=round(Yhi[-length(Yhi)] + diff(Yhi)/2)
  
  #Generate high recruitment values
  Rhi=runif(length(Yhi),Maxs[1],Maxs[2])
  
  #Generate low recruitment values
  Rlo=runif(length(Ylo),Mins[1],Mins[2])
  
  #Put everything together in a dataframe
  Rec=data.frame(Y=c(Yhi,Ylo),R=c(Rhi,Rlo))
  Rec=dplyr::arrange(Rec,Y)
  
  #Interpolate using logistic functions between extremes
  Rs=NULL #Empty storage
  for(t in seq(1,dim(Rec)[1]-1)){
    tmpRec=Rec[t:(t+1),] #Take 2 points at a time (low to high or high to low)
    
    Rlog=IntLog(Datin=tmpRec,sl=Slope,wmax=wmax,yres=1)
    
    Rlog=Rlog[-which(Rlog$Y%in%tmpRec$Y),] #Remove extreme years that have been generated before
    
    Rlog=rbind(tmpRec,Rlog) #Merge interpolated values with generated ones
    Rlog=arrange(Rlog,Y)
    
    Rs=rbind(Rs,Rlog)
  }
  Rs=unique(Rs)
  
  #Randomly pick a starting point between the first extremes
  Start=sample(seq(Yhi[1],Ylo[1]),1)
  #Trim recruitment vector
  Rs=Rs[Rs$Y>=Start,]
  Rs=Rs$R[1:Ys]
  return(Rs)
}

#Example usage (uncomment and run. Will output a figure in your working directory)
# tiff(filename = 'RecVectors.tiff', width = 4000, height = 3000, units = "px", pointsize = 12,
#      compression = "lzw", bg = "white", res = 600)
# par(mai=c(0.5,0.5,0.1,0.1)) #c(bottom, left, top, right)
# par(xaxs='i',yaxs='i',lend=1)
# par(mfrow=c(3,3))
# 
# Ys=20
# XL=c(0,Ys)
# YL=c(0,1)
# for(i in 1:9){
#   plot(RecMaker(Mins=c(0,0.3),Maxs=c(0.7,1),Periods=c(4,7),Ys=Ys,Slope=2.5,wmax=2.4),
#        type='b',xlim=XL,ylim=YL,
#        axes=F,xlab='',ylab='',lwd=1,xpd=T,cex=0.7) 
#   axis(1,at=seq(XL[1],XL[2]),tcl=-0.3,cex.axis=0.7,padj=-1.5)
#   axis(2,pos=XL[1]-0.1,las=1,tcl=-0.3,cex.axis=0.7)
#   text(0,0.5,'Proportional recruitment',adj=c(0.5,-6),xpd=T,srt=90,cex=0.7)
#   text(Ys/2,0,'Years',adj=c(0.5,5),xpd=T,cex=0.7)
# }
# 
# dev.off()
