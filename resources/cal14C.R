#Radiocarbon calibration using Bchron by Randy Haas 2023
#returns most-likely date and confidence intervals for a given 14C date and calibration curve
library(Bchron)
cal14C<-function(age,sd,calCurve,id,plot=FALSE){
    cal<-BchronCalibrate(ages=age,ageSds=sd,calCurves=calCurve)#calibrate the 14C date

                                       #compute confidence intervals.
    df<-data.frame(year=cal$Date1$ageGrid,density=cal$Date1$densities)
    df$CI68<-NA
    df$CI95<-NA
    df$CI99<-NA
    df<-df[order(-df$density),]
    df$cs<-cumsum(df$density)
    df$CI68<-replace(df$density,df$cs>.68,NA)
    df$CI95<-replace(df$density,df$cs>.95,NA)
    df$CI99<-replace(df$density,df$cs>.99,NA)
    df<-df[order(df$year),]

    #produce violin plot
    plot(x=NULL,y=NULL,"n",ylim=c(0,2),xlim=rev(range(cal$Date1$ageGrid)),bty="n",xlab="calendar years before present",ylab=NA,yaxt="n")
    abline(v=pretty(cal$Date1$ageGrid),col="gray",lty=2)
    abline(h=1,col="gray",lty=2)

    sf<-.4/max(cal$Date1$densities)#scale factor
    polygon(x=c(cal$Date1$ageGrid,rev(cal$Date1$ageGrid)),y=c(cal$Date1$densities*sf,-rev(cal$Date1$densities*sf))+1,col="gray")

    mtext(side=2,text=id,at=1,las=2)
    axis(3,labels=F)

        list(
        id=id,
        date=age,
        sd=sd,
        years=cal$Date1$ageGrid,
        densities=cal$Date1$densities,
        peak=round(df$year[which(df$density==max(df$density))],0),
        CI68=paste(rev(range(df$year[!is.na(df$CI68)])),collapse="-"),
        CI95=paste(rev(range(df$year[!is.na(df$CI95)])),collapse="-"),
        CI99=paste(rev(range(df$year[!is.na(df$CI99)])),collapse="-")
    )
}
