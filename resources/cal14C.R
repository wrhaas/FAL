#Radiocarbon calibration using Bchron by Randy Haas 2023
#returns most-likely date and confidence intervals for a given 14C date and calibration curve
library(Bchron)
cal14C<-function(age,sd,calCurve,id,plot=TRUE){
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

    #plotting
    if(plot==TRUE){
        par(mar=c(4,4,1,3))
        plot(0,xlim=rev(range(cal$Date1$ageGrid)),ylim=range(cal$Date1$densities),xlab="calendar year BP",ylab="density",bty="n")
        abline(h=min(df$density[!is.na(df$CI68)]),col="gray",lty=2)
        abline(h=min(df$density[!is.na(df$CI95)]),col="gray",lty=2)
        abline(h=min(df$density[!is.na(df$CI99)]),col="gray",lty=2)
        mtext(text=c("68%","95%","99%"),side=4,line=.5,las=2,at=c(min(df$density[!is.na(df$CI68)]),min(df$density[!is.na(df$CI95)]),min(df$density[!is.na(df$CI99)])))
        polygon(x=c(min(cal$Date1$ageGrid),cal$Date1$ageGrid,max(cal$Date1$ageGrid)),y=c(0,cal$Date1$densities,0),col="gray")
    }
    
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

