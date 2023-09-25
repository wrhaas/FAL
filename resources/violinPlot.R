#Produce violin plots for radiocarbon dates calibrated using Bchron. By Randy Haas 2023
library("Bchron")
violin<-function(id,age,ageSd,calCurve){
    rc<-BchronCalibrate(ages=age,ageSds=ageSd,calCurve,ids=id)#calibrate the 14C dates
    plot(x=NULL,y=NULL,"n",ylim=c(0,2),xlim=rev(range(rc[[1]]$ageGrid)),bty="n",xlab="calendar years before present",ylab=NA,yaxt="n")
    abline(v=pretty(rc[[1]]$ageGrid),col="gray",lty=2)
    abline(h=1,col="gray",lty=2)

    sf<-.4/max(rc[[i]]$densities)#scale factor
    polygon(x=c(rc[[1]]$ageGrid,rev(rc[[1]]$ageGrid)),y=c(rc[[1]]$densities*sf,-rev(rc[[1]]$densities*sf))+1,col="gray")

    mtext(side=2,text=id,at=1,las=2)
    axis(3,labels=F)
}
