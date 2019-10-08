## Started 8 October 2019 ##
## By Cat - based off chillfreeze brms plots ##

# with COLORS for each incr #

muplotfx <- function(modelhere, nameforfig, width, height, ylim, xlim, leg1, leg2){
  incrnum <- unique(df$incr)
  pdf(file.path(figpath, paste("", nameforfig, figpathmore, ".pdf", sep="")),
      width = width, height = height)
  par(xpd=FALSE)
  par(mar=c(5,7,3,10))
  plot(x=NULL,y=NULL, xlim=xlim, yaxt='n', ylim=ylim,
       xlab=xlab, ylab="", main=nameforfig)
  axis(2, at=1:4, labels=rev(c("Intercept", "Summer", "Fall", "Winter")), las=1)
  abline(v=0, lty=2, col="darkgrey")
  rownameshere <- c("b_Intercept", "b_asummer", "b_cfall", "b_winter")
  ppeffects <- c("b_Intercept", "b_asummer", "b_cfall", "b_winter") # or 1:4 here...
  for(i in 1:4){#i=4
    pos.y<-(4:1)[i]
    pos.x<-modoutput[(modoutput$term==rownameshere[i]),"estimate"]
    lines(modoutput[(modoutput$term==rownameshere[i]),c("lower","upper")],rep(pos.y,2),col="darkgrey")
    points(pos.x,pos.y,cex=1.5,pch=19,col="darkblue")
    for(incrsi in 1:length(incrnum)){#incrsi=4
      pos.sps.i<-which(grepl(paste("[",incrsi,"]",sep=""),mod.ranef$parameter,fixed=TRUE))
      jitt<-runif(1,0.05,0.4)
      pos.y.sps.i<-pos.y-jitt
      pos.x.sps.i<-mod.ranef[pos.sps.i[i],"mean"]
      lines(mod.ranef[pos.sps.i[i],c("25%","75%")],rep(pos.y.sps.i,2),
            col=alpha(my.pal[incrsi], alphahere))
      points(pos.x.sps.i,pos.y.sps.i,cex=0.8, col=alpha(my.pal[incrsi], alphahere))
      
    }
  }
  par(xpd=TRUE) # so I can plot legend outside
  legend(leg1, leg2, sort(unique(gsub("_", " ", df$incr))), pch=19,
         col=alpha(my.pal[1:length(incrnum)], alphahere),
         cex=1, bty="n", text.font=3)
  dev.off()
  
}
