#' @title makeMarkdown
#' @description create markdown plots for movement metrics
#' @param id_df data.frame of lookup table
#' @param rollmean movement data product from the moveMetrics function
#' @param subsetmonth month to start plotting
#' @param tempdir temporary directory for storing products
#' @return returns plots for movement metrics in the temporary directory
#' @details 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname makeMarkdown
#' @export 
makeMarkdown<-function(id_df, rollmean, subsetmonth, tempdir){
  
  plotdir<-paste0(tempdir, "/", "Plots")
  if(dir.exists(plotdir)){
    fls = dir(plotdir, full.names = TRUE, recursive = TRUE, include.dirs = TRUE)
    unlink(fls, force=TRUE, recursive = TRUE)
  }else{dir.create(plotdir)}
  
  
  rollmean<-rollmean[complete.cases(rollmean$t_),]
  uni<-unique(rollmean$id)
  
  tim<-paste(strftime(Sys.time(),format='%Y'),'-', subsetmonth, '-01',sep='')
  rollmean<-rollmean[rollmean$t_ >= tim,]
  
  for(k in 1:length(uni)){
    sub<-rollmean[rollmean$id == uni[k],]
    sub<-sub[order(sub$t_),]
    
  
    #bring in frequency for report
      fn <- id_df[id_df$AID == uni[k],]$Frequency
      fn <- gsub(".", "", fn, fixed = T)
      if(nchar(fn)< 6){
        fn<-paste0(fn,paste0(rep('0',6-nchar(fn)),collapse=''))
      }
      fn <- paste(plotdir, fn, sep = "/")
      fn <- paste(fn, "png", sep = ".")
      png(filename = fn, height = 1400, width = 1500, res = 75)
      par(mfrow = c(2, 2))
      
      
      
      # MOVEMENT RATE PLOTS
      
      plot(sub$t_, sub$moverate_kmhr, type = "l", ylab = "Movement rate (km/hr)",
           xlab = "Date", main = "Movement Rate", cex = 1.25)
      
      tdiff<-round(48/max(sub$timediff, na.rm = T))
      if(nrow(sub)>tdiff){
        mm <- quantile(sub[(nrow(sub) - tdiff):(nrow(sub)),]$moverate_kmhr,
                       probs = 0.75, na.rm = T)
        
      }else{ #'total quantile
        mm <- quantile(sub$moverate_kmhr,
                       probs = 0.75)
      }
     
      
      lines(sub$t_, sub$mr3_mean, col = "red") # rolling quantile
      abline(h = mm, col = "blue", lty = 2)
      
      
      
      
      
      # Home Range Plots--5 interval
      y1<-expression(Home ~ range ~ size ~ (km^2))
      plot(sub$t_, sub$HR, type = "l", ylab = y1,
           xlab = "Date", main = "Home Range Size", cex = 1.25)
      
      tdiff<-round(48/max(sub$timediff, na.rm = T))
      if(nrow(sub)>tdiff){
        mm <- quantile(sub[(nrow(sub) - tdiff):(nrow(sub)),]$HR,
                       probs = 0.75, na.rm = T)
        
      }else{ #'total quantile
        mm <- quantile(sub$HR,
                       probs = 0.75)
      }
      
      
      lines(sub$t_, sub$hr3_mean, col = "red") # rolling quantile
      abline(h = mm, col = "blue", lty = 2)
      
      
      
      
      
      # Residence Time
      plot(sub$t_, sub$RT, type = "l", ylab = "Residence time (100 meters)",
           xlab = "Date", main = "Residence time", cex = 1.25)
      
      tdiff<-round(48/max(sub$timediff, na.rm = T))
      if(nrow(sub)>tdiff){
        mm <- quantile(sub[(nrow(sub) - tdiff):(nrow(sub)),]$RT,
                       probs = 0.75, na.rm = T)
        
      }else{ #'total quantile
        mm <- quantile(sub$RT,
                       probs = 0.75)
      }
      
      
      lines(sub$t_, sub$rt3_mean, col = "red") # rolling quantile
      abline(h = mm, col = "blue", lty = 2)
      
      
      
      
      
      
      # Visitation
      plot(sub$t_, sub$visits, type = "l", ylab = "Number of visits (100 meters)",
           xlab = "Date", main = "Number of visits", cex = 1.25)
      
      tdiff<-round(48/max(sub$timediff, na.rm = T))
      if(nrow(sub)>tdiff){
        mm <- quantile(sub[(nrow(sub) - tdiff):(nrow(sub)),]$visits,
                       probs = 0.75, na.rm = T)
        
      }else{ #'total quantile
        mm <- quantile(sub$visits,
                       probs = 0.75)
      }
      
      
      lines(sub$t_, sub$vis3_mean, col = "red") # rolling quantile
      abline(h = mm, col = "blue", lty = 2)
      
      
      
      
      
     
      mf <- paste(paste("Ewe Frequency: ", id_df[id_df$AID == uni[k],]$Frequency,
                          sep = " "),paste('AID: ',id_df[id_df$AID == uni[k],]$AID,sep=''),sep=' ')
     
      
      mtext(mf, font = 2, side = 3, line = -2.25, outer = T,
            cex = 2)
      
     
    
      
     
        vf <- paste("Tag Number:", id_df[id_df$AID == uni[k],]$VitFreq, sep = " ")
        mtext(vf, font = 2, side = 3, line = -116, outer = T,
              cex = 2)
        
        dev.off()
        
        print(k)
      
  }
  
 
}
      
