#' @title runUpdate
#' @description wrapper function for running sheep update and emailing
#' @param id_df data.frame of lookup information
#' @param tempdir temporary directory to save files
#' @param veckeys directory to vectronics key files
#' @param telonic_usrs vector of telonic usernames
#' @param telonic_pass vector of telonic passwords. The order of passwords must match that of the usernames
#' @param ATS_usrs vector of ATS usernames
#' @param ATS_pass vector of ATS passwords. The order of passwords must match that of the usernames
#' @param lotek_usrs vector of Lotek usernames
#' @param lotek_pass vector of Lotek passwords. The order of passwords must match that of the usernames
#' @param tzone time zone of vectronics data if applicable. Options are "America/Los_Angeles" or "America/Denver". Default is "America/Los_Angeles"
#' @param subsetmonth beginning month to subset GPS data to. Default is 2 (February)
#' @param study Name of study area. Only used for subject in update email
#' @param markdowndir directory to the Rmd file for the report
#' @param spp character vector of species of interest. Options include BHS, MD, ELK, MOOSE
#' @return final product is email with deliverables
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[happybirthday]{getData}}, \code{\link[happybirthday]{makeMaps}}, \code{\link[happybirthday]{moveMetrics}}, \code{\link[happybirthday]{makeMarkdown}}, \code{\link[happybirthday]{renderMarkdown}}
#'  \code{\link[mailR]{send.mail}}
#' @rdname runUpdate
#' @export 
#' @importFrom mailR send.mail
runUpdate<-function(id_df, tempdir, veckeys = NA,telonic_usrs = NA, telonic_pass = NA, ATS_usrs = NA, ATS_pass = NA, lotek_usrs = NA, lotek_pass = NA, tzone = 'America/Los_Angeles', subsetmonth = "02", study = NA, markdowndir = NA, spp = NA){
 
  
  x<-happybirthday::getData(id_df = id_df, tempdir = tempdir, veckeys = veckeys, telonic_usrs = telonic_usrs, telonic_pass = telonic_pass, ATS_usrs = ATS_usrs, ATS_pass =ATS_pass, lotek_usrs= lotek_usrs, lotek_pass = lotek_pass, tzone = tzone, subsetmonth = subsetmonth)
  
  # make maps
  happybirthday::makeMaps(tempdir = tempdir, gpsdat = x, id_df = id_df)
  
  # create movement metrics
  move<-happybirthday::moveMetrics(gpsdat = x, subsetmonth = subsetmonth)
  
  rollmean = move
  
  makeMarkdown<-function(id_df, rollmean, subsetmonth, tempdir, envir = parent.frame(), spp){
    ls(envir = envir)
    require(zoo)
    require(dplyr)
    
    plotdir<-paste0(tempdir, "/", "Plots")
    if(dir.exists(plotdir)){
      fls = dir(plotdir, full.names = TRUE, recursive = TRUE, include.dirs = TRUE)
      unlink(fls, force=TRUE, recursive = TRUE)
    }else{dir.create(plotdir)}
    
    
    tim<-paste(strftime(Sys.time(),format='%Y'),'-', subsetmonth, '-01',sep='')
    rollmean<-rollmean[rollmean$t_ >= tim,]
    
    rollmean<-rollmean[complete.cases(rollmean$t_),]
    uni<-unique(rollmean$id)
    
    
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
      par(mfrow = c(3, 2))
      
      
      
      # MOVEMENT RATE PLOTS
      
      plot(sub$t_, sub$moverate_kmhr, type = "l", ylab = "Movement rate (km/hr)",
           xlab = "Date", main = "Movement Rate", cex = 1.25)
      
      tdiff<-round(24/max(sub$timediff, na.rm = T))
      if(nrow(sub)>tdiff){
        mm <- quantile(sub[(nrow(sub) - tdiff):(nrow(sub)),]$moverate_kmhr,
                       probs = 0.75, na.rm = T)
        
      }else{ #'total quantile
        mm <- quantile(sub$moverate_kmhr,
                       probs = 0.75)
      }
      
      
      lines(sub$t_, sub$mr24_mean, col = "red") # rolling quantile
      abline(h = mm, col = "blue", lty = 2)
      
      
      
      
      
      # Home Range Plots--
      y1<-expression(Home ~ range ~ size ~ (km^2))
      plot(sub$t_, sub$HR, type = "l", ylab = y1,
           xlab = "Date", main = paste0("Home Range Size (", sub$Increment[1], ")"), cex = 1.25)
      
      tdiff<-round(24/max(sub$timediff, na.rm = T))
      if(nrow(sub)>tdiff){
        mm <- quantile(sub[(nrow(sub) - tdiff):(nrow(sub)),]$HR,
                       probs = 0.75, na.rm = T)
        
      }else{ #'total quantile
        mm <- quantile(sub$HR,
                       probs = 0.75)
      }
      
      
      lines(sub$t_, sub$hr24_mean, col = "red") # rolling quantile
      abline(h = mm, col = "blue", lty = 2)
      
      
      
      
      
      # Residence Time 100 meters
      plot(sub$t_, sub$RT_100, type = "l", ylab = "Residence time (hours)",
           xlab = "Date", main = "Residence time at 100 meters", cex = 1.25)
      
      tdiff<-round(24/max(sub$timediff, na.rm = T))
      if(nrow(sub)>tdiff){
        mm <- quantile(sub[(nrow(sub) - tdiff):(nrow(sub)),]$RT_100,
                       probs = 0.75, na.rm = T)
        
      }else{ #'total quantile
        mm <- quantile(sub$RT_100,
                       probs = 0.75)
      }
      
      
      lines(sub$t_, sub$rt24_mean_100, col = "red") # rolling quantile
      abline(h = mm, col = "blue", lty = 2)
      
      
      # Residence Time 500 meters
      plot(sub$t_, sub$RT_500, type = "l", ylab = "Residence time (hours)",
           xlab = "Date", main = "Residence time at 500 meters", cex = 1.25)
      
      tdiff<-round(24/max(sub$timediff, na.rm = T))
      if(nrow(sub)>tdiff){
        mm <- quantile(sub[(nrow(sub) - tdiff):(nrow(sub)),]$RT_500,
                       probs = 0.75, na.rm = T)
        
      }else{ #'total quantile
        mm <- quantile(sub$RT_500,
                       probs = 0.75)
      }
      
      
      lines(sub$t_, sub$rt24_mean_500, col = "red") # rolling quantile
      abline(h = mm, col = "blue", lty = 2)
      
      
      # Visitation 100 meters
      plot(sub$t_, sub$Vis_100, type = "l", ylab = "Number of visits (100 meters)",
           xlab = "Date", main = "Number of visits", cex = 1.25)
      
      tdiff<-round(24/max(sub$timediff, na.rm = T))
      if(nrow(sub)>tdiff){
        mm <- quantile(sub[(nrow(sub) - tdiff):(nrow(sub)),]$Vis_100,
                       probs = 0.75, na.rm = T)
        
      }else{ #'total quantile
        mm <- quantile(sub$Vis_100,
                       probs = 0.75)
      }
      
      
      lines(sub$t_, sub$vis24_mean_100, col = "red") # rolling quantile
      abline(h = mm, col = "blue", lty = 2)
      
      
      
      if(spp == "BHS"){
        x1<-paste('bhs_', sub$Hour[1], "Hour", sep = "")
        ds<-c(x1)
        data(list = ds, package = "happybirthday")
      }
      
      
      if(spp == "MD"){
        x1<-paste('md_', sub$Hour[1], "Hour", sep = "")
        ds<-c(x1)
        data(list = ds, package = "happybirthday")
      }
      
      
      if(spp == "ELK"){
        x1<-paste('elk_', sub$Hour[1], "Hour", sep = "")
        ds<-c(x1)
        data(list = ds, package = "happybirthday")
      }
      
      if(spp == "MOOSE"){
        x1<-paste('moose_', sub$Hour[1], "Hour", sep = "")
        ds<-c(x1)
        data(list = ds, package = "happybirthday")
      }
      
      
      
      Pattern1_list <- mget(x = x1, envir = parent.frame())[[1]]
      # here need to pick the correct RF model 
      sub$pred_prob<-as.numeric(randomForest:::predict.randomForest(Pattern1_list,sub,type='prob')[,1])
      
      
      
      # here need to input the right threshold
      
      if(spp == "BHS"){
        # probability of parturition plot
        
        data(bhs_thresh, package = "happybirthday")
        thresh<-bhs_thresh[bhs_thresh$tw == sub$Hour[1],]$Threshold
        fneg<-bhs_thresh[bhs_thresh$tw == sub$Hour[1],]$sup_fn
      }
      
      
      if(spp == "MD"){
        data(md_thresh, package = "happybirthday")
        thresh<-md_thresh[md_thresh$tw == sub$Hour[1],]$Threshold
        fneg<-md_thresh[md_thresh$tw == sub$Hour[1],]$sup_fn
      }
      
      
      if(spp == "ELK"){
        data(elk_thresh, package = "happybirthday")
        thresh<-elk_thresh[elk_thresh$tw == sub$Hour[1],]$Threshold
        fneg<-elk_thresh[elk_thresh$tw == sub$Hour[1],]$sup_fn
      }
      
      if(spp == "MOOSE"){
        data(moose_thresh, package = "happybirthday")
        thresh<-moose_thresh[moose_thresh$tw == sub$Hour[1],]$Threshold
        fneg<-moose_thresh[moose_thresh$tw == sub$Hour[1],]$sup_fn
      }
      
      
      
      # here need to input the right sliding window 
      slw<-round(24/sub$Hour[1])
      
      sub[,"MeanThreshold"] <- as.vector(rollapply(zoo(sub[,"pred_prob"]), slw, function(x){mean(sum(as.numeric(x > thresh), na.rm=T), na.rm=T)}, fill=NA))/as.vector(rollapply(zoo(sub[,"pred_prob"]), slw, function(x){length(x[!is.na(x)])}, fill=NA))
      sub<-sub[complete.cases(sub$pred_prob),]
      
      # here need to input the simulrf--sup_fn
      toto<-rle(as.vector(as.numeric(sub[,"MeanThreshold"] > fneg)))
      
      if(length(which(toto$values==1)) > 0){
        
        # end = the cumulative sum of lengths where values == 1. 
        toto<-data.frame(end=cumsum(toto$lengths)[toto$values %in% "1"], duration=toto$lengths[toto$values %in% "1"])
        toto$parturition_start<-sub$t_[toto$end - toto$duration + 1]
        toto$parturition_end<-sub$t_[toto$end]
        toto$duration<-as.vector(difftime(toto$parturition_end, toto$parturition_start, unit="hours"))
        toto$id <- sub$AID[1]
        #toto$tw <- hrs[p]
        toto<-toto[,c("id","parturition_start","parturition_end","duration")]
        for(line in 1:nrow(toto)){
          
          #mean predicted probability above error rate between the parturition start and end
          toto$prop[line] <- mean(sub[sub$t_ >=toto$parturition_start[line] & sub$t_ <=toto$parturition_end[line],"MeanThreshold"], na.rm = T)
          # mean predicted probability raw
          toto$propmb[line] <- mean(sub[sub$t_ >=toto$parturition_start[line] & sub$t_ <=toto$parturition_end[line],"pred_prob"], na.rm = T)
          # max predicted probabiliyt between those two dates
          toto$max[line] <- max(sub[sub$t_ >=toto$parturition_start[line] & sub$t_ <=toto$parturition_end[line],"pred_prob"], na.rm = T)
          
          # mean date between these two dates, but weighted by predicted probability
          toto$weighted_date[line] <- as.character(weighted.mean(sub$t_[sub$t_ >=toto$parturition_start[line] & sub$t_ <=toto$parturition_end[line]], w=sub[sub$t_ >=toto$parturition_start[line] & sub$t_ <=toto$parturition_end[line],"pred_prob"],  na.rm=T))
        }
        toto <- toto[toto$max %in% max(toto$max),][1,]
        tmp <- toto
        #respred[[sub$AID[1]]] <- tmp
      }
      
      
      sub$jd<-as.Date(strftime(sub$t_, format = "%Y-%m-%d"), format = "%Y-%m-%d")
      subsum<-sub %>%
        group_by(jd) %>%
        summarize(MaxProb = max(pred_prob, na.rm = T), 
                  MeanProb = mean(pred_prob, na.rm = T))
      
      plot(subsum$jd, subsum$MeanProb, type = "l", ylab = "Probability of parturition",
           xlab = "Date", main = "Probability of Parturition", cex = 1.25, ylim = c(0, 1.0))
      lines(subsum$jd, subsum$MaxProb, type = "l", lty = "dashed")
      #abline(h = fneg, col = "blue", lty = 2)
      text.col<-c('grey', 'green', 'blue', 'red')
      if(nrow(tmp) >= 1){
        tmp$weighted_date<-as.POSIXct(tmp$weighted_date, format = "%Y-%m-%d %H:%M:%S")
        tmp$Date<-as.Date(strftime(tmp$weighted_date, format = "%Y-%m-%d"), '%Y-%m-%d')
        tmp$ProbCat<-ifelse(tmp$propmb >=0 & tmp$propmb <= 0.25, "Low", ifelse(tmp$propmb > 0.25 & tmp$propmb <= 0.5, "Medium", ifelse(tmp$propmb > 0.50 & tmp$propmb <= 0.75, "Medium-High", ifelse(tmp$propmb > 0.75 & tmp$propmb <= 1.0, "High", NA))))
        abline(v = tmp$Date, col = text.col[factor(tmp$ProbCat)], lwd = 2.5)
        legend('topright', legend = c('Low', 'Medium', 'Medium-High', 'High'), col = text.col, lwd = 1)
      }
      
      
      
      mf <- paste(paste("Ewe Frequency: ", id_df[id_df$AID == uni[k],]$Frequency,
                        sep = " "),paste('AID: ',id_df[id_df$AID == uni[k],]$AID,sep=''),sep=' ')
      
      
      mtext(mf, font = 2, side = 3, line = -2.25, outer = T,
            cex = 2)
      
      
      
      
      
      vf <- paste("Tag Number:", id_df[id_df$AID == uni[k],]$VitFreq, sep = " ")
      mtext(vf, font = 2, side = 3, line = -147, outer = T,
            cex = 2)
      
      dev.off()
      
      print(k)
      
    }
    
    
  }
  
  
  makeMarkdown(tempdir = tempdir, rollmean = rollmean, id_df = id_df, subsetmonth = subsetmonth, spp = spp)
  
  happybirthday::makeTables(gpsdat = x, id_df = id_df, tempdir = tempdir)
  
  
  happybirthday::renderMarkdown(tempdir = tempdir, markdowndir = markdowndir)
  
  #attachments. This is going to include the ParturitionMetrics PDF and html files if you want them to send as well.
  attach = c(paste0(tempdir, "/Products/Last3Days.html"),paste0(tempdir, "/Products/Last12Hours.html"), paste0(tempdir, "/Products/LatestLocs.kml"), paste0(tempdir, "/ParturitionMetrics.pdf"))
  
  mailR::send.mail(from = from,
                   to = to,
                   subject = paste0("Parturition Model Updated for ", study, " ", Sys.time()),
                   body = "This email contains the latest parturition model run.",
                   authenticate = TRUE,
                   smtp = list(host.name = "mail.gmx.com", port = 587, user.name = user, passwd = pass, tls = T), attach.files = attach)
  
}
