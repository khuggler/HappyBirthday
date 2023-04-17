#' @title moveMetrics
#' @description calculate movement metrics including: movement rate, rolling home range size, residence time, and visitation rate
#' @param gpsdat data.frame of gps data from the getData.R function
#' @param subsetmonth month to start subsetting GPS data. If GPS collars have recently been deployed, suggest using a month early in the year
#' @return returns a data.frame with rolling statistics of movement metrics
#' @details 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[amt]{track}}, \code{\link[amt]{summarize_sampling_rate}}, \code{\link[amt]{reexports}}
#'  \code{\link[adehabitatHR]{mcp}}
#'  \code{\link[recurse]{getRecursions}}
#'  \code{\link[zoo]{rollmean}}
#' @rdname moveMetrics
#' @export 
#' @importFrom amt make_track summarize_sampling_rate_many nest
#' @importFrom adehabitatHR mcp.area
#' @importFrom recurse getRecursions
#' @importFrom zoo rollmean
moveMetrics<-function(gpsdat, subsetmonth){
require(amt)
require(purrr)
require(sp)
  
# clean up gps data
gps<-gpsdat[complete.cases(gpsdat$tdate),]
gps$id<-gps$AID
trk<-gps |> amt::make_track(x, y, tdate, id = id, all_cols = TRUE)

summ<-trk |>
  amt::summarize_sampling_rate_many(c("id"), units = "hours")


# track_resample each individual to a constant movement rate 

uni<-unique(trk$id)
track_resamp<-NULL
for(i in 1:length(uni)){
  subtrk<-trk[trk$id == uni[i],]
  movesumm<-summ[summ$id == uni[i],]
  
  hrs<-floor(movesumm$median)
  
  new_trk<-amt::track_resample(subtrk, rate = lubridate::hours(hrs), tolerance = lubridate::minutes(15)) %>%
    amt::filter_min_n_burst(min_n = 3)
  
  track_resamp<-rbind(new_trk, track_resamp)
  
  print(i)
}

full_trk<-track_resamp
# trk_resamp<- trk |>
#   mutate(steps = purrr::map(data, function(x)
#     x |> amt::track_resample(rate = lubridate::hours(hrs), tolerance = lubridate::minutes(15)) |> 
#       amt::filter_min_n_burst(min_n = 3)))
# 
# 
# full_trk<- trk_resamp |>
#   amt::select(id, steps) |> amt::unnest(cols = steps)


# create new id so that everything is sampled by animal id-burst
full_trk$moveid<-paste0(full_trk$id, "_", full_trk$burst_)
sheep_resamp<-full_trk


# filter out data 
tim<-paste(as.numeric(strftime(Sys.time(),format='%Y'))-1,'-', subsetmonth, '-01 00:00:00',sep='')
mdat<-sheep_resamp[which(sheep_resamp$t_>=as.POSIXct(tim,'%Y-%m-%d %H:%M:%S',tz='')),]


sheep.spat<-mdat
sp::coordinates(sheep.spat)<-c('x_', 'y_')
sp::proj4string(sheep.spat)<-'+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'

# keep a utm version
sheep.utm<-sp::spTransform(sheep.spat, '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')




uni<-unique(sheep.utm$moveid)
movedata<-data.frame()
for(i in 1:length(uni)){
  
  sub<-data.frame(sheep.utm[sheep.utm$moveid== uni[i],])
  sub<-sub[order(sub$t_),]
  
  # sub$Hour<-strftime(sub$TelemDate,format='%H')
  #  sub$JulDay<-strftime(sub$TelemDate,format='%j')
  # sub$month<-strftime(sub$TelemDate, format = '%m')
  
  sub$dist<-NA
  sub$timediff<-NA
  sub$moverate_kmhr<-NA
  
  for(k in 2:nrow(sub)){
    sub$dist[k]<-sqrt(((sub$x_[k]-sub$x_[k-1]))^2)+ sqrt((sub$y_[k]-sub$y_[k-1])^2)
    sub$timediff[k]<-(as.numeric(difftime(sub$t_[k], sub$t_[k-1], units = "hours")))
    sub$moverate_kmhr[k]<-ifelse(sub$dist[k]==0, 0, (sub$dist[k])/(as.numeric(difftime(sub$t_[k], sub$t_[k-1], units = "hours"))))
  }
  
  sub$moverate_kmhr<-sub$moverate_kmhr/1000
  
  
  movedata<-rbind(movedata, sub)
}





sp::coordinates(movedata)<-c('x_', 'y_')
sp::proj4string(movedata)<-sp::proj4string(sheep.utm)

uni<-unique(movedata$id)


movedata2<-data.frame()
for(k in 1:length(uni)){
  sub<-movedata[movedata$id == uni[k],]
  movesum<-summ[summ$id == uni[k],]
  
  inc<-floor(24/movesum$median)
  
  if(inc < 5){
    inc = 5
  }
  
  # sequence of row ids
  s1<-seq(1, nrow(sub)-inc, 1)
  s2<-seq(inc, nrow(sub),1)
  
  # add HR column
  sub$HR<-NA
  
  #loop through and calculate rolling HR size in km
  all<-data.frame()
  for(l in 1:length(s1)){
    subsub<-sub[s1[l]:s2[l],]
    hr<-adehabitatHR::mcp.area(subsub, percent = 95, unin = "m", unout = "km2")
    
    s<-data.frame(subsub[1,])
    s$HR<-hr$a
    
    s$Increment<-paste0(round(inc*movesum$median), " Hours")
    
    all<-rbind(s, all)
    
    print(paste0('Working on home range ', l, ' for animal ', uni[k]))
  
  }
  
  print(paste0('Animal  ', uni[k], ' is complete...'))
  
  movedata2<-rbind(all, movedata2)
}





uni<-unique(movedata2$moveid)

rfprep<-data.frame()
for(i in 1:length(uni)){
  ss<-movedata2[movedata2$moveid == uni[i],]
  
  ss<-ss[order(ss$t_),]
  
  ssrec<-ss[, c('x_', 'y_', 't_', 'moveid')]
  
  rec100<-recurse::getRecursions(ssrec, 100, timeunits = "hours")
  rec250<-recurse::getRecursions(ssrec, 250, timeunits = "hours")
  rec500<-recurse::getRecursions(ssrec, 500, timeunits = "hours")
  
  ss$RT_100<-rec100$residenceTime
  ss$RT_250<-rec250$residenceTime
  ss$RT_500<-rec500$residenceTime
  
  ss$Vis_100<-rec100$revisits
  ss$Vis_250<-rec250$revisits
  ss$Vis_500<-rec500$revisits
  
  rfprep<-rbind(rfprep, ss)
}









movevars<-c('dist', 'moverate_kmhr', 'HR', 'RT_100', 'RT_250', 'RT_500', 'Vis_100', 'Vis_250', 'Vis_500')
rfprep<-rfprep[complete.cases(rfprep[,movevars]),]

uni<-unique(rfprep$AID)
roll.mean<-data.frame()
for(l in 1:length(uni)){
  sub<-rfprep[rfprep$AID == uni[l],]
  
  movesum<-summ[summ$id == uni[l],]
  
  sub<-sub[order(sub$t_),]
  sub$RollWindow_Inc<-round(movesum$median)
  
  hrs<-round(movesum$median)
  
  if(hrs <= 2){
  inc_4<-4/hrs
  # 4 hour rolling mean
  sub$dist4_mean<-NA
  sub$dist4_mean[inc_4:nrow(sub)]<-zoo::rollmean(sub$dist,align=c('right'),k=inc_4)
  
  sub$mr4_mean<-NA
  sub$mr4_mean[inc_4:nrow(sub)]<-zoo::rollmean(sub$moverate_kmhr,align=c('right'),k=inc_4)
  
  sub$hr4_mean<-NA
  sub$hr4_mean[inc_4:nrow(sub)]<-zoo::rollmean(sub$HR,align=c('right'),k=inc_4)
  
  sub$rt4_mean_100<-NA
  sub$rt4_mean_100[inc_4:nrow(sub)]<-zoo::rollmean(sub$RT_100,align=c('right'),k=inc_4)
  
  sub$rt4_mean_250<-NA
  sub$rt4_mean_250[inc_4:nrow(sub)]<-zoo::rollmean(sub$RT_250,align=c('right'),k=inc_4)
  
  sub$rt4_mean_500<-NA
  sub$rt4_mean_500[inc_4:nrow(sub)]<-zoo::rollmean(sub$RT_500,align=c('right'),k=inc_4)
  
  sub$vis4_mean_100<-NA
  sub$vis4_mean_100[inc_4:nrow(sub)]<-zoo::rollmean(sub$Vis_100,align=c('right'),k=inc_4)
  
  sub$vis4_mean_250<-NA
  sub$vis4_mean_250[inc_4:nrow(sub)]<-zoo::rollmean(sub$Vis_250,align=c('right'),k=inc_4)
  
  sub$vis4_mean_500<-NA
  sub$vis4_mean_500[inc_4:nrow(sub)]<-zoo::rollmean(sub$Vis_500,align=c('right'),k=inc_4)
  
  }
  
  if(hrs <= 4){
    
  # 8 hour rolling mean
  inc_8<-round(8/hrs)
  
  sub$dist8_mean<-NA
  sub$dist8_mean[inc_8:nrow(sub)]<-zoo::rollmean(sub$dist,align=c('right'),k=inc_8)
  
  sub$mr8_mean<-NA
  sub$mr8_mean[inc_8:nrow(sub)]<-zoo::rollmean(sub$moverate_kmhr,align=c('right'),k=inc_8)
  
  sub$hr8_mean<-NA
  sub$hr8_mean[inc_8:nrow(sub)]<-zoo::rollmean(sub$HR,align=c('right'),k=inc_8)
  
  sub$rt8_mean_100<-NA
  sub$rt8_mean_100[inc_8:nrow(sub)]<-zoo::rollmean(sub$RT_100,align=c('right'),k=inc_8)
  
  sub$rt8_mean_250<-NA
  sub$rt8_mean_250[inc_8:nrow(sub)]<-zoo::rollmean(sub$RT_250,align=c('right'),k=inc_8)
  
  sub$rt8_mean_500<-NA
  sub$rt8_mean_500[inc_8:nrow(sub)]<-zoo::rollmean(sub$RT_500,align=c('right'),k=inc_8)
  
  sub$vis8_mean_100<-NA
  sub$vis8_mean_100[inc_8:nrow(sub)]<-zoo::rollmean(sub$Vis_100,align=c('right'),k=inc_8)
  
  sub$vis8_mean_250<-NA
  sub$vis8_mean_250[inc_8:nrow(sub)]<-zoo::rollmean(sub$Vis_250,align=c('right'),k=inc_8)
  
  sub$vis8_mean_500<-NA
  sub$vis8_mean_500[inc_8:nrow(sub)]<-zoo::rollmean(sub$Vis_500,align=c('right'),k=inc_8)
  
  }
  
  if(hrs <= 8){
  
  # 16 hour rolling mean
  
  inc_16<-round(16/hrs)
  sub$dist16_mean<-NA
  sub$dist16_mean[inc_16:nrow(sub)]<-zoo::rollmean(sub$dist,align=c('right'),k=inc_16)
  
  sub$mr16_mean<-NA
  sub$mr16_mean[inc_16:nrow(sub)]<-zoo::rollmean(sub$moverate_kmhr,align=c('right'),k=inc_16)
  
  sub$hr16_mean<-NA
  sub$hr16_mean[inc_16:nrow(sub)]<-zoo::rollmean(sub$HR,align=c('right'),k=inc_16)
  
  sub$rt16_mean_100<-NA
  sub$rt16_mean_100[inc_16:nrow(sub)]<-zoo::rollmean(sub$RT_100,align=c('right'),k=inc_16)
  
  sub$rt16_mean_250<-NA
  sub$rt16_mean_250[inc_16:nrow(sub)]<-zoo::rollmean(sub$RT_250,align=c('right'),k=inc_16)
  
  sub$rt16_mean_500<-NA
  sub$rt16_mean_500[inc_16:nrow(sub)]<-zoo::rollmean(sub$RT_500,align=c('right'),k=inc_16)
  
  sub$vis16_mean_100<-NA
  sub$vis16_mean_100[inc_16:nrow(sub)]<-zoo::rollmean(sub$Vis_100,align=c('right'),k=inc_16)
  
  sub$vis16_mean_250<-NA
  sub$vis16_mean_250[inc_16:nrow(sub)]<-zoo::rollmean(sub$Vis_250,align=c('right'),k=inc_16)
  
  sub$vis16_mean_500<-NA
  sub$vis16_mean_500[inc_16:nrow(sub)]<-zoo::rollmean(sub$Vis_500,align=c('right'),k=inc_16)
  
  }
  
  if(hrs <= 12){
    
  inc_24<-round(24/hrs)
  
  # 24 hour rolling mean
  
  sub$dist24_mean<-NA
  sub$dist24_mean[inc_24:nrow(sub)]<-zoo::rollmean(sub$dist,align=c('right'),k=inc_24)
  
  sub$mr24_mean<-NA
  sub$mr24_mean[inc_24:nrow(sub)]<-zoo::rollmean(sub$moverate_kmhr,align=c('right'),k=inc_24)
  
  sub$hr24_mean<-NA
  sub$hr24_mean[inc_24:nrow(sub)]<-zoo::rollmean(sub$HR,align=c('right'),k=inc_24)
  
  sub$rt24_mean_100<-NA
  sub$rt24_mean_100[inc_24:nrow(sub)]<-zoo::rollmean(sub$RT_100,align=c('right'),k=inc_24)
  
  sub$rt24_mean_250<-NA
  sub$rt24_mean_250[inc_24:nrow(sub)]<-zoo::rollmean(sub$RT_250,align=c('right'),k=inc_24)
  
  sub$rt24_mean_500<-NA
  sub$rt24_mean_500[inc_24:nrow(sub)]<-zoo::rollmean(sub$RT_500,align=c('right'),k=inc_24)
  
  sub$vis24_mean_100<-NA
  sub$vis24_mean_100[inc_24:nrow(sub)]<-zoo::rollmean(sub$Vis_100,align=c('right'),k=inc_24)
  
  sub$vis24_mean_250<-NA
  sub$vis24_mean_250[inc_24:nrow(sub)]<-zoo::rollmean(sub$Vis_250,align=c('right'),k=inc_24)
  
  sub$vis24_mean_500<-NA
  sub$vis24_mean_500[inc_24:nrow(sub)]<-zoo::rollmean(sub$Vis_500,align=c('right'),k=inc_24)
  
  }
  
  if(hrs <= 16){
  
  inc_32<-round(32/hrs)
  
  sub$dist32_mean<-NA
  sub$dist32_mean[inc_32:nrow(sub)]<-zoo::rollmean(sub$dist,align=c('right'),k=inc_32)
  
  sub$mr32_mean<-NA
  sub$mr32_mean[inc_32:nrow(sub)]<-zoo::rollmean(sub$moverate_kmhr,align=c('right'),k=inc_32)
  
  sub$hr32_mean<-NA
  sub$hr32_mean[inc_32:nrow(sub)]<-zoo::rollmean(sub$HR,align=c('right'),k=inc_32)
  
  sub$rt32_mean_100<-NA
  sub$rt32_mean_100[inc_32:nrow(sub)]<-zoo::rollmean(sub$RT_100,align=c('right'),k=inc_32)
  
  sub$rt32_mean_250<-NA
  sub$rt32_mean_250[inc_32:nrow(sub)]<-zoo::rollmean(sub$RT_250,align=c('right'),k=inc_32)
  
  sub$rt32_mean_500<-NA
  sub$rt32_mean_500[inc_32:nrow(sub)]<-zoo::rollmean(sub$RT_500,align=c('right'),k=inc_32)
  
  sub$vis32_mean_100<-NA
  sub$vis32_mean_100[inc_32:nrow(sub)]<-zoo::rollmean(sub$Vis_100,align=c('right'),k=inc_32)
  
  sub$vis32_mean_250<-NA
  sub$vis32_mean_250[inc_32:nrow(sub)]<-zoo::rollmean(sub$Vis_250,align=c('right'),k=inc_32)
  
  sub$vis32_mean_500<-NA
  sub$vis32_mean_500[inc_32:nrow(sub)]<-zoo::rollmean(sub$Vis_500,align=c('right'),k=inc_32)
  
  }
  
  if(hrs <= 24){
  
  inc_48<-round(48/hrs)
  
  # 48 hour rolling mean
  
  sub$dist48_mean<-NA
  sub$dist48_mean[inc_48:nrow(sub)]<-zoo::rollmean(sub$dist,align=c('right'),k=inc_48)
  
  sub$mr48_mean<-NA
  sub$mr48_mean[inc_48:nrow(sub)]<-zoo::rollmean(sub$moverate_kmhr,align=c('right'),k=inc_48)
  
  sub$hr48_mean<-NA
  sub$hr48_mean[inc_48:nrow(sub)]<-zoo::rollmean(sub$HR,align=c('right'),k=inc_48)
  
  sub$rt48_mean_100<-NA
  sub$rt48_mean_100[inc_48:nrow(sub)]<-zoo::rollmean(sub$RT_100,align=c('right'),k=inc_48)
  
  sub$rt48_mean_250<-NA
  sub$rt48_mean_250[inc_48:nrow(sub)]<-zoo::rollmean(sub$RT_250,align=c('right'),k=inc_48)
  
  sub$rt48_mean_500<-NA
  sub$rt48_mean_500[inc_48:nrow(sub)]<-zoo::rollmean(sub$RT_500,align=c('right'),k=inc_48)
  
  sub$vis48_mean_100<-NA
  sub$vis48_mean_100[inc_48:nrow(sub)]<-zoo::rollmean(sub$Vis_100,align=c('right'),k=inc_48)
  
  sub$vis48_mean_250<-NA
  sub$vis48_mean_250[inc_48:nrow(sub)]<-zoo::rollmean(sub$Vis_250,align=c('right'),k=inc_48)
  
  sub$vis48_mean_500<-NA
  sub$vis48_mean_500[inc_48:nrow(sub)]<-zoo::rollmean(sub$Vis_500,align=c('right'),k=inc_48)
  
  }
  
  roll.mean<-plyr::rbind.fill(sub, roll.mean)

  
  print(l)
}


return(roll.mean)

}




