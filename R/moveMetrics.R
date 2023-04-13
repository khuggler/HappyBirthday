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

# get the minimum sampling rate 
hrs<-round(max(summ$min))

#group by id and nest track

trk<- trk|> amt::nest(data = -"id")

# track resample on multiple animals

trk_resamp<- trk |>
  mutate(steps = map(data, function(x)
    x |> track_resample(rate = hours(hrs), tolerance = minutes(15)) |> 
      filter_min_n_burst(min_n = 3)))


full_trk<- trk_resamp |>
  select(id, steps) |> unnest(cols = steps)


# create new id so that everything is sampled by animal id-burst
full_trk$moveid<-paste0(full_trk$id, "_", full_trk$burst_)
sheep_resamp<-full_trk


# filter out data 
tim<-paste(as.numeric(strftime(Sys.time(),format='%Y'))-1,'-', subsetmonth, '-01 00:00:00',sep='')
mdat<-sheep_resamp[which(sheep_resamp$t_>=as.POSIXct(tim,'%Y-%m-%d %H:%M:%S',tz='')),]



sheep.spat<-mdat
coordinates(sheep.spat)<-c('x_', 'y_')
proj4string(sheep.spat)<-'+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'

# keep a utm version
sheep.utm<-spTransform(sheep.spat, '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')




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





coordinates(movedata)<-c('x_', 'y_')
proj4string(movedata)<-proj4string(sheep.utm)

uni<-unique(movedata$id)


#' incremental HR size
#' ideally would want 24 hour home range, but some fix rates won't allow that--try to get as close as possible

inc<-round(24/mean(movedata$timediff, na.rm = T))


movedata2<-data.frame()
for(k in 1:length(uni)){
  sub<-movedata[movedata$id == uni[k],]
  
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
  rec100<-recurse::getRecursions(ssrec, 100)
  
  ss$RT<-rec100$residenceTime
  ss$visits<-rec100$revisits
  
  rfprep<-rbind(rfprep, ss)
}



movevars<-c('dist', 'moverate_kmhr', 'HR', 'RT', 'visits')
movedata<-rfprep[complete.cases(rfprep[,movevars]),]

uni<-unique(rfprep$id)
roll.mean<-data.frame()
for(l in 1:length(uni)){
  sub<-movedata[movedata$id == uni[l],]
  
  sub<-sub[order(sub$t_),]
  
  
  # 2 increment rolling mean
  
  sub$dist2_mean<-NA
  sub$dist2_mean[2:nrow(sub)]<-zoo::rollmean(sub$dist,align=c('right'),k=2)
  
  sub$mr2_mean<-NA
  sub$mr2_mean[2:nrow(sub)]<-zoo::rollmean(sub$moverate_kmhr,align=c('right'),k=2)
  
  sub$hr2_mean<-NA
  sub$hr2_mean[2:nrow(sub)]<-zoo::rollmean(sub$HR,align=c('right'),k=2)
  
  sub$rt2_mean<-NA
  sub$rt2_mean[2:nrow(sub)]<-zoo::rollmean(sub$RT,align=c('right'),k=2)
  
  sub$vis2_mean<-NA
  sub$vis2_mean[2:nrow(sub)]<-zoo::rollmean(sub$visits,align=c('right'),k=2)
  
  
  
  # 3 increment rolling mean
  
  sub$dist3_mean<-NA
  sub$dist3_mean[3:nrow(sub)]<-zoo::rollmean(sub$dist,align=c('right'),k=3)
  
  sub$mr3_mean<-NA
  sub$mr3_mean[3:nrow(sub)]<-zoo::rollmean(sub$moverate_kmhr,align=c('right'),k=3)
  
  sub$hr3_mean<-NA
  sub$hr3_mean[3:nrow(sub)]<-zoo::rollmean(sub$HR,align=c('right'),k=3)
  
  sub$rt3_mean<-NA
  sub$rt3_mean[3:nrow(sub)]<-zoo::rollmean(sub$RT,align=c('right'),k=3)
  
  sub$vis3_mean<-NA
  sub$vis3_mean[3:nrow(sub)]<-zoo::rollmean(sub$visits,align=c('right'),k=3)
  
  
  
  # 6 increment rolling mean
  
  sub$dist6_mean<-NA
  sub$dist6_mean[6:nrow(sub)]<-zoo::rollmean(sub$dist,align=c('right'),k=6)
  
  sub$mr6_mean<-NA
  sub$mr6_mean[6:nrow(sub)]<-zoo::rollmean(sub$moverate_kmhr,align=c('right'),k=6)
  
  sub$hr6_mean<-NA
  sub$hr6_mean[6:nrow(sub)]<-zoo::rollmean(sub$HR,align=c('right'),k=6)
  
  sub$rt6_mean<-NA
  sub$rt6_mean[6:nrow(sub)]<-zoo::rollmean(sub$RT,align=c('right'),k=6)
  
  sub$vis6_mean<-NA
  sub$vis6_mean[6:nrow(sub)]<-zoo::rollmean(sub$visits,align=c('right'),k=6)
  
  roll.mean<-rbind(sub, roll.mean)
  
  print(l)
}

return(roll.mean)

}




