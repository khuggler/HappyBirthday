#' @title makeMaps
#' @description create maps of most recent locations and movement for bighorn sheep
#' @param tempdir temporary directory for storing maps
#' @param gpsdat data.frame of GPS data
#' @param id_df lookup table of animal IDs and serial numbers
#' @return Does not return any values. This function will save maps of the most recent locations in KML and GPS locations as well as the last three days of locations and movement in a leaflet map
#' @details
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  makeMaps(tempdir = 'directory', gpsdat = gpsdat, id_df = id_df)
#'  }
#' }
#' @seealso 
#'  \code{\link[assertthat]{assert_that}}
#'  \code{\link[sp]{coordinates}}, \code{\link[sp]{is.projected}}
#'  \code{\link[rgdal]{writeOGR}}
#'  \code{\link[purrr]{keep}}, \code{\link[purrr]{map}}
#'  \code{\link[htmlwidgets]{saveWidget}}
#' @rdname makeMaps
#' @export 
#' @importFrom assertthat assert_that
#' @importFrom sp coordinates proj4string
#' @importFrom rgdal writeOGR
#' @importFrom purrr keep walk
#' @importFrom htmlwidgets saveWidget
makeMaps<-function(tempdir, gpsdat, id_df){
  
  require(leaflet)

  savedir = paste0(tempdir, "/", 'Products/')
  if(!dir.exists(savedir)){
    dir.create(savedir)
  }
  
  assertthat::assert_that(class(gpsdat$tdate)[1] == "POSIXct", msg = "TelemDate column must be in POSIXct format")
  
  assertthat::assert_that('Frequency' %in% unique(names(id_df)), msg = "Lookup data must include Frequency")
  assertthat::assert_that('Serial' %in% unique(names(id_df)), msg = "Lookup data must include Serial")
  assertthat::assert_that('IdCol' %in% unique(names(id_df)), msg = "Lookup data must include IdCol")
  
  
  
uni<-unique(gpsdat$AID)

lastpoint<-data.frame()
for(i in 1:length(uni)){
  sub<-gpsdat[gpsdat$AID==uni[i],]
  sub<-sub[order(sub$tdate,decreasing = T),]
  sub<-as.data.frame(sub)
  
  lastpoint<-rbind(lastpoint,sub[1,])
}
lastpoint$AID<-as.character(lastpoint$AID)


lastpoint<-lastpoint[complete.cases(lastpoint$x),]

sp::coordinates(lastpoint)<-~x+y
sp::proj4string(lastpoint)<-'+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs' 

names(lastpoint)[names(lastpoint) == 'AID']<-'name'
rgdal::writeOGR(lastpoint["name"], paste(savedir,'LatestLocs.kml',sep=''), layer = 'BHS', driver = "KML",
                overwrite = T)

#lastpoint<-sp::spTransform(lastpoint,'+proj=utm +zone=12 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
lastpoint<-lastpoint[,names(lastpoint) == 'name']
#names(lastpoint)[1]<-'Frequency'
rgdal::writeOGR(lastpoint,paste(savedir,'LatestLocs.gpx',sep=''),layer='locs',driver='GPX',overwrite_layer=T)





pal <- leaflet::colorFactor(palette = 'Paired',domain = gpsdat$AID)

#create custom icons for most recent locations
icon.active <- makeAwesomeIcon(icon = "star", markerColor = "lightgray", spin=TRUE,
                               iconColor = "black", library = "fa",
                               squareMarker =  FALSE)
icon.inactive <- makeAwesomeIcon(icon = "star", markerColor = "lightgray", spin=FALSE,
                                 iconColor = "black", library = "fa",
                                 squareMarker =  FALSE)

# icon.mortality <- makeIcon("https://www.svgrepo.com/svg/404123/skull-and-crossbones",
#                            iconWidth= 30, iconHeight= 30)



sheepmap<-gpsdat[gpsdat$tdate>= Sys.time()-lubridate::days(3),]
sheepmap<-sheepmap[complete.cases(sheepmap$x),]
uni<-unique(sheepmap$AID)
sheepmap$popup<-paste0(signif(sheepmap$y, digits = 6), ",", signif(sheepmap$x, digits = 7))
for(n in 1:length(uni)){
  out<-sheepmap[sheepmap$AID == uni[n],]
  out<-out[order(out$tdate, decreasing = FALSE),]
  
  if(nrow(out)>0){
    f_name<-out$AID[1] #create unique filename
    
    #set up leaflet options
    out$Label<-NA
    out$Label<-as.character(out$popup) #Create hover over layer
    out$Popup<-NA
    out$Popup<-f_name #Create hover over layer
    if(exists("a")==FALSE){ #build leaflet with first animal
      a<-out %>%
        leaflet() %>%
        #addTiles() %>%
        #addProviderTiles("Esri.WorldImagery") %>%
        addProviderTiles(providers$Esri.NatGeoWorldMap) %>%  #choose base layer
        addCircleMarkers(lng=~x, lat=~y, label=~Label, popup=~Popup,col=~pal(AID), radius=1.5, opacity=100) %>% #add as circles
        addPolylines(lng=~x, lat=~y, weight=0.5, col="black", opacity=200)
    }
    
    
    a<-addCircleMarkers(map=a,data=out,lng=~x, lat=~y, label=~Label, popup=~Popup,col=~pal(AID), radius=1.5, opacity=100)
    a<-addPolylines(map=a, data=out,lng=~x, lat=~y, weight=0.5, col="black", opacity=200)
    
    # if(out$idmortalitystatus[nrow(out)] == "5"){
    #   #add mortality markers
    #   a<-addMarkers(map=a, data=out[nrow(out),],lng=~longitude, lat=~latitude,label=~Label, popup=~Popup, icon=icon.mortality)
    # }
    
    #add active/inactive/mort icons ---- inactive defined as no iridium uplink in last 2 days
    
    if(out$tdate[nrow(out)] <= Sys.time() - as.difftime(2, unit= "days")){ #if its inactive
      a<-addAwesomeMarkers(map=a, data=out[nrow(out),],lng=~x, lat=~y,
                           label=out$AID,
                           labelOptions= labelOptions(noHide=T, textOnly = T, style=list("font-style" = "bold", "font-size"="15px")),
                           popup=~Popup, icon=icon.inactive)
    }
    
    if(out$tdate[nrow(out)] >= Sys.time() - as.difftime(2, unit = "days")) { #make it active
      a<-addAwesomeMarkers(map=a, data=out[nrow(out),],lng=~x, lat=~y,
                           label= out$AID,
                           labelOptions= labelOptions(noHide=T, textOnly = T, style=list("font-style" = "bold", "font-size"="15px")),
                           popup=~Popup, icon=icon.active)
    }
  }
  
}

#add mortality locations
#a<-addCircleMarkers(map=a,data=elk2,lng=~longitude, lat=~latitude,popup=~popup, col="red", radius=1.5, opacity=100)

#add layer control
# Take out ESRI provided tiles
esri <- providers %>%
  purrr::keep(~ grepl('^Esri',.))
#remove a bunch of worthless esri layers
esri[[11]]<-NULL
esri[[9]]<-NULL
esri[[8]]<-NULL
esri[[7]]<-NULL
esri[[6]]<-NULL
esri[[2]]<-NULL
#reorder list so desired list is on top of legend and as primary basemap
esri <- esri[c("Esri.DeLorme", "Esri.WorldImagery", "Esri.WorldTopoMap","Esri.NatGeoWorldMap", "Esri")]
esri %>%
  purrr::walk(function(x) a <<- a %>% addProviderTiles(x,group=x))
a<-a %>%
  addLayersControl(
    baseGroups = names(esri),
    options = layersControlOptions(collapsed = TRUE)) %>%
  addLegend(pal = pal, values = sheepmap$AID, group = "sheepmap", opacity=100, position = "bottomleft")
#a #plot

#a<- a%>% addTitle(text=paste('Updated:', Sys.time()), color= "black", fontSize= "18px", leftPosition = 50, topPosition=2)
# 



htmlwidgets::saveWidget(a, file=paste(savedir, 'Last3Days.html', sep = ""),
                        title="SheepMovement", selfcontained=TRUE)


rm(a)

}


