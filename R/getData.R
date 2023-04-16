#' @title getData
#' @description This function downloads GPS data for a specified population and appends animal IDs
#' @param id_df data.frame that has the animal ids and serial numbers
#' @param tempdir character. path to temporary directory for telonics files to be saved. If this file in the path does not exist it will be created for you. Default is NA. 
#' @param veckeys if Vectronic data is downloaded, path to Vectronic keys is needed, Default: NA
#' @param lotek_usrs if lotek data is needed, vector of lotek usernames for download
#' @param lotek_pass if lotek data is needed, vector of lotek passwords for download
#' @param ATS_usrs if ATS data is needed, vector of ATS usernames
#' @param ATS_pass if ATS data is needed, vectors of ATS passwords
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[processx]{run}}
#'  \code{\link[collar]{ats_login}}, \code{\link[collar]{fetch_ats_positions}}, \code{\link[collar]{ats_logout}}, \code{\link[collar]{get_paths}}, \code{\link[collar]{fetch_vectronics}}
#'  \code{\link[dplyr]{bind_rows}}
#' @rdname getData
#' @export 
#' @importFrom processx run
#' @importFrom collar ats_login fetch_ats_positions ats_logout get_paths fetch_vectronics
#' @importFrom dplyr bind_rows
getData<-function(id_df, tempdir = NA, veckeys = NA, lotek_usrs = NA, lotek_pass = NA, ATS_usrs = NA, ATS_pass = NA){
  
  require(dplyr)
  require(collar)
  
  if(!dir.exists(tempdir)){
    dir.create(tempdir)
  }
  
  mans<-id_df$Brand
  
  full.tel<-data.frame()
  tel<-NA
if('Telonics' %in% mans){
  
  for(l in 1:length(lotek_usrs)){
    username = lotek_usrs[l]
    password =lotek_pass[l]   # must be \\ slashes
    TDC_path = "C:\\Program Files (x86)\\Telonics\\Data Converter\\TDC.exe"
    keep.reports = FALSE
    
    # create a folder for telonics data to go
    fldr_out<-paste0(tempdir, "/", 'Telonics')
    if(!dir.exists(fldr_out)){
      dir.create(fldr_out)
    }
    
    if(all(c("processx","sf") %in% installed.packages()[, 1]) == FALSE) 
      stop("You must install the following packages: processx and sf")
    
    # some tests
    if(length(dir(fldr_out))>0)
      stop("Your fldr_out must be empty to proceed!")
    
    # create a reports folder
    fldr_reports <- paste0(fldr_out, "\\reports")
    dir.create(fldr_reports)
    
    
    # create the xml file
    txt <- paste("<BatchSettingsV2>",
                 "\t<Globalstar>", 
                 paste0("\t\t<Username>",username,"</Username>"),
                 paste0("\t\t<Password>",password,"</Password>"),
                 "\t</Globalstar>", 
                 "\t<DownloadData>true</DownloadData>",
                 "\t<ConvertAllData />",
                 paste0("\t<BatchLog>",fldr_out,"\\BatchLog.txt</BatchLog>"),
                 paste0("\t<OutputFolder>",fldr_reports,"</OutputFolder>"),
                 "<ReportFileMode>overwrite</ReportFileMode>",
                 "</BatchSettingsV2>",
                 sep="\n")
    
    Batch_path <- paste0(fldr_out, "\\TelonicsBatchFile.xml")
    # save the xml file
    cat(txt, file=Batch_path)
    
    print("Downloading data from Telonics")
    Batch_path = paste0("/batch:", Batch_path)  # create new batch path for processx
    processx::run(TDC_path, Batch_path)  # TDC should be closed on your computer
    
    print("Importing CSV files")
    #Import the csv files from batch ####
    # Create a list of the data you will import
    fls <- list.files(fldr_reports, ".csv$")
    
    ## Run a loop that goes over the list, cleans and merges the data
    # Create an empty data frame where all the individuals will be merged in
    fixes <- do.call(rbind, lapply(1:length(fls), function(i){
      # The skip parameter is because there is some meta information above where the recordings begin
      df.i = read.csv(paste0(fldr_reports,"/",fls[i]), skip = 22, header = TRUE)
      
      # Get the ID and add it as a column (I am using the name the file is saved under and extracting the
      # component that will match with the way it is saved in my meta data column)
      df.i$CollarSerialNumber <- substr(fls[i], 1, 7)
      
      # Isolate the cases with a successful fix attempt
      df.i <- df.i[which(df.i$GPS.Fix.Attempt=="Succeeded"),]
      
      print(paste0(nrow(df.i), " rows of data imported for individual: ", substr(fls[i], 1, 7)))
      
      # Work on the DateTime stamp. It is a character string so I will first convert it to POSIXct
      # I always try to avoid deleting raw data (you never know when you will need it) so I will create a new DateTime Column
      df.i$GPS.Fix.Time = as.POSIXct(df.i$GPS.Fix.Time, format="%Y.%m.%d %H:%M:%S", tz = "UTC")
    
      return(df.i)

    }))
    
    # order by serial number and then by date
    fixes <- fixes[order(fixes$CollarSerialNumber, fixes$GPS.Fix.Time),]
    
    
    tel<-data.frame(fixes)
    
    tel <- tel %>%
      rename(tdate = "GPS.Fix.Time", x = "GPS.Longitude", y = "GPS.Latitude", SN = "CollarSerialNumber") %>%
      select(SN, tdate, x, y)
    
    full.tel<-rbind(tel, full.tel)
    
    unlink(fldr_out, force = TRUE, recursive = TRUE)
    
  }
  
  # remove all the temporary files if keep.reports = FALSE
  
  #fls = dir(fldr_out, full.names = TRUE, recursive = TRUE, include.dirs = TRUE)
  #unlink(fls, force=TRUE, recursive = TRUE)
  

  
}
   
    
   
  
  
  
  # ATS data 
  ats<-NA
  ats.full<-data.frame()
  if('ATS' %in% mans){
    
    for (i in 1:length(ATS_usrs)){
    collar::ats_login(ATS_usrs[i], ATS_pass[i])
    sns<-id_df[id_df$Brand == "ATS",]$Serial
    sns<-paste0("0", sns) # missing leading zero
    
    out.acct <- collar::fetch_ats_positions(device_id = sns)
    
  
    
    # ats timezones are programmed to individual collars, all of our as of Jan 2021 are programmed to Pacific time
    programmed_pacific <- c("043205", "043202", "043204", "043207",
                            "042803", "042804","046977","047204", "047205", "047207", "047208", "047212", "047213", "047214")
    programmed_mountain <- c("030230", "042800", "048782", "048783", "048784", "048785", "048786", "048787", "048788", "048789", "048790", "048791", "048792", "048793", "048794", "048796", "048797") # we can add collar ids as new collars are deployed
    
    #out <- dplyr::bind_rows(out.acct.1, out.acct.2, out.acct.3) 
    
    out.acct$JulianDay = formatC(out.acct$JulianDay, width = 3, format = "d", flag = "0")
    out.acct$Hour = formatC(out.acct$Hour, width = 2, format = "d", flag = "0")
    out.acct$Minute = formatC(out.acct$Minute, width = 2, format = "d", flag = "0")
    
    
    ats <- out.acct %>%
      rename(tdate = "DateLocal", SN = 'CollarSerialNumber', x = 'Longitude', y = 'Latitude') %>%
      select(SN, tdate, x, y)
    ats<-data.frame(ats)
    
    collar::ats_logout()
    
    ats.full<-rbind(ats, ats.full)
    
    
    }
    
  }
  
  vec<-NA
  if('Vectronic' %in% mans){
    
    key_path <- collar::get_paths(veckeys)
    vecdat<-collar::fetch_vectronics(key_path, type = "gps")
    
    vec <- vecdat %>%
      rename(tdate = "acquisitiontime", SN = 'idcollar', x = 'longitude', y = 'latitude') %>%
      select(SN, tdate, x, y)
    
    vec$tdate <-as.POSIXct(vec$tdate,
                                       format = paste0("%Y-%m-%d", "T", "%H:%M:%S"),
                                       tz = "UTC",
                                       origin = vec$tdate)
    vec<-data.frame(vec)
  }
 
  # bind all the data together 
  gps<-rbind(ats, vec, tel)
  
  # add animal IDs to data-- remove data before 2023
  dt<-as.POSIXct("2023-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S")
  gps<-gps[gps$tdate >= dt,]
  
  id_df$Serial<-ifelse(id_df$Brand == "ATS", paste0("0", id_df$Serial), id_df$Serial)
  id_df$IdCol<-id_df$Serial
  
  gps<-gps[gps$SN %in% id_df$Serial,]
  gps<-merge(gps, id_df[, c('Serial', 'VitFreq', 'Frequency', 'Species', 'IdCol', 'AID', 'Sex')], by.x = "SN", by.y = "Serial", all = T)
  
  return(gps)
}
  


  




  
