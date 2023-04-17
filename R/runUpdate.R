#' @title runUpdate
#' @description wrapper function for running sheep update and emailing
#' @param id_df data.frame of lookup information
#' @param tempdir temporary directory to save files
#' @param veckeys directory to vectronics key files
#' @param ATS_usrs vector of ATS usernames
#' @param ATS_pass vector of ATS passwords. The order of passwords must match that of the usernames
#' @param lotek_usrs vector of Lotek usernames
#' @param lotek_pass vector of Lotek passwords. The order of passwords must match that of the usernames
#' @param subsetmonth beginning month to subset GPS data to. Default is 2 (February)
#' @param study Name of study area. Only used for subject in update email
#' @param markdowndir directory to the Rmd file for the report
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
runUpdate<-function(id_df, tempdir, veckeys = NA, ATS_usrs = NA, ATS_pass = NA, lotek_usrs = NA, lotek_pass = NA, subsetmonth = "02", study = NA, markdowndir = NA){
 
  x<-happybirthday::getData(id_df = id_df, tempdir = tempdir, veckeys = veckeys, ATS_usrs = ATS_usrs, ATS_pass =ATS_pass, lotek_usrs= lotek_usrs, lotek_pass = lotek_pass)
  
  # make maps
  happybirthday::makeMaps(tempdir = tempdir, gpsdat = x, id_df = id_df)
  
  # create movement metrics
  move<-happybirthday::moveMetrics(gpsdat = x, subsetmonth = subsetmonth)
  
  rollmean = move
  
  happybirthday::makeMarkdown(tempdir = tempdir, rollmean = rollmean, id_df = id_df, subsetmonth = subsetmonth)
  
  
  happybirthday::renderMarkdown(tempdir = tempdir, markdowndir = markdowndir)
  
  #attachments. This is going to include the ParturitionMetrics PDF and html files if you want them to send as well.
  attach = c(paste0(tempdir, "/Products/Last3Days.html"), paste0(tempdir, "/Products/LatestLocs.kml"), paste0(tempdir, "/ParturitionMetrics.pdf"))
  
  mailR::send.mail(from = from,
                   to = to,
                   subject = paste0("Parturition Model Updated for ", study, " ", Sys.time()),
                   body = "This email contains the latest parturition model run.",
                   authenticate = TRUE,
                   smtp = list(host.name = "mail.gmx.com", port = 587, user.name = user, passwd = pass, tls = T), attach.files = attach)
  
}
