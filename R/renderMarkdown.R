#' @title renderMarkdown
#' @description render Rmarkdown file with plots and most recent locatiosn 
#' @param tempdir temporary directory to store output
#' @param markdowndir directory to the Rmarkdown file for creating reports
#' @return returns a pdf in tempdir with all collated plots and recent locations
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[rmarkdown]{render}}
#' @rdname renderMarkdown
#' @export 
#' @importFrom rmarkdown render
renderMarkdown<-function(tempdir, markdowndir){
  
  datastore<-paste0(tempdir, "/Tables/RecentLocs.RDS")
  prettydatastore<-paste0(tempdir, "/PrettyData/PrettyData.RDS")
  plotfolder<-paste0(tempdir, "/Plots")
  pdffolder<-paste0(tempdir, "/PDFs")
  if(!dir.exists(pdffolder)){
    dir.create(pdffolder)
  }
  
  hj<-readRDS(prettydatastore)
  hj$MatchFreq<-ifelse(nchar(hj$MatchFreq)<6,paste0(hj$MatchFreq,'0'),hj$MatchFreq)
  
  fn<-data.frame(datastore=datastore,prettydatastore=prettydatastore,
                 pathloc=paste0(tempdir,'/path.RDS'),stringsAsFactors=F)
  saveRDS(fn,paste0(tempdir,'AllPaths.RDS'))
  
  path<-plotfolder

  llist<-list.files(path,full.names=T)

  finlist<-vector()
  for(i in 1:length(llist)){
    for(k in 1:length(hj$MatchFreq)){
      tes = grepl(hj$MatchFreq[k], llist[i])
      tes = TRUE %in% tes
      if(tes == TRUE){
        finlist<-c(finlist,llist[i])
      }
    }
  }
  finlist<-finlist[!duplicated(finlist)]
  llist<-finlist
  
  
  for(i in 1:length(llist)){
    
    
    
    fn<-data.frame(datastore=datastore,prettydatastore=prettydatastore,
                   pathloc=paste0(tempdir,'path.RDS'),plotpath=llist[i],
                   vhist=id_df,stringsAsFactors=F)
    
    
    sb<-gsub(plotfolder,'',llist[i])
    sb<-gsub('.png','',sb)
    
    if((i+1)<10){
      of<-paste0(paste0(pdffolder,'/100'),i+1,'.pdf')
    }
    if((i+1)>=10){
      of<-paste0(paste0(pdffolder,'/10'),i+1,'.pdf')
    }
    if((i+1)>=100){
      of<-paste0(paste0(pdffolder,'/1'),i+1,'.pdf')
    }
    
    #will need to change path to PartPlot RMD file
    rmarkdown::render(input=markdowndir,
                      output_format = 'pdf_document',
                      output_file=of,
                      params=list(tabby=fn[1,1],
                                  ll=fn[1,2],
                                  plotlink=fn[1,4],
                                  basepath=paste0(plotfolder,'/')),quiet=F)
    
    print(i)
    
  }
  
  c<-list.files(pdffolder,full.names=T)
  c<-c('pdftk',c,'output',paste0(tempdir,'/ParturitionMetrics.pdf'))
  
  c<-paste(c,collapse=' ')
  system(c)
  

}
