#' Batch aggregate raster objects 
#' 
#'@description
#' Takes a raster object and a vector of aggrgation values calculates resampling a writes to a folder
#' 
#'This function mainly exists as it seems difficult to use parallel cores to resample a raster object. Hence paralle the 
#'different scale functions and use the outputs in seperate functions.  
#'@param inRaster A raster object to be resampled
#'@param scaleVector A vector of the different resampling factors to be applied
#'@param outNames a string of folder location and stack names used for writing outputs, will have the aggregation values appended
#'@param NoCores Number of Cores to used to parallel processing 
#'@return raster objects written to file following resampling. 
#'@examples
#'  #resolutionVector  <-2:8
#'  #outName <- "F:\\Projects\\shrub_cover_paper\\pixelSizeTests\\ras_stack"
#'  #returns:  "F:\Projects\shrub_cover_paper\pixelSizeTests\ras_stack8.tif"  "F:\Projects\shrub_cover_paper\pixelSizeTests\ras_stack7.tif" etc 


BatchAggregate <- function(inRaster,scaleVector, outNames, NoCores){
  
  cl <- makeCluster(mc <- getOption("cl.cores", NoCores))
  registerDoParallel(cl, cores=NoCores)
  clusterExport(cl=cl, varlist=c("inRaster","scaleVector", "outNames"),envir=environment())
  clusterEvalQ(cl, library(raster))

  foreach(i=scaleVector)%dopar%{
    filenam <- paste0(outNames,i,".tiff")
    raster::aggregate(x = cropedstack,fact = i,FUN = mean, filename=filenam ,bylayer=FALSE)}
  
  stopCluster(cl)

}




