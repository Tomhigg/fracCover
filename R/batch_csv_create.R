#' Batch Generate CSV Training Files

#'@param raster_path A list of paths to the classified high-resolution imagery (use list.files)
#'@param inPredImage Name and path for the input image that will be used for predictions (Landsat/MODIS)
#'@param fromVals vector of the values in classified image
#'@param toVals vector which values will be changes too 
#'@param ndPred No data value for the prediction image (normally 0)
#'
#'@description
#'
#'Preforms a batch implementation of the csv_create function, allowing multiple high-res images to to processed into training files
#'at the same time.

#'@examples
#'Landsat_SA_CRS <-"..//Landsat_SA_CRS//Landsat_SA_AEA.tif"
#'shrub_mask_list <-list.files(path = "F:\\Projects\\Shrub_cover\\NGI_aerial_imagery\\shrub_masks\\",pattern = "*tif$",full.names = TRUE)
#'set.seed(seed = 33)
#'percent_cover <-csv_batch(raster_path = shrub_mask_list,inPred = Landsat_SA_CRS,numSamps = 5000)

#' Class numbers that will be mapped using the following scheme:
#'    0 = no data such as background, clouds and shadow
#'    1 = class for which percent cover is being calculated 
#'    2 = all other land cover classes
#' fromVals <- c(0,1, 2, 3)
#' toVals <-   c(2,1, 2, 2)

csv_batch <- function(raster_path,inPred,numSamps, fromVals,toVals){
  mat <- sapply(X = raster_path,FUN = create_csv, inPred=inPred,numSamps=numSamps,fromVals=fromVals,toVals=toVals)
  
  len  <- length(raster_path)
  
  x <- as.vector(mat[1:(nrow(x = mat)/3),1:len])
  y <- as.vector(mat[((nrow(x = mat)/3)+1):((nrow(x = mat)/3)*2),1:len])
  pc <- as.vector(mat[(((nrow(x = mat)/3)*2)+1):(nrow(x = mat)),1:len])
  pc.df <- data.frame(x,y,pc) 
  return(pc.df)

}



