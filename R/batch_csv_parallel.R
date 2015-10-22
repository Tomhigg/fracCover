#' Parallel  Generate CSV Training Files
#' 
#' 
#'@param no_cores number of cores to implement parallel on 
#'@param list A list of paths to the classified high-resolution imagery (use list.files)
#'@param inPredImage Name and path for the input image that will be used for predictions (Landsat/MODIS)
#'@param fromVals vector of the values in classified image
#'@param toVals vector which values will be changes too 
#'@param numSamps Number of samples to select from each images
#'
#'@description
#'
#'Preforms a parallel batch implementation of the csv_create function, allowing multiple high-res images to to processed into training files
#'at the same time.

csv_batch_parallel  <- function(no_cores, list, predImage, fromVals, toVals,numSamps){

cl <- makeCluster(mc <- getOption("cl.cores",no_cores ))
varlist  <- c("list", "predImage","fromVals", "toVals","numSamps")
clusterExport(cl=cl, varlist, envir=environment())
clusterEvalQ(cl, library(raster))

mat <- parSapply(cl = cl,X = list,FUN = create_csv, inPred=predImage, fromVals=fromVals,toVals=toVals,numSamps=numSamps)
len  <- length(list)
x <- as.vector(mat[1:(nrow(x = mat)/3),1:len])
y <- as.vector(mat[((nrow(x = mat)/3)+1):((nrow(x = mat)/3)*2),1:len])
pc <- as.vector(mat[(((nrow(x = mat)/3)*2)+1):(nrow(x = mat)),1:len])
pc.df <- data.frame(x,y,pc) 

stopCluster(cl)

return(pc.df)
}