#' Test the affect of changing the resolution of the training data and resulting prediction on model performance 
#' 
#'@description
#' 
#' Test how changing a model resolution affect model performance. 


#'@param inRaster A raster object to be resamples 
#'@param 
#'@param 
#'@param 
#'@param 
#'@return 

# rasterStack
# 
# NoCores, list,fromVals, toVals, numSamps
# 
# SA_AEAC_CRS
# 
# SpatialAggregationTest  <- function(train,trainingY,testY,testX,sizes,NoCores){
#   
#   
#   
#   # parallel aggregate
#   
# rasterEngine()  
#   
# # parallel create csv
#  All.data  <-  csv_batch_parallel(NoCores, list, predImageI, fromVals, toVals, numSamps)
# 
#  
# # extract point values, split training and testing data
# xy.dats  <- SpatialPoints(coords = All.data[,1:2])
# crs(xy.dats)  <- SA_AEAC_CRS
# 
# xy.dats <-spTransform(x = xy.dats,CRSobj =crs(predImageI)
#                       
#                       All.data.extracts  <-cbind(All.data, extract(predImageI, y = xy.dats)) 
#                       
# set.seed(3456)
# trainIndex <- createDataPartition(All.data.extracts$pc, p = .5,list = FALSE,                                                        times = 1)
#     PCtrain <- All.data.extracts[ trainIndex,]
#     PCtest  <- All.data.extracts[-trainIndex,]
#   
#   
#   
#   
# }