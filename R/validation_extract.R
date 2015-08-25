#' Extract Validation Values 

#'@description
#'Takes and input dataframe and containing co-ordinates and actual(validation) values, extracts corresponding 
#'predicted values and return a long format data frame 

#'@param validationData data.frame containing the validation values in format "x","y","pc.cov"
#'@param rasterLayers raster object of the variable to extract.
#'@param pointCRS CRS object for use with the validationData points.

#'@return a dataframe containing the extracted values, plus actual values and cell numbers. In long format 

validation_extract <-function(validationData,rasterLayers,pointCRS){

#load csv and convert to point file

xy <- SpatialPoints(validationData[,1:2],proj4string = pointCRS)
xy <-spTransform(x = xy,CRSobj =crs(rasterLayers))

#extract measured values
#extract raster values
raster_values <- extract(rasterLayers, xy,cellnumbers=TRUE)
names(raster_values) <- names(rasterLayers)

all_values <- cbind(validationData[,3],raster_values)
all.df  <- as.data.frame(all_values)

all_values.melt <- melt(data = all.df,measure.vars = c(names(rasterLayers)))
all_values.melt <- rename(all_values.melt, c("V1"="Actual", "value"="Predicted","variable"="Model"))

return(all_values.melt)
}





