#' Random Forest Percent Cover Modelling   
#' 
#'@description
#' This script reads training data from the CSV file created using the "csv_create/batch_csv" 
#' script.  The script then uses the X and Y coordinates from the training data file to select
#' the pixel values (predictor values) for each sample point in the input image. The predictor 
#' values and the percent cover data from the training data file (response variable) are 
#' combined and used as input to the random forests model. After the model is created percent 
#' cover predictions are made on the input image to create an output image with percent cover 
#' values ranging from 0 to 1. 

#'@param inImage A raster stack of Landsat (or other) data
#'@param pointdata Data frame of training data from the csv_create function, does not need to be projected, cols 1,2 are x/y 3 is pc
#'@param LS.no.data No data value for the Landsat image, normally 0 
#'@param outImage. Name and path for the output classified image 
#'@param point_CRS A CRS file for the points data-not needed if the the CRS is equal 
#'@return a raster image classified into percentage coverage

percent_cover_model<-function(inImage, pointdata,outImage,LS.no.data,point_CRS){
# Start processing
print("Set variables and start processing")
startTime <- Sys.time()
cat("Start time", format(startTime),"\n")

# Load the moderate resolution image
satImage <-stack(inImage)
for (b in 1:nlayers(satImage)) { NAvalue(satImage@layers[[b]]) <- LS.no.data}

#load point data
xy <- SpatialPoints(pointdata[,1:2],proj4string = point_CRS)
xy <-spTransform(x = xy,(crs(satImage)))
response <- as.numeric(pointdata[,3])

# Get pixel DNs from the input image for each sample point
print("Getting the pixel values under each point")
trainvals <- cbind(response, extract(satImage, xy)) 

# Remove NA values from trainvals
trainvals_no_na <- na.omit(trainvals)

# Run Random Forest
print("Starting to calculate random forest object")

randfor <- randomForest(response ~. , data=trainvals_no_na)

cover_output<- predict(satImage, randfor, filename=outImage, progress='text', format='GTiff', datatype='FLT4S', type='response', overwrite=TRUE)

return(cover_output)

# Calculate processing time
timeDiff <- Sys.time() - startTime
cat("Processing time", format(timeDiff), "\n")}
