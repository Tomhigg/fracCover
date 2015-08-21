#' Percentage Cover Calculations-Full Function 

#' This function is identical to "percent_cover_paralell" but omits the write to raster command to make easier integration in loops 


percent_cover_parallel_fortest<-function(no_cores,inImage, pointdata,LS.no.data,point_CRS){
# Start processing
print("Set variables and start processing")
startTime <- Sys.time()
cat("Start time", format(startTime),"\n")


cl <- makeCluster(spec = no_cores)
# Register the cluster with foreach:
registerDoParallel(cl)

# Load the moderate resolution image
for (b in 1:nlayers(inImage)) { NAvalue(inImage@layers[[b]]) <- LS.no.data}

#load point data
#point.csv <- read.csv(file = pointData,header = TRUE)
xy <- SpatialPoints(pointdata[,1:2],proj4string = point_CRS)
xy <-spTransform(x = xy,(crs(inImage)))
response <- as.numeric(pointdata[,3])


# Get pixel DNs from the input image for each sample point
print("Getting the pixel values under each point")
trainvals <- cbind(response, extract(inImage, xy)) 

# Remove NA values from trainvals
trainvals_no_na <- na.omit(trainvals)

# Run Random Forest
print("Starting to calculate random forest object")

randfor <- randomForest(response ~. , data=trainvals_no_na)


print("Enter the Matrix")
randomForest_raster_predict <- function(inraster,rfModel,...)
{
  # We need to load randomForest (note this is only
  # loaded once per worker on most cluster setups):
  require(randomForest)
  
  # First, preserve the names:
  band_names <- dimnames(inraster)[3][[1]]
  
  # This will “flatten” the array to a matrix (we lose the names here):
  inraster_matrix <- inraster
  dim(inraster_matrix) <- c(dim(inraster)[1]*dim(inraster)[2],dim(inraster)[3])
  
  # Now, let’s coerce this to a data.frame:
  inraster.df <- as.data.frame(inraster_matrix)
  
  # We need to re-set the names because randomForest requires it:
  names(inraster.df) <- band_names
  
  # Now we can use this in a predict statement:
  out_predictions <- predict(rfModel,inraster.df)
  
  # We must return this as a numeric array (a raster cannot
  # use character classes), and set the dims to match the input.
  # Also, right now rasterEngine requires ALL outputs to be double
  # precision floating point, so we cannot use “as.numeric” on the
  # factor, because that will return an integer.
  
  out_predictions_array <- array(as.double(out_predictions),dim=c(dim(inraster)[1:2],1))
  
  return(out_predictions_array)
}

# Now, rasterEngine. Notice we pass the randomForest model along
# via the args= setting.
system.time(
  
  cover_output_array <-
    rasterEngine(
      # Match the variable name in the function to the raster:
      inraster=inImage,
      # Assign the function:
      fun=randomForest_raster_predict,
      args=list(rfModel= randfor)
    ))
print("Unplug")
cover_output <- setMinMax(cover_output_array)


stopCluster(cl) # Stops the cluster

# Calculate processing time
timeDiff <- Sys.time() - startTime
cat("Processing time", format(timeDiff), "\n")
return(cover_output)

}
