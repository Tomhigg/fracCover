#' Test the affect of changing sample size on model performance 
#' 
#'@description
#' 
#' Test how changing a model sample size affect model performance. Function requires separate training and test data. Currently only parallel processing using doParalell is 
#' supported, will add a non parallel version soon 
#' 
#'
#'  
#' 

#'@param train A dataframe/matrix containing a dependent variable and set of independent (predictor) variable used for model training
#'@param trainingY Name of the dependent variable
#'@param testY A vector of dependent variable to be used for model validation (testing)
#'@param testX A data frame of predictor variables used for testing, row numbers MUST corespond to the values in testY
#'@param sizes A vector of samples sizes to be tested
#'@param NoCores Number of Cores to used to parallel processing 
#'@return A dataframe of model performance statistics for each sample size test. Metrics returned include coefficient of determination (Rsq), root mean squared error (RMSE), mean error (ME), mean absolute error (MAE).


SampleSizeTest  <- function(train,trainingY,testY,testX,sizes,NoCores){

cl <- makeCluster(mc <- getOption("cl.cores", NoCores))
registerDoParallel(cl, cores=NoCores)
clusterExport(cl=cl, varlist=c("train","trainingY","testY","testX","sizes"),envir=environment())
clusterEvalQ(cl, library(raster))
clusterEvalQ(cl, library(Rsenal))
clusterEvalQ(cl, library(randomForest))


sample.runs  <-as.data.frame(do.call(rbind, parLapply(cl = cl,X = sizes, 
                                                      fun = function(i){
                                                        # select samples from the training data set, i 
                                                        train.sample<- train[sample(nrow(train), 50),]
                                                        #subset the trainingY vector
                                                        trainY  <- train.sample[,trainingY]
                                                        #delect the "TrainingY vector from the df
                                                        train.sample[,trainingY] <- NULL
                                                        # run random forest on the training sample
                                                        randfor <- randomForest( trainY ~ . , data=train.sample)
                                                        # use the model to predict onto the validation data  
                                                        val.predicted <- stats::predict(randfor,testX)  
                                                        #run regression stats (Marburg Rsenel) and populate data frame   
                                                        stats <- regressionStats(val.predicted, testY, adj.rsq = TRUE, plot = FALSE)
                                                        return(stats)})))
stopCluster(cl)
              #Add the sample size vector as data frame variable 
              sample.runs$Sample.No  <- sizes
              return(sample.runs)
}
