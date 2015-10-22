#' Easily test models with various variable combination 
#' 
#'@description
#' 
#' Test how different model predictor combinations affect model preformance. Function requires seperate training and test data. Currently only parallel processing using doParalell is 
#' supported, will add a non paralel version soon 
#' 
#'
#'  
#' 

#'@param trainY Vector of dependent variable, used for model training
#'@param trainX dataframe/matrix of predictor variables, used for model training
#'@param testY A vector of dependent variable to be used for model validation (testing)
#'@param testX A data frame of predictor variables used for testing
#'@param varCol a list of the column numbers for each model combination, 
#'@param ModelNames a vector containing the names used for each model combination, should corespond to variables list
#'@param reps number of times to run each model
#'@param NoCores Number of Cores to used to parallel processing 
#'@return A dataframe of model preformance statistics for each sample variable combination.  Metrics returned include coefficient of determination (Rsq), root mean squared error (RMSE), mean error (ME), mean absolute error (MAE).

#'@examples
#'#varCol  <- list(c(4:9),c(4:11),c(10:11),c(12:17),c(12:19),c(18:19),c(4:9,12:17), c(4:19),c(18,19,10,11),c(20,21),c(20,21,12:17),c(20,21,4:9),c(20,21,4:9,12:17),c(4:21))
#'#ModelNames  <- c("Wet SR","Wet SR+VI","Wet VI","Dry SR", "Dry SR+VI","Dry VI","Dry & Wet SR","Dry & Wet SR+VI","Dry & Wet VI","PALSAR","PALSAR & Dry SR","PALSAR & Wet SR","PALSAR & Wet+Dry SR","All" )



VariableComboTest  <- function(trainY,trainX,testY,testX,varCol,ModelNames,reps, NoCores){

#establish cluster
  cl <- makeCluster(mc <- getOption("cl.cores", NoCores))
  registerDoParallel(cl, cores= NoCores)
  clusterExport(cl=cl, varlist=c("trainY","trainX","testY","testX","varCol","ModelNames","reps", "NoCores"),envir=environment())
  clusterEvalQ(cl, library(raster))
  clusterEvalQ(cl, library(Rsenal))
  clusterEvalQ(cl, library(randomForest))
  
  #rep varCol by reps to get number of models to make
  
  varCol  <- rep(x = varCol,times = reps)
  
  #run model tests
  model.test  <-as.data.frame(do.call(rbind, parLapply(cl = cl,X = varCol, 
                                                       fun = function(i){
                                                         # select predictors from the training predictor data frame
                                                         trainX.Sample<- trainX[,i]
                                                         # run random forest on the training sample
                                                         randfor <- randomForest(trainY ~ . , data= trainX.Sample)
                                                         # use the model to predict onto the validation data  
                                                         val.predicted <- stats::predict(randfor,testX[,i])  
                                                         #run regression stats (Marburg Rsenel) and populate data frame   
                                                         stats <- regressionStats(val.predicted, testY, adj.rsq = TRUE, plot = FALSE)
                                                         return(stats)})))
   #stop cluster to prevent melting
  stopCluster(cl)
    #add model names to the dataframe
      ModelNames  <- rep(x = ModelNames,times = reps)
      model.test$Model  <- ModelNames
  
  return(model.test)
}