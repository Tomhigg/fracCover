% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/SampleSizeTest.R
\name{SampleSizeTest}
\alias{SampleSizeTest}
\title{Test the affect of changing sample size on model performance}
\usage{
SampleSizeTest(train, trainingY, testY, testX, sizes, NoCores)
}
\arguments{
\item{train}{A dataframe/matrix containing a dependent variable and set of independent (predictor) variable used for model training}

\item{trainingY}{Name of the dependent variable}

\item{testY}{A vector of dependent variable to be used for model validation (testing)}

\item{testX}{A data frame of predictor variables used for testing, row numbers MUST corespond to the values in testY}

\item{sizes}{A vector of samples sizes to be tested}

\item{NoCores}{Number of Cores to used to parallel processing}
}
\value{
A dataframe of model performance statistics for each sample size test. Metrics returned include coefficient of determination (Rsq), root mean squared error (RMSE), mean error (ME), mean absolute error (MAE).
}
\description{
Test how changing a model sample size affect model performance. Function requires separate training and test data. Currently only parallel processing using doParalell is
supported, will add a non parallel version soon
}

