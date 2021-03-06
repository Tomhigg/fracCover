% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/batch_csv_parallel.R
\name{csv_batch_parallel}
\alias{csv_batch_parallel}
\title{Parallel  Generate CSV Training Files}
\usage{
csv_batch_parallel(no_cores, list, predImage, fromVals, toVals, numSamps)
}
\arguments{
\item{no_cores}{number of cores to implement parallel on}

\item{list}{A list of paths to the classified high-resolution imagery (use list.files)}

\item{fromVals}{vector of the values in classified image}

\item{toVals}{vector which values will be changes too}

\item{numSamps}{Number of samples to select from each images}

\item{inPredImage}{Name and path for the input image that will be used for predictions (Landsat/MODIS)}
}
\description{
Preforms a parallel batch implementation of the csv_create function, allowing multiple high-res images to to processed into training files
at the same time.
}

