% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mergeDataFrameList.R
\name{merge_dataframe_list}
\alias{merge_dataframe_list}
\title{Custom merge wrapper}
\usage{
merge_dataframe_list(data_frame_list, matching_key)
}
\arguments{
\item{data_frame_list}{A list of dataFrames to be merged into a single df}

\item{matching_key}{The unique var to be used to merge the dataFrames}
}
\value{
A merged dataFrame
}
\description{
This function merges a list of dataFrames by a common key variable
using base merge and purrr::reduce
}
\examples{
merge_dataframe_list(list_of_dataframes, key_var)
}
