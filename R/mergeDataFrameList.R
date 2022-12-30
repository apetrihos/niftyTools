#' Custom merge wrapper
#'
#' This function merges a list of dataFrames by a common key variable
#' using base merge and purrr::reduce
#'
#' @param dataFrameList A list of dataFrames to be merged into a single dataFrame
#' @param matchingKey The unique key to be used to merge the dataFrames
#' @return A merged dataFrame
#' @examples
#' 
#' @export

mergeDataframeList <- function(dataFrameList, matchingKey) {
  
  
  purrr::reduce(dataFrameList,
                function(x, y, key = matchingKey) merge(x, y, by = key, all = T))
  
}