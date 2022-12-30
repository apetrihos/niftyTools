#' Custom merge wrapper
#'
#' This function merges a list of dataFrames by a common key variable
#' using base merge and purrr::reduce
#'
#' @param data_frame_list A list of dataFrames to be merged into a single df
#' @param matching_key The unique var to be used to merge the dataFrames
#' @return A merged dataFrame
#' @examples
#'merge_dataframe_list(list_of_dataframes, key_var)
#' @export

merge_dataframe_list <- function(data_frame_list, matching_key) {

  purrr::reduce(data_frame_list,
                function(x, y, key = matching_key) merge(x, y, by = key, all = T))

}
