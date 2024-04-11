    
#' Clean and Validate Data Frame
#'
#' This function checks a data frame for required columns, ensuring it's not empty,
#' and all required columns are present. It returns a cleaned data frame with only the required columns.
#'
#' @param data_frame A data frame that needs to be cleaned and validated.
#' @param required_columns A character vector of column names that are required in the data frame.
#'
#' @return A cleaned data frame containing only the required columns given.
#' @export
#'
#' @examples
#' required_columns <- c("shares", "num_hrefs", "num_imgs", "num_videos")
#' share_data <- data.frame(color = c("red","yellow","yellow"),shares = c(1,2,3), 
#' num_hrefs = c(1,2,3), num_imgs = c(1,2,3), num_videos = c(1,2,3),is_pop = c(0,0,0))
#' clean_data_frame <- clean_data(share_data, required_columns)
#'
 clean_data <- function(data_frame,required_columns){
   
   # Check if data_frame is a data frame
   if (!is.data.frame(data_frame)) {
     stop("`data_frame` should be a data frame or data frame extension (e.g. a tibble)")
   }
   
   # Check if required_columns is empty
   if (length(required_columns) == 0) {
     stop("The `required_columns` argument must contain at least one column name")
   }
   

   # Check if the data frame is empty
   if (nrow(data_frame) == 0) {
     stop("The data frame should not be empty")
   }
   
   # Check if all required_columns are in the data_frame
   if (!all(required_columns %in% names(data_frame))) {
     missing_cols <- required_columns[!required_columns %in% names(data_frame)]
     stop(paste("The data frame does not contain all the required columns. Missing columns:", paste(missing_cols, collapse = ", ")))
   }
   

   clean_data_frame <- data_frame |>
     dplyr::select(all_of(required_columns)) 

   return(clean_data_frame)

}

