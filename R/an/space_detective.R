#'
#' the space detective
#' 
#' Wrapper for \code{\link{classify_xy}}. Runs \code{\link{classify_xy}}, counts number of hits in each dimension, then decides what to depending on those two numbers. 
#'
#' @param list Metajam output for a single data entity
#' 
#' @return A character vector whose two elements are named "lat_col" and "lon_col", indicating the names of columns containing lat and lon information respectively, or an error message if not detected or if we aren't equipped to deal with the number of hits we got. 
#'

space_detective <- function(list) {
  x_detect <- classify_xy(list, "x")
  y_detect <- classify_xy(list, "y")
  
  # count hits, assuming all logical. TODO: what to do with possible hits
  
  x_hits <- sum(as.logical(unlist(x_detect)), na.rm = T)
  y_hits <-
    sum(as.logical(unlist(y_detect)), na.rm = T)
  
  # no hits at all
  
  if (x_hits == 0 & y_hits == 0) {
    return(
      "Space detective was not able to detect columns containing spatial information. Spatial plots will not be generated."
    )
  }
  
  # equal hits in both dimensions
  
  if (x_hits == y_hits) {
    
    # single pair
    
    if (x_hits == 1 & y_hits == 1) {
      # get target column names
      x_col <- list[["attribute_metadata"]][which(x_detect), "attributeName"]
      y_col <- list[["attribute_metadata"]][which(y_detect), "attributeName"]
      
      message(
        paste("Space detective found a single pair of columns containing spatial information. \n Latitude column: ", y_col, "\n Longitude column: ", x_col, "\n")
      )
      
      cols <- c(y_col, x_col)
      names(cols) <- c("lat_col", "lon_col")
      return(cols)
    } else {
      
      # equal hits and larger than 1
      
      message("Space detective found equal and larger than 1 numbers of x and y columns. What to do with this information pending. Meanwhile, spatial plots will not be generated.")
      
      }
  } else {
    
    # unequal number of hits
    
    message("Space detective found unequal numbers of x and y columns. What to do with this information pending. Meanwhile, spatial plots will not be generated.")
    }
}