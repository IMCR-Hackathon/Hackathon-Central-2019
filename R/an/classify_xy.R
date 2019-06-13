#'
#' Detect spatial information in data columns
#'
#' @param list A metajam list output for a single data entity
#' @param x_or_y "x" or "y", dimension to look for
#'
#' @return A logical vector
#'

classify_xy <- function(list, x_or_y) {
  
  # set up conditions depending on whether we are looking for lat or lon
  
  if (x_or_y == "x") {
    name_matches <- paste(c("lon", "lng", "*x", "x$"), collapse = "|")
  } else if (x_or_y == "y") {
    name_matches <- paste(c("lat", "*y", "y$"), collapse = "|")
  } else {
    stop("Please specify either x or y as criteria to classify columns.")
  }
  
  # use metadata if it exists
  
  if ("attribute_metadata" %in% names(list)) {
    unit_matches <- paste(c("deg", "min", "sec"), collapse = "|")
    scale_matches <-
      paste(c("int", "ord", "rat"), collapse = "|")
    
    use_metadata <- function(attr_row) {
      
      # defining condition layers
      name_cond <-
        grepl(name_matches, c(attr_row[["attributeName"]]))
      scale_cond <-
        grepl(scale_matches,
              c(attr_row[["measurementScale"]]))
      unit_cond <-
        grepl(unit_matches, c(attr_row[["unit"]]))
      
      if (scale_cond) {
        if (name_cond) {
          if (unit_cond) {
            return(T)
          } else
            return("possible")
        } else
          return(F)
      } else
        return(F)
    }
    xy_or_not <-
      apply(list[["attribute_metadata"]], 1, use_metadata)
  } else {
    
    # only use data if metadata is not available
    
    data_only <- function (column) {
      name_cond <-
        grepl(name_matches, colnames(column))
      type_cond <- is.numeric(column)
      range_cond_x <- min(column) >= -180 & max(column) <= 180
      range_cond_y <- min(column) >= -90 & max(column) <= 90
      
      if (name_cond & type_cond) {
        if (x_or_y == "x") {
          if (range_cond_x) {
            return(T)
          }
        } else if (x_or_y == "y") {
          if (range_cond_y) {
            return(T)
          }
        }
      } else {
        return(F)
      }
    }
    xy_or_not <- apply(list[["data"]], 2, data_only)
  }
  return(xy_or_not)
}
