#'
#' the Space plot
#' 
#' Create variable specific spatial heatmaps to show data availability and data summary over the geographical study area. 
#' 
#' @param space_cols A character vector whose two elements are named "lat_col" and "lon_col". Outpur from \code{\link{space_detective}}
#' @param var Name of variable to make data availability and summary. Remember to enclose in quotes. 
#' 
#' @return For all variables, function returns a data availability plot over the study area. For categorical column types, function returns an extra plot showing most prevalent level of that variable in each spatial grid cell over the study area. For numeric column types, function returns an extra plot showing mean of that variable in each spatial grid cell over the study area. Lat and lon are binned; there are 50 bins normalized to attempt to enforce equal scales along each dimension.

space_plot <- function (space_cols = space_cols, var) {
  
  if (is.vector(space_cols) & length(space_cols) == 2) {
    x_col <- list[["data"]][[space_cols[["lon_col"]]]]
    y_col <- list[["data"]][[space_cols[["lat_col"]]]]
    
    xrange <-
      max(x_col, na.rm = T) - min(x_col, na.rm = T)
    yrange <-
      max(y_col, na.rm = T) - min(y_col, na.rm = T)
    
    xbreak <- round(50 / (xrange + yrange) * xrange)
    ybreak <- round(50 / (xrange + yrange) * yrange)
    
    non_na <-
      aggregate(
        list[["data"]][[var]],
        by = list(
          lat = cut(y_col, breaks = yrange),
          lng = cut(x_col, breaks = xrange)
        ),
        FUN = function(x) {
          sum(!is.na(x))
        }
      )
    
    non_na_plot <-
      ggplot(non_na, aes(
        x = as.factor(lng),
        y = as.factor(lat),
        fill = x
      )) +
      geom_tile(colour = "white", size = 0.25) +
      theme(
        axis.text.x = element_text(angle = 60, hjust = 1),
        plot.background = element_blank(),
        panel.border = element_blank()
      ) +
      scale_fill_gradient(low="blue", high="red",
                          na.value="transparent") +
      coord_fixed(ratio = 1) +
      labs(title = paste0("How much data are there in variable \"", var, "\" over the study area?"),
           x = "lon (binned)",
           y = "lat (binned)") +
      guides(fill = guide_legend(paste0("Count of non-NAs in \n variable \"", var, "\"")))
    
    print(non_na_plot)
    
    if (is.numeric(list[["data"]][[var]])) {
      avg <-
        aggregate(
          list[["data"]][[var]],
          by = list(
            lat = cut(y_col, breaks = yrange),
            lng = cut(x_col, breaks = xrange)
          ),
          FUN = function(x) {
            mean(x, na.rm = T)
          }
        )
      
      avg_plot <-
        ggplot(avg, aes(
          x = as.factor(lng),
          y = as.factor(lat),
          fill = x
        )) +
        geom_tile(colour = "white", size = 0.25) +
        theme(
          axis.text.x = element_text(angle = 60, hjust = 1),
          plot.background = element_blank(),
          panel.border = element_blank()
        ) +
        scale_fill_gradient(low="blue", high="red",
                            na.value="transparent") +
        coord_fixed(ratio = 1) +
        labs(title = paste0("What is the average of numeric variable \"", var, "\" over the study area?"),
             x = "lon (binned)",
             y = "lat (binned)") +
        guides(fill = guide_legend(paste0("Mean of \n variable \"", var, "\"")))
      
      print(avg_plot)
    } else if (is.factor(list[["data"]][[var]]) | is.character(list[["data"]][[var]])) {
      prev <-
        aggregate(
          list[["data"]][[var]],
          by = list(
            lat = cut(y_col, breaks = yrange),
            lng = cut(x_col, breaks = xrange)
          ),
          FUN = function(x) {
            names(which.max(table(x)))
          }
        )
      
      prev_plot <-
        ggplot(prev, aes(
          x = as.factor(lng),
          y = as.factor(lat),
          fill = x
        )) +
        geom_tile(colour = "white", size = 0.25) +
        theme(
          axis.text.x = element_text(angle = 60, hjust = 1),
          plot.background = element_blank(),
          panel.border = element_blank()
        ) +
        coord_fixed(ratio = 1) +
        labs(title = paste0("What is the most common level in categorical variable \"", var, "\" over the study area?"),
             x = "lon (binned)",
             y = "lat (binned)") +
        guides(fill = guide_legend(paste0("Level in \n variable \"", var, "\"")))
      
      print(prev_plot)
    }
  }
}