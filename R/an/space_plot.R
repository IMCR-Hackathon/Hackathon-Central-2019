space_plot <- function (list, var) {
  
  detected <- space_detective(list)
  
  if (is.vector(detected) & length(detected) == 2) {
    x_col <- list[["data"]][[detected[["lon_col"]]]]
    y_col <- list[["data"]][[detected[["lat_col"]]]]
    
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