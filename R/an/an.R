## An: SANDBOX
## Module goal: given a list output from metajam, (1) detect which data columns contain spatial information, and (2) output a series of variable-specific spatial heatmaps to indicate presence/absence of data.
## Working assumptions:
### Only lat/lon spatial data

library(metajam)

library(inspectdf)
library(ggplot2)
library(dplyr)

metajam::download_d1_data(
  "https://cn.dataone.org/cn/v2/resolve/urn%3Auuid%3A43f82ba7-6c5c-4d9d-84f6-b4162537e043",
  "D:/git repos/hackathon_2019/subsistence"
)

data <-
  read_d1_files(
    "D:/git repos/hackathon_2019/subsistence/doi_10.5063_F1TD9VKD__ASFDB_FullPermit__csv"
  )


classify_xy <- function(list, x_or_y) {
  
  matches <- if (x_or_y == "x") {
    paste(c("lon", "lng", "*x", "x$"), collapse = "|")
  } else if (x_or_y == "y") {
    paste(c("lat", "*y", "y$"), collapse = "|")
  } else {
    stop("Please specify either x or y as criteria to classify columns.")
  }
  
  unit_matches <- paste(c("deg", "min", "sec"), collapse = "|")
  
  find_by_row <- function(attr_row) {
    # defining condition layers
    name_cond <- grepl(matches, c(attr_row[["attributeName"]]))
    scale_cond <-
      attr_row[["measurementScale"]] %in% c("interval", "ordinal", "ratio")
    unit_cond <- grepl(unit_matches, c(attr_row[["unit"]]))
    
    # nested if else
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
  xy_or_not <- apply(list[["attribute_metadata"]], 1, find_by_row)
  return(xy_or_not)
}

space_detective <- function(list) {
  x_detect <- classify_xy(list, "x")
  y_detect <- classify_xy(list, "y")
  
  # count hits, assuming all logical
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
    } else {message("Space detective found equal and larger than 1 numbers of x and y columns. What to do with this information pending... Meanwhile, spatial plots will not be generated.")}
  } else {message("Space detective found unequal numbers of x and y columns. What to do with this information pending... Meanwhile, spatial plots will not be generated.")}
}

space_plot <- function(list, var){
  detected <- space_detective(list)
  if(is.vector(detected)){
    x_col <- list[["data"]][[detected[["lon_col"]]]]
    y_col <- list[["data"]][[detected[["lat_col"]]]]
    
    xrange <-
      max(x_col, na.rm = T) - min(x_col, na.rm = T)
    yrange <-
      max(y_col, na.rm = T) - min(y_col, na.rm = T)
    
    xbreak <- round(50 / (xrange + yrange) * xrange)
    ybreak <- round(50 / (xrange + yrange) * yrange)
    
    grouped3 <-
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
    
    plot3 <-
      ggplot(grouped3, aes(
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
      labs(title = paste("Presence/absence heatmap of variable", var),
           x = "lat (binned)",
           y = "lon (binned)") +
      guides(fill = guide_legend(paste("Count of non-NAs in", var))
    
    # looking better! we can sorta see the shape of Alaska
    print(plot3)
  }
}

# goal plots: what we are aiming for ------------------------------------------------------------

## first pass ------------------------------
grouped <-
  aggregate(amount ~ lat + lng, data = data$data, function(x) {
    sum(!is.na(x))
  }, na.action = NULL)

plot1 <-
  ggplot(grouped, aes(
    x = as.factor(lng),
    y = as.factor(lat),
    fill = amount
  )) + geom_tile()

# plot turns out to be not informative. geographical range too large
print(plot1)

## second attempt --------------------------
### we will bin lat and lon values and limit the number of bins
grouped2 <-
  aggregate(
    data[["data"]]$amount,
    by = list(
      lat = cut(data[["data"]]$lat, breaks = 20),
      lng = cut(data[["data"]]$lng, breaks = 20)
    ),
    FUN = function(x) {
      sum(!is.na(x))
    }
  )

plot2 <-
  ggplot(grouped2, aes(
    x = as.factor(lng),
    y = as.factor(lat),
    fill = x
  )) + geom_tile()

print(plot2)

## third attempt

### we will safe guard against long and narrow sampling ranges
### by taking into account x and y ranges

xrange <-
  max(data[["data"]]$lng, na.rm = T) - min(data[["data"]]$lng, na.rm = T)
yrange <-
  max(data[["data"]]$lat, na.rm = T) - min(data[["data"]]$lat, na.rm = T)

xbreak <- round(50 / (xrange + yrange) * xrange)
ybreak <- round(50 / (xrange + yrange) * yrange)

grouped3 <-
  aggregate(
    data[["data"]]$amount,
    by = list(
      lat = cut(data[["data"]]$lat, breaks = yrange),
      lng = cut(data[["data"]]$lng, breaks = xrange)
    ),
    FUN = function(x) {
      sum(!is.na(x))
    }
  )

plot3 <-
  ggplot(grouped3, aes(
    x = as.factor(lng),
    y = as.factor(lat),
    fill = x
  )) + geom_tile() + theme(axis.text.x = element_text(angle = 60, hjust = 1))

# looking better! we can sorta see the shape of Alaska
print(plot3)
