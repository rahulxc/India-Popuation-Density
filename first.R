install.packages("tidyverse")
library(raster)
library(tidyverse)


population.raster <- raster("indp00ag.asc")
population.points <- rasterToPoints(population.raster)

all.data <- as.data.table(population.points)
setnames(all.data, c("x", "y", "population"))

head(all.data)


startEnd <- function(lats, lngs) {
  # Find the "upper left" (NW) and "bottom right" (SE) coordinates 
  # of a set of data.
  #
  # Args:
  #  lats: A list of latitude coordinates
  #  lngs: A list of longitude coordinates
  #
  # Returns: 
  #   A list of values corresponding to the northwest-most and 
  # southeast-most coordinates
  
  # Convert to real number and remove NA values
  lats <- na.omit(as.numeric(lats))
  lngs <- na.omit(as.numeric(lngs))
  
  topLat <- max(lats)
  topLng <- min(lngs)
  botLat <- min(lats)
  botLng <- max(lngs)
  
  return(c(topLat, topLng, botLat, botLng))
  
  
  
  startEndVals <- startEnd(all.data$y, all.data$x)
  remove(startEnd)
  
  
  startLat <- startEndVals[1]
  endLat <- startEndVals[3]
  startLng <- startEndVals[2]
  endLng <- startEndVals[4]
  remove(startEndVals)
  
  
  
  interval.v.num = 200.0
  interval.h.num = 800.0
  interval.v <- (startLat - endLat) / interval.v.num
  interval.h <- (endLng - startLng) / interval.h.num
  remove(num_intervals)
  
  
  lat.list <- seq(startLat, endLat + interval.v, -1*interval.v)
  
  
  # testLng <- -66.66152983 # Fredericton
  # testLat <- 45.96538183 # Fredericton
  
  # Prepare the data to be sent in
  # If you have a value you want to sum, use this
  data <- all.data[,list(x, y, population)]
  
  # If you want to perform a count, use this
  # data <- all.data[,list(x, y)]
  # data[,Value:=1]
  
  sumInsideSquare <- function(pointLat, pointLng, data) {
    # Sum all the values that fall within a square on a map given a point,
    # an interval of the map, and data that contains lat, lng and the values
    # of interest
    
    setnames(data, c("lng", "lat", "value"))
    
    # Get data inside lat/lon boundaries
    lng.interval <- c(pointLng, pointLng + interval.h)
    lat.interval <- c(pointLat - interval.v, pointLat)
    data <- data[lng %between% lng.interval][lat %between% lat.interval]
    
    return(sum(data$value))
  }

  
  
  calcSumLat <- function(startLng, endLng, lat, data) {
    row <- c()
    lng <- startLng
    while (lng < endLng) {
      row <- c(row, sumInsideSquare(lat, lng, data))
      lng <- lng + interval.h
    }
    
    return(row)
  }  
  
  
  
  
  cl <- makeCluster(detectCores(), outfile = "./Progress.txt")
  registerDoParallel(cl)
  
  all.sums <- foreach(lat=lat.list, .packages=c("data.table")) %dopar% {
    
    lat.data <- calcSumLat(startLng, endLng, lat, data)
    
    # Progress indicator that works on Mac/Windows
    print((startLat - lat)/(startLat - endLat)*100) # Prints to Progress.txt
    
    lat.data
    
  }
  
  stopCluster(cl = cl)
  
  # Convert to data frame
  all.sums.table <- as.data.table(all.sums)
  
  
  # Save to disk so I don't have to run it again
  if (!file.exists("./GeneratedData")) {
    dir.create("./GeneratedData")
  }
  output.file <- "./GeneratedData/India.csv"
  write.csv(all.sums.table, file = output.file, row.names = FALSE)
  
  data
  View(head(arrange(data,desc(y,x))))
  
  ggplot(aes(data,y, x)) +
    geom_segment(size=0.4, alpha=0.8, color='#5A3E37') +
    ggthemes::theme_map() +
    coord_equal(0.9)
  
  max(data$population)
  
  library(graphics)
  install.packages("tcltk")
  library(tcltk)
  install.packages("pracma")
  library(pracma)
  
  # remove(cl, endLat, endLng, startLat, startLng, lat.list, start, calcSumLat, sumInsideSquare, interval)
  
  
plot.data <- read.csv("GeneratedData/India.csv", header=TRUE, stringsAsFactors=FALSE)

top.padding <- 1:5
for (i in top.padding) {
  plot.data <- cbind(0, plot.data)
}
# On bottom
bottom.padding <- 1:1
for (i in bottom.padding) {
  plot.data <- cbind(plot.data, 0)
}

# On left
zero.row <- vector(mode="integer", length=dim(plot.data)[1])

left.padding <- 1:10
for (i in left.padding) {
  plot.data <- rbind(zero.row, plot.data)
}

# On right
right.padding <- 1:10
for (i in left.padding) {
  plot.data <- rbind(plot.data, zero.row)
}

max <- max(plot.data) # Max value in the data, used for scaling
plottingHeight <- 1000 # Arbitrary number that provides the graph's height
scaleFactor <- 300 # Discovered through trial and error to keep the graph in the boundaries
gap <- plottingHeight / length(plot.data) # Space between lines

# Output the file to a 36 inch by 24 inch SVG canvas
plot.width = 36
plot.height = 26.5


svg(filename = "./India.svg", pointsize=12, width=plot.width, height=plot.height)

yVals <- as.vector(plot.data[[1]] / max * scaleFactor)
plot(0, 0, xlim=c(0, length(yVals)), ylim=c(0,1100), type="n", las=1, xlab=NA, ylab=NA, bty="n", axes=FALSE)

plotting.threshold <- 0.1

plot.length = length(plot.data)

# Plot each line
for (i in 1:plot.length) {
  # Grabs a row of data
  yVals <- as.vector(plot.data[[i]] / max * scaleFactor)
  xVals <- seq_along(yVals)
  # yVals.smooth =  savgol(yVals, 3, forder=4)
  
  # polygon(xVals, yVals + plottingHeight, border = NA, col = "#ffffff")
  lines(xVals, yVals + plottingHeight, col=c("red", "blue"), lwd=0.5, type="o", cex=1 )
  
  plottingHeight <- plottingHeight - gap
  
}

dev.off()