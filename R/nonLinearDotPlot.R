#' Creates a non linear dot plot.
#' 
#' @description Non linear dot plots are diagrams that allow dots of varying size to be constructed,
#' so that columns with a large number of samples are reduced in height. An efficient two-way
#' sweep algorithm is used to obtain a dense and symmetrical layout.
#'
#' @references N. Rodrigues and D. Weiskopf, "Nonlinear Dot Plots",
#' IEEE Transactions on Visualization and Computer Graphics, vol. 24, no. 1, pp. 616-625, 2018.
#' Available: \doi{10.1109/TVCG.2017.2744018}
#'
#' @param data Input data frame.
#' @param ... Extra arguments are put into the R plot function. Using these may result in bad plots.
#' @param xAttribute The name or the index of the header to use as X axis of the plot.
#' @param colorAttribute The name or index of the header to use as reference for the colors of
#' the plot.
#' @param colors Vector of the colors that will be used to color the dots. Colors can
#' be specified either by name (e.g: "red") or as a hexadecimal RGB triplet (e.g: "#FFCC00").
#' \href{https://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/col2rgb.html}{See more.}
#' @param colorPositions Vector of values between 0 and 1 that defines the relative positions of
#' each color used in the `colors` vector on the x axis of the plot.
#' If `colorPositions` is equal NULL, the colors used in `colors` will be equidistant.
#' The length of `colors` and `colorPositions` must be equal.
#' @param colorAttributeMap Function to map the color values of the plot. If set to NULL,
#' the color values will be sorted by their default order and converted to numbers if not numeric.
#' @param dSingle Constant start diameter used by the two-way sweep algorithm that facilitates
#' an overall scaling of all dots. (see reference)
#' @param asp Forces the plot to a specific aspect ratio. Set to NULL to disable it.
#' @param asplim The number of iterations allowed for the aspect ratio approximation. The larger 
#' the value is, the more time is needed for the calculation.
#' @param useDeviceAsp If it is set to TRUE, plot generation uses the aspect ratio of the available graphical output area.
#' This will override the `asp` parameter.
#' @param circlePadding Value between 0 and 1 that scales the dots of the plot by the given percentage.
#' @param main Title of the plot.
#' @param sub Subtitle for the plot.
#' @param xlab Title for the x axis. If set to NULL, the name of the column is used.
#' @param useBlur Applies a simulated vertical Blur to all dots.
#' @param blurEdge The number of dots at the top and bottom of every column in the plot that
#' will be left unchanged when blur is activated.
#' @param blurGapDistance Specifies how far away a column must be from the surrounding columns so that
#' the blur will not be effective on it.
#' @param xlim Vector (xMin,xMax) specifies the limits for the x axis, where xMin is the smallest
#' value and xMax is the biggest value of the x axis.
#' @param xTicks Vector of the values at which tick-marks are to be drawn. if set to NULL, tick-mark
#'  locations will be computed and set automatically.
#' @param xAxisMargin Specifies the margin between the x Axis and the plot.
#' @param dotscaling Function responsible for the calculation of the circle diameter depending on the number
#' of data values of the corresponding column and `dSingle`. These premade functions can be applied:
#' `dotscaling.root(e)`, `dotscaling.linear()`, and `dotscaling.log(base)`.
#' A custom scaling function can be used as well.
#' For more details, please refer to the chapter 4.3 of the referenced paper \doi{10.1109/TVCG.2017.2744018}.
#' @param mar Vector with margins around the plot in inches: (bottom, left, top, right). Set to NULL to use the standard margin of the R plot function.
#' 
#' @returns No return value. Plots directly to active device.
#' 
#' @example inst/examples/plots.R
#' 
#' @import grid
#' @import grDevices
#' @import graphics
#' @import utils
#' 
#' @export
nonLinearDotPlot <- function(data, ..., xAttribute=1, colorAttribute=NULL,
                             main=NULL, sub=NULL, xlab=NULL,
                             dSingle=1, circlePadding=0.05,
                             useBlur=FALSE, blurEdge=1, blurGapDistance=2,
                             colors="black", colorPositions=NULL,
                             xlim=NULL, xTicks=NULL, xAxisMargin=0.5,
                             asp=NULL, asplim=10, useDeviceAsp=FALSE,
                             colorAttributeMap=NULL, dotscaling=dotscaling.root(0.3), 
                             mar=NULL){
  
  # --------- user input validation ----------------------
  
  # making sure Par is only changed within function
  oldPar <- par(no.readonly = TRUE)
  on.exit(par(oldPar))
  
  par(mar=mar, ...)
  
  error <- function(msg) {
    stop(paste("Error: ", msg))
  }
  
  #data
  if(is.null(data)) {
    error("Argument data is missing")
  }
  
  if(!is.data.frame(data)) {
    error("data has to be a data frame")
  }
  
  #xAttribute/colorAttribute
  isColumn <- function(attrib) {
    if(is.character(attrib) && attrib %in% colnames(data)) {
      return(TRUE)
    }
    
    if(is.numeric(attrib) && attrib >= 1 && attrib <= length(data)) {
      return(TRUE)
    }
    
    return(FALSE)
  }
  
  #xAttribute
  if(!isColumn(xAttribute)) {
    error("Undefined column selected for x attribute")
  }
  
  if(!is.numeric(data[,xAttribute])) {
    error("xAttribute has to be numeric")
  }
  
  #colorAttribute
  if(!is.null(colorAttribute) && !isColumn(colorAttribute)) {
    error("colorAttribute has to be a valid column or NULL")
  }
  
  #main
  if(!is.null(main) && !is.character(main)) {
    error("main has to be a character vector or NULL")
  }
  
  #sub
  if(!is.null(sub) && !is.character(sub)) {
    error("sub has to be a character vector or NULL")
  }
  
  #xlab
  if(!is.null(xlab) && !is.character(xlab)) {
    error("xlab has to be a character vector")
  }
  
  #dSingle
  if(!is.numeric(dSingle) || dSingle <= 0){
    error("Wrong input for the value dSingle")
  }
  
  #circlePadding
  if(!is.numeric(circlePadding) || circlePadding < 0 || circlePadding >= 1){
    error("circlePadding has to be numeric, bigger or equal then 0 and less then 1")
  }
  
  #useBlur
  if (!is.logical(useBlur)){
    error("Use TRUE or FALSE as condition for useBlur")
  }
  
  #blurEdge
  if(!is.numeric(blurEdge) || blurEdge < 0) {
    error("blurEdge has to be numeric and bigger or equal then 0")
  }
  
  #blurGapDistance
  if(!is.numeric(blurGapDistance) || blurGapDistance < 0) {
    error("blurGapDistance has to be numeric and bigger or equal then 0")
  }
  
  #colors
  if(length(colors) < 1) {
    error("colors needs atleast one color")
  }
  
  isColor <- function(col) {
    sapply(col, function(col) {
      tryCatch(is.matrix(col2rgb(col)), error = function(err) FALSE)
    })
  }
  
  if(!all(isColor(colors))) {
    error("colors contains non color strings")
  }
  
  #colorPositions
  if(!is.null(colorPositions) && length(colors) != length(colorPositions)) {
    error("ColorPositions need to be NULL or the same length as colors")
  }
  
  if((!is.null(colorPositions) && !is.numeric(colorPositions))
     || any(colorPositions < 0) || any(colorPositions > 1)) {
    error("colorPositions have to be numeric and between 0 and 1 or NULL")
  }
  
  #xlim
  if(!is.null(xlim) && !is.numeric(xlim)) {
    error("xlim has to be numeric or NULL")
  }
  
  #xTicks
  if(!is.null(xTicks) && !is.numeric(xTicks)) {
    error("xTicks has to be numeric or NULL")
  }
  
  #xAxisMargin
  if(!is.numeric(xAxisMargin) || xAxisMargin < 0){
    error("xAxisMargin has to be numeric and bigger or equal then 0")
  }
  
  #asp
  if(!is.null(asp) && (!is.numeric(asp) || asp <= 0)){
    error("asp has to be numeric and bigger then 0 or NULL")
  }
  
  #asplim
  if(!is.numeric(asplim) || asplim <= 0) {
    error("asplim has to be numeric and bigger then 0")
  }
  
  #useDeviceAsp
  if(!is.logical(useDeviceAsp)) {
    error("Use TRUE or FALSE as condition for useDeviceAsp")
  }
  
  #colorAttributeMap
  if(!is.null(colorAttributeMap) && !is.function(colorAttributeMap)) {
    error("colorAttributeMap has to be a function or NULL")
  }
  
  #dotscaling
  if(!is.function(dotscaling)) {
    error("dotscaling has to be a function")
  }
  
  #--------- init new plot ---------
  
  #convert column name to a column index because otherwise the xlab will not be displayed
  if(is.character(xAttribute)) {
    xAttribute <- grep(xAttribute, colnames(data))
  }         
  
  #if xlab=NULL use column name instead
  if(is.null(xlab)) {
    xlab <- colnames(data)[xAttribute]
  }
  

  
  #--------- aspect ratio ---------
  
  if(useDeviceAsp) {
    #get size of device
    xSize = abs(grconvertX(1, "ndc", "lines") - grconvertX(0, "ndc", "lines"))
    ySize = abs(grconvertY(1, "ndc", "lines") - grconvertY(0, "ndc", "lines"))
    
    #outer margins
    margins <- par("mar")
    
    #substract margins
    xSize = xSize - margins[2] - margins[4]
    ySize = ySize - margins[1] - margins[3] - xAxisMargin
    
    
    #calculate new aspect ratio
    asp = xSize / ySize
  }
  
  #--------- sweep ---------
  
  # order data increasing
  data <- data [order (data[,xAttribute], decreasing = FALSE),]
  
  #two way sweep with specific dSingle
  #sweep returns a table with diameter, count and position of all columns
  sweep <- function(dSingle) {
    columns <- data.frame()
    
    xPos <- tail(data[,xAttribute], n=1)
    c <- 0
    diameter <- 0
    
    #right to left sweep
    for (index in length(data[,xAttribute]):1) {
      c <- c + 1
      
      diameter <- dSingle * dotscaling(c)
      
      if(index > 1) {
        if(abs(data[index-1,xAttribute] - xPos) > diameter) {
          columns <- rbind(data.frame(
            xPos=xPos,
            count=c,
            diameter=0
          ), columns)
          
          xPos <- data[index-1,xAttribute]
          c <- 0
          diameter <- dSingle
        }
      } else {
        columns <- rbind(data.frame(
          xPos=xPos,
          count=c,
          diameter=0
        ), columns)
      }
    }
    
    xPos <- data[1,xAttribute]
    c <- 0
    
    colIndex <- 1
    overflow <- FALSE
    cTemp <- 0.0
    
    #left to right sweep
    for (index in 1:length(data[,xAttribute])) {
      c <- c + 1
      
      diameter <- dSingle * dotscaling(c)
      
      if(index < length(data[,xAttribute])) {
        if(abs(data[index+1,xAttribute] - xPos) > diameter) {
          columns$xPos[colIndex] <- (columns$xPos[colIndex] + xPos) / 2
          
          cTemp <- (columns$count[colIndex] + c) / 2
          
          if ((cTemp %% 1) != 0) {
            if (overflow) {
              columns$count[colIndex] <- ceiling(cTemp)
              overflow <- FALSE
            } else{
              columns$count[colIndex] <- floor(cTemp)
              overflow <- TRUE
            }
          } else {
            columns$count[colIndex] <- cTemp
          }
          
          columns$diameter[colIndex] = dSingle * dotscaling(columns$count[colIndex])
          
          xPos <- data[index+1,xAttribute]
          c <- 0
          diameter <- dSingle
          colIndex <- colIndex + 1
        }
      } else {
        columns$xPos[colIndex] <- (columns$xPos[colIndex] + xPos) / 2
        
        cTemp <- (columns$count[colIndex] + c) / 2
        if(overflow) {
          columns$count[colIndex] <- ceiling(cTemp)
        } else {
          columns$count[colIndex] <- cTemp
        }
        columns$diameter[colIndex] = dSingle * dotscaling(columns$count[colIndex])
      }
    }
    
    return(columns)
  }
  
  #calculate first columns with dSingle
  columns <- sweep(dSingle)
  
  # get radius from diameter, also adds padding
  getRadius <- function(diameter) {
    return((diameter / 2) * (1-circlePadding))
  }
  
  
  # draw an empty frame and setup a new plot
  plot(c(min(data[,xAttribute]), max(data[,xAttribute])), c(0, 0),
       main=main, sub=sub, xlab=xlab,
       ylab="", yaxt='n', xaxt='n', xlim=xlim, cex=0, ...)
  
  #draw x axis
  # axis(1, at=xTicks)
  
  #used to force an specific aspect ratio
  if(!is.null(asp)) {
    
    #get the length of the x Axis
    xAxis <- abs(grconvertX(1, from = "npc", to = "user") - grconvertX(0, from = "npc", to = "user"))
    
    #helper function to get the size of the tallest column
    maxColumn <- function(columns) {
      maxCol <- 0
      
      for(i in 1:length(columns$xPos)) {
        maxCol <- max(maxCol, columns$count[i] * columns$diameter[i])
      }
      
      return(maxCol)
    }
    
    #distance between x axis and first dot in user units
    marUsr <- abs(grconvertX(xAxisMargin, from = "lines", to = "user") - grconvertX(0, from = "lines", to = "user"))
    
    #helper function to calculate the aspect ratio
    calcAsp <- function(columns) {
      return(xAxis / (maxColumn(columns) + marUsr))
    }
    
    currAsp <- calcAsp(columns)
    
    #step by which to multiply each step
    step <- ifelse(currAsp < asp, 0.5, 2)
    
    interval <- c(dSingle, dSingle)
    colMin <- columns
    colMax <- columns
    
    #calculate inital interval
    while(calcAsp(colMin) < asp) {
      interval[1] = 0.5*interval[1]
      colMin <- sweep(interval[1])
    }
    
    while(calcAsp(colMax) > asp) {
      interval[2] = 2*interval[2]
      colMax <- sweep(interval[2])
    }
    
    #defines when the approximation is good enough
    epsilon <- 0.05
    
    aspcount <- 0
    
    #approximates dSingle
    while((calcAsp(colMax) + epsilon) < asp) {
      newPos <- ((interval[2] - interval[1]) / 2) + interval[1]
      newCol <- sweep(newPos)
      
      if(calcAsp(newCol) <= asp) {
        interval[2] <- newPos
        colMax <- newCol
      } else {
        interval[1] <- newPos
        colMin <- newCol
      }
      
      aspcount <- aspcount + 1
      if(aspcount >= asplim) {
        break
      }
    }
    
    #use bigger dSingle / smaller aspect ratio because the y axis should still fit
    columns <- colMax
  }
  
  #draw x axis
  axis(1, at=xTicks)
  
  
  #--------- normalize color values and calculate the colors ---------
  
  #use color column or default color if color column is not defined
  if(!is.null(colorAttribute)) {
    rv <- data[,colorAttribute]
  } else {
    rv <- rep(0, length(data[,xAttribute]))
  }
  
  #determine if colorAttributeMap is used and initlalize accordingly
  if(!is.null(colorAttributeMap)) {
    rv <- colorAttributeMap(rv)
  }
  
  #Sort non numerics by their default order and convert them to numbers
  if(!is.numeric(rv)) {
    cValues <- sort(unique(rv))
    
    indices <- 1:length(cValues)
    names(indices) <- cValues
    
    rv <- unname(indices[rv])
  }
  
  #normalize all values
  minValue <- min(rv)
  maxValue <- max(rv)
  
  if(maxValue - minValue != 0) {
    rv <- (rv - minValue) / (maxValue - minValue)
  } else {
    #if there is no difference between min and max take lowest possible color
    rv <- rep(0, length(rv))
  }
  
  #ignore mapColor as a user input
  mapColor <- NULL
  
  #define default mapColor
  if(is.null(mapColor)) {
    #check if equidistant intervals should be generated
    if(is.null(colorPositions)) {
      #create equidistant intervals
      if(length(colors) > 1) {
        colorPositions <- (0:(length(colors) - 1)) / (length(colors) - 1)
      } else {
        colorPositions <- c(0,1)
        colors <- c(colors, colors)
      }
    } else {
      
      #prepare colorPositions for the mapColor function
      
      #Sort all color positions ascending
      permutation <- order(colorPositions)
      colorPositions <- colorPositions[permutation]
      colors <- colors[permutation]
      
      #Append 0 and 1 to colorPositions for easier calculation
      if(colorPositions[1] != 0) {
        colors <- c(colors[1], colors)
        colorPositions <- c(0, colorPositions)
      }
      
      if(colorPositions[length(colorPositions)] != 1) {
        colors <- c(colors, colors[length(colorPositions)])
        colorPositions <- c(colorPositions, 1)
      }
    }
    
    #Default for mapColor
    mapColor <- function(values) {
      result <- c()
      
      #iterate over all intervals
      for(i in 2:length(colorPositions)) {
        #limits of currently selected interval
        prev <- colorPositions[i - 1]
        curr <- colorPositions[i]
        
        #color ramp for this interval
        ramp <- colorRamp(c(colors[i - 1], colors[i]))
        
        #true at all positions of values that are within the interval false otherwise
        selection <- (prev <= values) & (values < curr)
        
        #normalize values and calculate color for all selected values
        color <- ramp((values[selection] - prev) / (curr - prev))
        
        #convert to color string and add it to the result
        result[selection] <- rgb(color[,1] / 0xFF, color[,2] / 0xFF, color[,3] / 0xFF)
      }
      
      #Add color sepeatly for 1 because the upper limit is not included in the last interval
      result[values == 1] <- colors[length(colors)]
      
      return (result)
    }
  }
  
  #--------- calc coords ---------
  
  x <- c() #x coordinate of the points (circles)
  y <- c() # the y coordinate of each circle belonging to each x corrdinate of a circle
  s <- c() # the size of each circle
  v <- c() # normalised color value
  
  for(index in 1:length(columns$xPos)) {
    x <- c(x, rep(columns$xPos[index], columns$count[index]))
    
    radius <- getRadius(columns$diameter[index])
    
    y <- c(y, 0:(columns$count[index]-1))
    
    s <- c(s, rep(radius, columns$count[index]))
    
    v <- c(v, sort(rv[(length(v)+1):length(x)]))
  }
  
  #---------------------init grid viewports--------------------------------
  
  #initalize viewports
  margins <- par("mar")
  #1: bottom 2:left 3:top 4:right
  mb <- grid::unit(margins[1], "lines")
  ml <- grid::unit(margins[2], "lines")

  mt <- grid::unit(margins[3], "lines")
  mr <- grid::unit(margins[4], "lines")
  
  #create viewport equivalent to margins in par
  grid::pushViewport(grid::viewport(x = ml, y = mb,
                                    width = grid::unit(1, "npc") - ml - mr, height = grid::unit(1, "npc") - mb - mt,
                                    just=c("left", "bottom"), clip=TRUE))
  
  #add xAxisMargin to viewport
  xAxisMargin <- grid::unit(xAxisMargin, "lines")
  grid::pushViewport(grid::viewport(x = grid::unit(0, "npc"), y = xAxisMargin,
                                    width = grid::unit(1, "npc"), height = grid::unit(1, "npc") - xAxisMargin,
                                    just=c("left", "bottom"), clip=TRUE))
  
  #max (inverse) aspect ration for whitch the graph is "properly" drawn
  #everything above this is unusable anyway due to figure margins and because the human eye can not see a thing anymore
  maxasp <- 1000000
  
  #create a viewport that is really high to trick snpc into always using the x axis
  grid::pushViewport(grid::viewport(x = grid::unit(0, "npc"), y = grid::unit(0, "npc"),
                                    width = grid::unit(1, "npc"), height = grid::unit(maxasp, "npc"),
                                    just=c("left", "bottom"), clip=FALSE))
  
  #---------------------plot helper functions--------------------------------
  
  circle <- function(x, y, r, col) {
    #convert user units to npc
    r <- grid::unit(abs(grconvertX(r, "user", "npc") - grconvertX(0, "user", "npc")), "snpc")
    x <- grid::unit(grconvertX(x, "user", "npc"), "npc")
    
    #draw circle in npc units
    grid::grid.circle(x=x, y=r + (2*y*r), r=r, gp=grid::gpar(col="NA", fill=col))
  }
  
  rectangle <- function(x, y, width, height, col) {
    #convert user units to npc
    width <- grid::unit(abs(grconvertX(width, "user", "npc") - grconvertX(0, "user", "npc")), "snpc")
    x <- grid::unit(grconvertX(x, "user", "npc"), "npc")
    
    #draw rect in npc units
    grid::grid.rect(x=x, y=0.5*width + (y*width), width=width, height=height*width, just=c("centre", "bottom"), gp=grid::gpar(col="NA", fill=col))
  }
  
  gradient <- function(x, y, r, col) {
    for(i in 1:(length(col)-1)) {
      colmin <- col[i]
      colmax <- col[i+1]
      
      ymin <- y + (i-1)
      ymax <- ymin + 1
      
      #calculate how many colors are between colmin and colmax
      rgbmin <- col2rgb(colmin)
      rgbmax <- col2rgb(colmax)
      maxdif <- max(abs(rgbmin[1] - rgbmax[1]),
                    abs(rgbmin[2] - rgbmax[2]),
                    abs(rgbmin[3] - rgbmax[3])) + 1
      
      #create a ramp with a specific amount of colors
      ramp <- colorRampPalette(c(colmin, colmax))(maxdif)
      
      #calculate step positions
      ys <- ((0:(maxdif-1)) / maxdif)
      
      #let them overlap to prevent white lines between layers caused by rounding errors
      height <- 1 - ys
      
      #draw segments longer to prevent white lines between segments caused by rounding errors
      if(i < (length(col)-1)) {
        height <- height + 0.5
      }
      
      #draw rectangles
      rectangle(rep(x, maxdif), ys + ymin, rep(2*r, maxdif), height, ramp)
    }
  }
  
  #---------------------plot--------------------------------
  
  mappedColors <- mapColor(v)
  
  #vars used for blur calculation
  ux <- NULL
  sx <- NULL
  ex <- NULL
  
  #columns that are ignored from blur
  ignoreColumn <- NULL
  
  if(useBlur) {
    #init vars used for blur calculation
    ux <- unique(x)
    ignoreColumn <- rep(FALSE, length(ux))
    
    #find start and end index of each column
    sx <- match(ux, x)
    ex <- length(x) - match(ux, rev(x)) + 1
    
    #determine columns that are ignored from blur
    if(length(ux) > 1 && blurGapDistance > 0) {
      for(i in 1:(length(ux)-1)) {
        #get x and radius of current and next column
        currX <- ux[i]
        nextX <- ux[i+1]
        
        currR <- s[sx[i]]
        nextR <- s[sx[i+1]]
        
        #calculate distance between the circles from 2 adjacent columns
        distance <- max(0, abs(nextX - currX) - currR - nextR)
        
        #use smaller diameter for calculation
        dia <- 2*min(currR, nextR)
        
        if(distance >= (dia*blurGapDistance)) {
          ignoreColumn[i] <- TRUE
          ignoreColumn[i+1] <- TRUE
        }
      }
    }
  }
  
  #floor blurEdge
  blurEdge <- floor(blurEdge)
  
  #draw circles
  edges <- 1:(blurEdge+1)
  
  for(i in seq(1, length(x))) {
    #draw all non blured circles
    if(!useBlur || ignoreColumn[match(x[i], ux)] #create circle if blur is disabled or column ignored
       || any(i == edges) || any(i == (length(x) - edges + 1)) #first and last dots will never be blured
       || any(x[i-edges] != x[i]) || any(x[i] != x[i+edges])) { #dont draw dots in blured areas
      circle(x[i], y[i], s[i], mappedColors[i])
    }
  }
  
  #draw gradients
  if(useBlur) {
    for(i in 1:length(ux)) {
      #check if there are enough points to blur or the line is ignored
      if((ex[i] - sx[i]) > (2*blurEdge) && !ignoreColumn[i]) {
        gradient(ux[i], blurEdge, s[ex[i]], mappedColors[(sx[i]+blurEdge):(ex[i]-blurEdge)])
      }
    }
  }
  
  #---------------------deinit plot--------------------------------
  
  #pop all viewports
  grid::popViewport()
  grid::popViewport()
  grid::popViewport()
}

#----------------------Dot diameter functions----------------------

#' Predefined function to use as `dotscaling` in the `nonLinearDotPlot` function.
#' @description dotscaling(c) = 1 / (c**e) is a root function
#'
#' @param e determines which root should be used
#' Default value of e equals 0.3
#'
#' @references N. Rodrigues and D. Weiskopf, "Nonlinear Dot Plots",
#' IEEE Transactions on Visualization and Computer Graphics, vol. 24, no. 1, pp. 616-625, 2018.
#' Available: \doi{10.1109/TVCG.2017.2744018}
#'
#' @returns Function to calculate dot size with 1 / (c ** e).
#'
#' @export
dotscaling.root <- function(e=0.3) {
  return(function(c) {
    return(1 / (c ** e))
  })
}

#' Predefined function to use as `dotscaling` in the `nonLinearDotPlot` function.
#' @description dotscaling(c) = 1 is a linear function
#'
#' @references N. Rodrigues and D. Weiskopf, "Nonlinear Dot Plots",
#' IEEE Transactions on Visualization and Computer Graphics, vol. 24, no. 1, pp. 616-625, 2018.
#' Available: \doi{10.1109/TVCG.2017.2744018}
#'
#' @returns Function that always returns 1.
#'
#' @export
dotscaling.linear <- function() {
  return(function(c) {
    return(1)
  })
}

#' Predefined function to use as `dotscaling` in the `nonLinearDotPlot` function.
#' @description dotscaling(c) = (log(c + base - 1) / log(base)) / c is a logarithmic function
#'
#' @param base value of the base of the logarithm
#' Default value of base equals 2
#'
#' @references N. Rodrigues and D. Weiskopf, "Nonlinear Dot Plots",
#' IEEE Transactions on Visualization and Computer Graphics, vol. 24, no. 1, pp. 616-625, 2018.
#' Available: \doi{10.1109/TVCG.2017.2744018}
#'
#' @returns Function to calculate dot size with (log(c + base - 1) / log(base)) / c.
#'
#' @export
dotscaling.log <- function(base = 2) {
  return(function(c) {
    return((log(c + base - 1) / log(base)) / c)
  })
}
