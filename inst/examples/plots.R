library(nonLinearDotPlot)

# creating data.frame with two columns
x <- c(0, 0, 1, 1, 2, 
       2, 2, 2, 2, 3, 
       3, 3, 3, 3, 4, 
       4, 4, 5, 5, 5)
letter <- c("a", "b", "c", "c", "b",
           "b", "a", "b", "c", "a",
           "a", "a", "c", "a", "c",
           "a", "b", "c", "c", "a")
data <-  data.frame(x, letter)

# small margin around all plots
margin <- c(2.5,0.5,0.5,0.5)
# set the x axis limits for all plots
limits <- c(-0.5, 5.5)

# Simple plot with no axis description
nonLinearDotPlot(data = data, mar=margin,
                 xlim = limits, xlab ="",
                 xAttribute = "x")

# Dots are smaller
nonLinearDotPlot(data = data, mar=margin,
                 xlim = limits, xlab ="",
                 xAttribute = "x",
                 dSingle = 0.5)

# Plot adapts aspect ratio to device size
nonLinearDotPlot(data = data, mar=margin,
                 xlim = limits, xlab ="",
                 xAttribute = "x",
                 useDeviceAsp = TRUE)

# Logarithmic scaling function
nonLinearDotPlot(data = data, mar=margin,
                 xlim = limits, xlab ="",
                 xAttribute = "x",
                 dotscaling=dotscaling.log(3),
                 useDeviceAsp = TRUE)

# Linear scaling function
nonLinearDotPlot(data = data, mar=margin,
                 xlim = limits, xlab ="",
                 xAttribute = "x",
                 dotscaling=dotscaling.linear(),
                 useDeviceAsp = TRUE)

# Uniformly Changed color
nonLinearDotPlot(data = data, mar=margin,
                 xlim = limits, xlab ="",
                 xAttribute = "x",
                 colors = "Red")

# Simple coloring of dots depending on their table entry of "letter" in the data
colors <- c("#a6cee3", "#1f78b4", "#7570b3")

nonLinearDotPlot(data = data, mar=margin,
                 xlim = limits, xlab ="",
                 xAttribute = "x",
                 colorAttribute = "letter",
                 colors = colors)

# Change order of the stacked colors
colors <- c("#a6cee3", "#1f78b4", "#7570b3")
colorPositions <- c(1, 1/2, 0)

nonLinearDotPlot(data = data, mar=margin,
                 xlim = limits, xlab ="",
                 xAttribute = "x",
                 colorAttribute = "letter",
                 colors = colors,
                 colorPositions = colorPositions)

# Map specific color to each value in "letter"
colors <- c("#a6cee3", "#1f78b4", "#7570b3")
colorAttributeMapDict <- c('a' = 1, 'b' = 1/2, 'c' = 0)

colorAttributeMap <- function(names){
  return(colorAttributeMapDict[names])
}

nonLinearDotPlot(data = data, mar=margin,
                 xlim = limits, xlab ="",
                 xAttribute = "x",
                 colorAttribute = "letter",
                 colors = colors,
                 colorAttributeMap = colorAttributeMap)

# Use simulated blur
nonLinearDotPlot(data = data, mar=margin,
                 xlim = limits, xlab ="",
                 xAttribute = "x",
                 useBlur = TRUE,
                 blurEdge = 1,
                 blurGapDistance = 2)

