## Example conversion using shapefiles ##

# Assuming you have downloaded the repo and set the wd:
# setwd("~/Desktop/hatt2egsa87") #MacOS

library(mpatools)
library(rgdal)
source("hatt2egsa87.R")

# Import and convert points
nyms <- readOGR("examples/nyms", "nyms")
nymsegsa87 <- hatt2egsa87(nyms, 178)

# Import and convert polygons
surv <- readOGR("examples/surv", "surv")
survegsa87 <- hatt2egsa87(surv, 178)

# Plot
plot(survegsa87, col="grey75", border="grey50", axes=TRUE)
points(nymsegsa87, pch=19, cex=0.4, col="red")
title(main="The centre of the Greek island of Kythera")

