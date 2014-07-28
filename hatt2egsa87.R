hatt2egsa87 <- function(sphatt, msid){
    # Converts Greek military HATT projection to Greek national grid (EGSA87)
    # applying the necessary mapsheet-specific transformations.
    require(rgdal)
    require(maptools)
    egsa87 <- "+proj=tmerc +lat_0=0 +lon_0=24 +k=0.9996 +x_0=500000 +y_0=0 +ellps=GRS80 +towgs84=-199.87,74.79,246.62,0,0,0,0 +units=m +no_defs"
    sps <- c("SpatialPoints","SpatialPointsDataFrame","SpatialLines","SpatialLinesDataFrame","SpatialPolygons","SpatialPolygonsDataFrame")
    
    if (!class(sphatt) %in% sps){
        stop("Input data must be a Spatial* class of points, lines or polygons")
    }

    if (class(sphatt) %in% sps[c(2,4,6)]){
        df <- sphatt@data
    }
    
    transf <- read.csv("hattcoeffs.csv", header=TRUE)
    mapsheet <- transf[transf$MSID==msid,,drop=FALSE]

    if (class(sphatt) %in% sps[1:2]){
        attach(mapsheet)
        x <- coordinates(sphatt)[,1]
        y <- coordinates(sphatt)[,2]
        GR87x <- round(A0 + A1 * x + A2 * y + A3 * x^2 + A4 * y^2 + A5 * x * y, 3)
        GR87y <- round(B0 + B1 * x + B2 * y + B3 * x^2 + B4 * y^2 + B5 * x * y, 3)
        detach(mapsheet)
        res <- data.frame(X=GR87x,Y=GR87y)
        coordinates(res) <- ~X+Y
        res <- SpatialPointsDataFrame(res, data=df)
        proj4string(res) <- egsa87
        
    } else if (class(sphatt) %in% sps[3:4]){
        nmplys <- length(sphatt@lines)
        mplist<- vector("list", nmplys)
        for (a in 1:nmplys){
            nplys <- length(sphatt@lines[[a]]@Lines)
            plist <- vector("list", nplys)
            for (b in 1:nplys){
                xy <- sphatt@lines[[a]]@Lines[[b]]@coords
                attach(mapsheet)
                x <- coordinates(xy)[,1]
                y <- coordinates(xy)[,2]
                GR87x <- round(A0 + A1 * x + A2 * y + A3 * x^2 + A4 * y^2 + A5 * x * y, 3)
                GR87y <- round(B0 + B1 * x + B2 * y + B3 * x^2 + B4 * y^2 + B5 * x * y, 3)
                detach(mapsheet)
                plist[b] <- Line(cbind(GR87x, GR87y))
            }
            mplist[a] <- Lines(plist, as.character(a))
        }
        res <- SpatialLines(mplist, proj4string=CRS(egsa87))
        if (class(sphatt) %in% sps[c(2,4,6)]){
            row.names(df) <- as.character(1:nmplys)
            res <- SpatialLinesDataFrame(res, data=df)  
        }

    } else {
        nmplys <- length(sphatt@polygons)
        mplist<- vector("list", nmplys)
        for (a in 1:nmplys){
            nplys <- length(sphatt@polygons[[a]]@Polygons)
            plist <- vector("list", nplys)
            for (b in 1:nplys){
                xy <- sphatt@polygons[[a]]@Polygons[[b]]@coords
                attach(mapsheet)
                x <- coordinates(xy)[,1]
                y <- coordinates(xy)[,2]
                GR87x <- round(A0 + A1 * x + A2 * y + A3 * x^2 + A4 * y^2 + A5 * x * y, 3)
                GR87y <- round(B0 + B1 * x + B2 * y + B3 * x^2 + B4 * y^2 + B5 * x * y, 3)
                detach(mapsheet)
                plist[b] <- Polygon(cbind(c(GR87x,GR87x[1]), c(GR87y,GR87y[1])), hole=sphatt@polygons[[a]]@Polygons[[b]]@hole)
            }
            mplist[a] <- Polygons(plist, as.character(a))
        }
        res <- SpatialPolygons(mplist, proj4string=CRS(egsa87))
        if (class(sphatt) %in% sps[c(2,4,6)]){
            row.names(df) <- as.character(1:nmplys)
            res <- SpatialPolygonsDataFrame(res, data=df)
        }
    }
    
    return(res)
}

