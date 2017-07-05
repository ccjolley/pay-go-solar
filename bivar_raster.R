library(raster)
library(dplyr)
library(grDevices)
library(ggplot2)
library(llamar)

###############################################################################
# Convert an (x,y) pair to an RGB triplet
###############################################################################
xy_to_col <- function(x,y) {
  c1 <- colorRamp(c(lo_x_lo_y,lo_x_hi_y),bias=1,space='Lab',
                  interpolate='linear',alpha=FALSE)(y)
  rgb1 <- rgb(c1[1],c1[2],c1[3],maxColorValue=255)
  c2 <- colorRamp(c(hi_x_lo_y,hi_x_hi_y),bias=1,space='Lab',
                  interpolate = 'linear',alpha=FALSE)(y)
  rgb2 <- rgb(c2[1],c2[2],c2[3],maxColorValue=255)
  c3 <- colorRamp(c(rgb1,rgb2),bias=1,space='Lab',
                  interpolate = 'linear',alpha=FALSE)(x)
  c3
}

###############################################################################
# Convert an (x,y) pair to a hex color string
###############################################################################
xy_to_colstr <- function(x,y) {
  c3 <- xy_to_col(x,y)
  rgb(c3[1],c3[2],c3[3],maxColorValue=255)
}

###############################################################################
# Use consistent palettes
###############################################################################
set_palette <- function(x) {
  if (x == 'mobile') {
    hi_x_hi_y <<- '#574249'
    hi_x_lo_y <<- '#c85a5a'
    lo_x_hi_y <<- '#64acbe'
    lo_x_lo_y <<- '#e8e8e8'
  }
  if (x == 'electric') {
    hi_x_hi_y <<- '#804d36'
    hi_x_lo_y <<- '#9972af'
    lo_x_hi_y <<- '#c8b35a'
    lo_x_lo_y <<- '#e8e8e8'
  }
}

###############################################################################
# Make a bivariate raster 
###############################################################################
make_raster <- function(f1,f2="C:/Users/Craig/Desktop/Live projects/bivar-raster/landscan-pop/w001001x.adf",
                        palette = 'mobile',outname) {
  set_palette(palette)
  r1 <- raster(f1)
  r2 <- raster(f2) %>%
    crop(r1) %>%
    resample(r1) %>%
    mask(r1)
  q90 <- function(x,na.rm) {
    quantile(x,probs=c(0.05,0.95),na.rm=na.rm)
  }
  q1 <- cellStats(r1,q90)
  q2 <- cellStats(r2,q90)
  clipscale <- function(q,x) {
    if (x < q[1]) return(0)
    if (x > q[2]) return(1)
    (x - q[1])/(q[2]-q[1])
  }
  r <- g <- b <- raster(extent(r1),nrows=nrow(r1),ncols=ncol(r1),crs=crs(r1))
  r1vals <- getValues(r1)
  r2vals <- getValues(r2)
  rvals <- gvals <- bvals <- rep(NA,length(r1vals))
  ti <- Sys.time()
  for (i in 1:length(r1vals)) {
    if (is.na(r1vals[i]) | is.na(r2vals[i])) {
      rvals[i] <- gvals[i] <- bvals[i] <- NA
    }
    else {
      norm1 <- clipscale(q1,r1vals[i])
      norm2 <- clipscale(q2,r2vals[i])
      c <- xy_to_col(norm2,norm1)
      rvals[i] <- c[1]
      gvals[i] <- c[2]
      bvals[i] <- c[3]
    }
  }
  print(Sys.time()-ti)
  r <- setValues(r,rvals)
  g <- setValues(g,gvals)
  b <- setValues(b,bvals)
  s <- brick(r,g,b)
  plotRGB(s)
  writeRaster(s,outname,'GTiff')
}

###############################################################################
# Make some rasters
###############################################################################

setwd("C:/Users/Craig/Desktop/Live projects/Pay-go solar/hh survey data/Tanzania DHS 2016")
make_raster('tza_mobile.tif',outname='tza_mpop.tif')
make_raster('tza_elect.tif',palette='electric',outname='tza_epop.tif')

setwd("C:/Users/Craig/Desktop/Live projects/Pay-go solar/hh survey data/Nigeria DHS 2013")
make_raster('nga_mobile.tif',outname='nga_mpop.tif')
make_raster('nga_elect.tif',palette='electric',outname='tza_epop.tif')

setwd("C:/Users/Craig/Desktop/Live projects/Pay-go solar/hh survey data/Zambia DHS 2013-4")
make_raster('Zambia-mobile.tif',outname='zmb_mpop.tif')
make_raster('Zambia-electric.tif',palette='electric',outname='zmb_epop.tif')

setwd("C:/Users/Craig/Desktop/Live projects/Pay-go solar/hh survey data/Rwanda DHS 2015")
make_raster('Rwanda-mobile.tif',outname='rwa_mpop.tif')
make_raster('Rwanda-electric.tif',palette='electric',outname='rwa_epop.tif')
