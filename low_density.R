### Better way to do this:
# Randomly sample points from LandScan raster
# keep only those that are within boundaries and have low pop.
# continue until reaching a specified number of unique points
# output dataframe with new points that can be fed into another
# round (to do this iteratively)

library(raster)
library(rgeos)
library(rgdal)
library(maptools)
library(dplyr)
library(ggplot2)
library(FNN)

###############################################################################
# Utility function: Given a lat/long pair, find the distance to the closest 
# point in a dataframe
###############################################################################
closest_dist <- function(lat_,long_,df,omit.zero=TRUE) {
  df_i <- df %>%
    mutate(d2=(lat-lat_)^2 + (long-long_)^2)
  if (omit.zero) {
    df_i <- filter(df_i,d2>0)
  }
  min(df_i$d2) %>% sqrt
}

###############################################################################
# Utility function: Given a spatial data frame, return an ordinary data frame
# with lat/long points
###############################################################################
shp2df <- function(shp) {
  shp@data$id <- rownames(shp@data)
  shp.points = fortify(shp, region="id")
  plyr::join(shp.points, shp@data, by="id")
}

###############################################################################
# Prepare landscan raster
###############################################################################
get_ls <- function(pts_df) {
  xmin <- min(pts_df$long)
  xmax <- max(pts_df$long)
  xbuf <- 0.05*(xmax-xmin)
  ymin <- min(pts_df$lat)
  ymax <- max(pts_df$lat)
  ybuf <- 0.05*(ymax-ymin)
  f <- "C:/Users/Craig/Desktop/Live projects/bivar-raster/landscan-pop/w001001x.adf"
  raster(f) %>% crop(extent(xmin-xbuf,xmax+xbuf,ymin-ybuf,ymax+ybuf))
}

###############################################################################
# Get the points with zero population density that are located within the 
# country boundaries and are not too close to a DHS sample location.
###############################################################################
get_zero_pts <- function(r,shp,npts,pt_df,cutoff=1,dist_cutoff=0.5,seed=12345,
                         check_adm0=TRUE) {
  set.seed(seed)
  res <- data.frame()
  while (nrow(res) < npts) {
    x <- runif(1,min=r@extent@xmin,max=r@extent@xmax)
    y <- runif(1,min=r@extent@ymin,max=r@extent@ymax)
    c <- closest_dist(y,x,pt_df)
    if (c > dist_cutoff) { # check if too close to sample points
      p <- extract(r,cbind(x,y))
      if (!is.na(p) && p < cutoff) { # check for zero population density
        addme <- TRUE
        if (check_adm0) {
          pt <- data.frame(x=x,y=y)
          coordinates(pt) <- ~x+y
          proj4string(pt) <- proj4string(shp)
          if (is.na(sp::over(pt,shp)$ADMIN)) { # check if inside polygon
            addme <- FALSE
          }
        } 
        if (addme) {
          res <- rbind(res,data.frame(lat=y,long=x))
        }
      }
    }
  }
  res
}

###############################################################################
# Kriged surfaces in ArcMap often don't extend all the way to the country's 
# borders, because EBK can't extrapolate beyond the sampled region. Find the
# four points that make a bounding box around the entire country, and estimate
# the target variable at those points using KNN.
# TODO: Fix the code that's moving points; something isn't right here.
###############################################################################
get_bounding_pts <- function(shp,pt_df,varname,buf_scale=0.025) {
  shp_df <- shp2df(shp)
  lat_max <- max(shp_df$lat)
  lat_min <- min(shp_df$lat)
  lat_buf <- buf_scale*(lat_max-lat_min)
  long_max <- max(shp_df$long)
  long_min <- min(shp_df$long)
  long_buf <- buf_scale*(long_max-long_min)
  extrema <- shp_df[c(which.max(shp_df$lat),
                          which.max(shp_df$long),
                          which.min(shp_df$lat),
                          which.min(shp_df$long)),c('lat','long')] %>%
    mutate(lat_=ifelse(lat==lat_min,lat_min-lat_buf,lat),
           lat_=ifelse(lat_==lat_max,lat_max+lat_buf,lat_),
           long_=ifelse(long==long_min,long_min-long_buf,long),
           long_=ifelse(long_==long_max,long_max+long_buf,long_)) %>%
    select(lat_,long_) %>%
    rename(lat=lat_,long=long_)
  # Select optimal value of k for KNN model
  vars <- c('lat','long')
  k_rmse <- sapply(2:50,function(i) {
    knn_i <- knn.reg(pt_df[,vars],y=pt_df[,varname],
                     k=i,algorithm='kd_tree')
    (pt_df[,varname] - knn_i$pred)^2 %>% mean %>% sqrt
  })
  k <- which.min(k_rmse)
  paste0('Using k = ',k) %>% print
  pred_knn <- knn.reg(train=pt_df[,vars],test=extrema[,vars],
                      y=pt_df[,varname],k=k,algorithm='kd_tree')
  extrema[,varname] <- pred_knn$pred
  extrema
}

###############################################################################
## Get "offshore" points for bodies of water -- places where
# LandScan gives an NA value. Ideally these should be close to
# shore so that I'm not kriging the ocean.
###############################################################################
get_shore_pts <- function(r,npts,pt_df,dist_cutoff=0.5,seed=12345) {
  set.seed(seed)
  res <- data.frame()
  while (nrow(res) < npts) {
    x <- runif(1,min=r@extent@xmin,max=r@extent@xmax)
    y <- runif(1,min=r@extent@ymin,max=r@extent@ymax)
    p <- extract(r,cbind(x,y))
    if (is.na(p)) { # should be in the water
      c <- closest_dist(y,x,pt_df)
      if (c < dist_cutoff) { # should be close to shore
        res <- rbind(res,data.frame(lat=y,long=x))
      }
    }
  }
  res
}

###############################################################################
# Load Tanzania-specific data
###############################################################################
tzn_data <- read.csv('tzn_dhs_2016.csv') %>% filter(lat != 0)
ls <- get_ls(tzn_data)
tzn_shp <- readOGR(dsn="./tzn_borders", layer="tzn_borders")

###############################################################################
# Enrich and visualize
###############################################################################
zp <- get_zero_pts(ls,tzn_shp,200,tzn_data,cutoff=2)
extrema <- get_bounding_pts(tzn_shp,tzn_data,'mobile')
shore <- get_shore_pts(ls,1000,tzn_data)

## Zero-point visualization

plot_zero <- function(df,zp) {
  df %>%
    dplyr::select(lat,long) %>%
    mutate(zero=FALSE) %>%
    rbind(zp %>% mutate(zero=TRUE)) %>%
    ggplot(aes(x=long,y=lat,color=zero)) +
    geom_point(size=4) +
    coord_equal()  
}
plot_zero(tzn_data)
  

tzn_data %>%
  dplyr::select(lat,long,mobile) %>%
  rbind(zp %>% mutate(mobile=0)) %>%
  write.csv('tzn_zero.csv',row.names=FALSE)

## Extrema visualization
plot_ext <- function(df,extrema) {
  df %>%
    dplyr::select(lat,long) %>%
    mutate(ex=FALSE) %>%
    rbind(extrema %>% dplyr::select(lat,long) %>% mutate(ex=TRUE)) %>%
    ggplot(aes(x=long,y=lat,color=ex)) +
    geom_point(size=4) +
    coord_equal()
}

tzn_data %>%
  dplyr::select(lat,long,mobile) %>%
  rbind(zp %>% mutate(mobile=0)) %>%
  rbind(extrema) %>%
  write.csv('tzn_zero_ext.csv',row.names=FALSE)

## Shore visualization
plot_shore <- function(df,shp,shore) {
  df_shore <- df %>%
    dplyr::select(lat,long) %>% 
    mutate(water=FALSE) %>%
    rbind(shore %>% mutate(water=TRUE))
  
  shp %>% shp2df %>%
    ggplot(aes(long,lat)) + 
      geom_polygon(fill='gray85') +
      geom_path(color="black") +
      coord_equal() +
      geom_point(data=df_shore,size=2,aes(color=water)) 
}

tzn_data %>%
  dplyr::select(lat,long,mobile) %>%
  rbind(zp %>% mutate(mobile=0)) %>%
  rbind(extrema) %>%
  rbind(shore %>% mutate(mobile=0)) %>%
  write.csv('tzn_zero_ext_water.csv',row.names=FALSE)

###############################################################################
# Uganda
###############################################################################
uga_data <- read.csv('uga_dhs_2011.csv') %>% filter(lat != 0)
ls <- get_ls(uga_data)
uga_shp <- readOGR(dsn="./uga_borders", layer="uga_borders")

zp <- get_zero_pts(ls,uga_shp,200,uga_data,cutoff=1,check_adm0=FALSE) # use cutoff=1 for production
plot_zero(uga_data,zp) # none of them inside Uganda
extrema <- get_bounding_pts(uga_shp,uga_data,'mobile')
extrema_elect <- get_bounding_pts(uga_shp,uga_data,'elect')
plot_ext(uga_data,extrema)
shore <- get_shore_pts(ls,50,uga_data)
plot_shore(uga_data,uga_shp,shore)

uga_data %>%
  dplyr::select(lat,long,mobile) %>%
  #rbind(zp %>% mutate(mobile=0)) %>%
  rbind(extrema) %>%
  rbind(shore %>% mutate(mobile=0)) %>%
  write.csv('uga_zero_ext_water.csv',row.names=FALSE)

# Downsample to fewer shore points for electricity, because I'm finding the
# bounding region using SVM. SVM takes a lot longer to train and finds
# much coarser surfaces.
uga_data %>%
  dplyr::select(lat,long,elect) %>%
  rbind(extrema_elect) %>%
  rbind(shore %>% mutate(elect=0) %>% head(50)) %>%
  write.csv('uga_elect.csv',row.names=FALSE)
