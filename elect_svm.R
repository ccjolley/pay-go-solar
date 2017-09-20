library(dplyr)
library(ggplot2)
library(raster)
library(rgeos)
library(rgdal)
library(maptools)
library(e1071)
library(optimx)

rwa_data <- read.csv('rwa_dhs_2015.csv') %>% filter(lat != 0)
tzn_data <- read.csv('tzn_dhs_2016.csv') %>% filter(lat != 0) 
zmb_data <- read.csv('zmb_dhs_2014.csv') %>% filter(lat != 0)
nga_data <- read.csv('nga_dhs_2013.csv') %>% filter(lat != 0)

uga <- read.csv('uga_elect.csv') # includes dummy points for water, etc.
rwa <- read.csv('rwa_elect.csv')
tzn <- read.csv('tzn_elect.csv')
zmb <- read.csv('zmb_elect.csv')
nga <- read.csv('nga_elect.csv')

color_scatter <- function(df) {
  ggplot(df,aes(x=long,y=lat,color=elect)) +
    geom_point(size=4) +
    scale_color_gradient(name='Electrification',low='#F7FCF5',high='#00441B') 
}
color_scatter(zmb) 

###############################################################################
# Re-optimize SVM for each -- turns out things vary quite a bit.
# Uganda map had RMSE = 0.02976139
###############################################################################
svm_tune <- function(df,eps_vals=seq(0,1,0.1),c_vals=2^(2:9)) {
  tuneResult <- tune(svm, elect ~ .,  data = df,
                     ranges = list(epsilon = eps_vals, cost = c_vals)
  )
  print(tuneResult)
  plot(tuneResult)
}

svm_tune(rwa) # eps = 0.5, cost=512
svm_tune(rwa,eps_vals=seq(0.25,0.75,0.05),c_vals=seq(500,1500,200))
# eps = 0.65, cost = 1500

svm_tune(tzn) # eps = 0.4, cost=512, mse=0.07389062
svm_tune(tzn,eps_vals=seq(0.3,0.5,0.01),c_vals=seq(400,1600,200))
# eps = 0.43, cost = 1600

svm_tune(zmb) # eps=0.5, cost=256, MSE=0.8409718
svm_tune(zmb,eps_vals=seq(0.2,0.6,0.02),c_vals=seq(100,500,100))
# eps = 0.5, cost=500, MSE=0.8335141

svm_tune(nga) # eps = 0.6, cost = 64, MSE=0.1401751
svm_tune(nga,eps_vals=seq(0.5,0.65,0.015))
# eps = 0.545, cost = 64

svm_tune(uga)
svm_tune(uga,eps_vals=seq(0.1,0.3,0.02),c_vals=seq(500,1500,200))
# eps = 0.28, cost = 1500

###############################################################################
# make_elect_shp()
#   Assumes that df will have columns named 'lat','long', and 'elect'
#   wrapper should be a function that will return an array of predictions
###############################################################################
make_elect_shp <- function(df,wrapper,out_name=NULL,thresh=0.5,
                           enrich_pts=NULL,res=0.008333333) {
  # Raster setup
  print('Making raster...')
  xpad <- (max(df$long) - min(df$long))*0.05
  ypad <- (max(df$lat) - min(df$lat))*0.05
  r1 <- raster(vals=-1,res=res,
               xmn=min(df$long)-xpad,xmx=max(df$long)+xpad,
               ymn=min(df$lat)-ypad,ymx=max(df$lat)+ypad)
  r1pts <- rasterToPoints(r1) 
  if (!is.null(enrich_pts)) {
    r1pts <- enrich_pts(r1pts)
  }
  print('Calling wrapper...')
  pred <- wrapper(df,r1pts)
  print('Cleaning up...')
  r1 <- setValues(r1,pred)
  if (!is.null(out_name)) {
    r2 <- setValues(r1,pred > thresh)
    pg1 <- rasterToPolygons(r2,function(x) {x ==1},dissolve=TRUE)
    writeOGR(obj=pg1, dsn=out_name, layer=out_name, driver="ESRI Shapefile")
  }
  r1
}

###############################################################################
# Wrapper function for SVM. I tuned these parameters using data from Uganda;
# we'll have to see how well it works on these without additional input.
###############################################################################
svm_wrap <- function(df,pts,eps,c) {
  #print('Calling svm()...')
  my_svm <- svm(elect~.,df,epsilon=eps,cost=c)
  pts_rename <- pts %>% as.data.frame %>%
    dplyr::rename(long=x,lat=y)
  #print('Predicting...')
  predict(my_svm,pts_rename)
}

###############################################################################
# Raster cleanup function
###############################################################################
bound_raster <- function(r) {
  svm_vals <- (r %>% rasterToPoints)[,3]
  svm_filter <- ifelse(svm_vals>1,1,ifelse(svm_vals<0,0,svm_vals))
  setValues(r,svm_filter)
}

###############################################################################
# Production time!
##############################################################################
rwa_svm <- make_elect_shp(rwa,function(df,pts) svm_wrap(df,pts,0.65,1500),
                          'rwa_svm') %>% bound_raster
plot(rwa_svm)
color_scatter(rwa)

tzn_svm <- make_elect_shp(tzn,function(df,pts) svm_wrap(df,pts,0.43,1600),
                          'tzn_svm',res=0.02) %>% bound_raster
plot(tzn_svm)
setValues(tzn_svm,getValues(tzn_svm) > 0.5) %>% plot
color_scatter(tzn)

zmb_svm <- make_elect_shp(zmb,function(df,pts) svm_wrap(df,pts,0.5,500),
                          'zmb_svm') %>% bound_raster
plot(zmb_svm) 
# cutoff of 40% instead of 50% gets me the northern cluster as well
# I think this is worth doing
setValues(zmb_svm,getValues(zmb_svm) > 0.5) %>% plot
color_scatter(zmb)

nga_svm <- make_elect_shp(nga,function(df,pts) svm_wrap(df,pts,0.545,64),
                          'nga_svm',res=0.02) %>% bound_raster
setValues(nga_svm,getValues(nga_svm) > 0.5) %>% plot
color_scatter(nga)

uga_svm <- make_elect_shp(uga,function(df,pts) svm_wrap(df,pts,0.28,1500),
                          'uga_svm',res=0.02) %>% bound_raster
plot(uga_svm)
setValues(uga_svm,getValues(uga_svm) > 0.5) %>% plot
color_scatter(uga)

# How low does Uganda have to go before I see anything other than Kampala?
setValues(uga_svm,getValues(uga_svm) > 0.5) %>% plot
setValues(uga_svm,getValues(uga_svm) > 0.25) %>% plot

###############################################################################
# See which polygons we want
###############################################################################
view_poly <- function(shp_name) {
  my_shp <- readOGR(dsn = shp_name, layer = shp_name)
  my_xy <- my_shp@polygons[[1]]@Polygons
  my_pts <- plyr::ldply(1:length(my_xy), function(i) {
    xy_i <- my_xy[[i]]@coords %>% 
      data.frame %>%
      rename(lat=X2,long=X1) %>% 
      mutate(poly=i)
    xy_i
  }) %>% mutate(poly=as.factor(poly))
  ggplot(my_pts,aes(x=long,y=lat,color=poly)) +
    geom_point() +
    theme_classic()
}

###############################################################################
# Utility function for making polygon shapefiles
# https://stackoverflow.com/questions/21759134/convert-spatialpointsdataframe-to-spatialpolygons-in-r
# NOTE: This is a much more general function than what I actually need for
# this task.
###############################################################################

points2polygons <- function(df,data) {
  get.grpPoly <- function(group,ID,df) {
    Polygon(coordinates(df[df$id==ID & df$group==group,]))
  }
  get.spPoly  <- function(ID,df) {
    Polygons(lapply(unique(df[df$id==ID,]$group),get.grpPoly,ID,df),ID)
  }
  spPolygons  <- SpatialPolygons(lapply(unique(df$id),get.spPoly,df))
  SpatialPolygonsDataFrame(spPolygons,match.ID=T,data=data)
}

###############################################################################
# Select the relevant area and refine 
###############################################################################
better_shp <- function(shp_name,polylist,df,eps,c,target=0.5) {
  my_shp <- readOGR(dsn = shp_name, layer = shp_name)
  my_xy <- my_shp@polygons[[1]]@Polygons
  my_pts <- plyr::ldply(polylist,function(i) {
    xy_i <- my_xy[[i]]@coords %>% 
      data.frame %>%
      rename(lat=X2,long=X1) %>% 
      mutate(poly=i)
  }) 
  my_svm <- svm(elect~.,df,epsilon=eps,cost=c)
  opt_wrap <- function(par) {
    d <- data.frame(long=par[1],lat=par[2])
    (predict(my_svm,d) - target)^2
  }
  my_opt <- apply(my_pts,1,function(x) optimx(par=x,method='BFGS',fn=opt_wrap)) %>% 
    do.call(rbind,.) %>% data.frame
  # turn back into a shapefile
  shp_me <- my_opt %>%
    rename(x=long,y=lat,id=poly) %>%
    dplyr::select(x,y,id) %>%
    mutate(hole=F,piece=1,group=id,box_id=id) %>%
    group_by(id) %>%
    mutate(order=row_number()) %>%
    ungroup()
  coordinates(shp_me)<-~x+y
    
  shp_me_data <- data.frame(box_id=unique(shp_me$box_id),row.names=unique(shp_me$id))
  shp_me2 <- points2polygons(shp_me,shp_me_data)

  fname <- paste0(shp_name,'_opt')
  paste0('Writing shapefile ',fname) %>% print
  writeOGR(shp_me2, dsn=fname, layer=fname, driver='ESRI Shapefile')
  # show it off
  my_opt %>% data.frame %>%
    dplyr::select(long,lat) %>%
    mutate(opt=TRUE) %>%
    rbind(my_pts %>% dplyr::select(long,lat) %>% mutate(opt=FALSE)) %>%
    ggplot(aes(x=long,y=lat,color=opt)) +
    geom_point(size=1) +
    theme_classic()
}

view_poly('uga_svm')
better_shp('uga_svm',1,uga,0.28,1500) 

view_poly('nga_svm')
better_shp('nga_svm',c(2,4),nga,0.545,64) 

view_poly('tzn_svm')
better_shp('tzn_svm',2,tzn,0.43,1600) 

view_poly('rwa_svm')
better_shp('rwa_svm',3,rwa,0.65,1500) 

view_poly('zmb_svm')
better_shp('zmb_svm',2,zmb,0.5,500) 








