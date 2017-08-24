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
rwa <- rwa_data %>% dplyr::select(lat,long,elect)
tzn <- tzn_data %>% dplyr::select(lat,long,elect)
#zmb <- zmb_data %>% dplyr::select(lat,long,elect)
zmb <- read.csv('zmb_elect.csv')
nga <- nga_data %>% dplyr::select(lat,long,elect)

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

svm_tune(rwa) # eps = 0.5, cost=512, mse=0.06766672
svm_tune(rwa,eps_vals=seq(0.4,0.6,0.01),c_vals=seq(500,1500,200))
# eps = 0.52, cost=900, mse=0.06668185

svm_tune(tzn) # eps = 0.5, cost=512, mse=0.07389062

svm_tune(zmb) # eps=0.5, cost=256, MSE=0.8409718
svm_tune(zmb,eps_vals=seq(0.2,0.6,0.02),c_vals=seq(100,500,100))
# eps = 0.5, cost=500, MSE=0.8335141

svm_tune(nga) # eps = 0.6, cost = 64, MSE=0.1401751
svm_tune(nga,eps_vals=seq(0.5,0.65,0.015))
# eps = 0.575, cost = 128

svm_tune(uga)
svm_tune(uga,eps_vals=seq(0.1,0.3,0.02),c_vals=seq(500,1500,200))
# eps = 0.3, cost = 1500

###############################################################################
# make_elect_shp()
#   Assumes that df will have columns named 'lat','long', and 'elect'
#   wrapper should be a function that will return an array of predictions
###############################################################################
make_elect_shp <- function(df,wrapper,out_name=NULL,thresh=0.4,
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
rwa_svm <- make_elect_shp(rwa,function(df,pts) svm_wrap(df,pts,0.52,900),
                          'rwa_svm') %>% bound_raster
plot(rwa_svm)
color_scatter(rwa)

tzn_svm <- make_elect_shp(tzn,function(df,pts) svm_wrap(df,pts,0.5,512),
                          'tzn_svm',res=0.02) %>% bound_raster
plot(tzn_svm)
color_scatter(tzn)

zmb_svm <- make_elect_shp(zmb,function(df,pts) svm_wrap(df,pts,0.5,500),
                          'zmb_svm') %>% bound_raster
plot(zmb_svm) 
# cutoff of 40% instead of 50% gets me the northern cluster as well
# I think this is worth doing
setValues(zmb_svm,getValues(zmb_svm) > 0.4) %>% plot
color_scatter(zmb)

nga_svm <- make_elect_shp(nga,function(df,pts) svm_wrap(df,pts,0.575,128),
                          'nga_svm',res=0.02) %>% bound_raster
plot(nga_svm)
color_scatter(nga)

uga_svm <- make_elect_shp(uga,function(df,pts) svm_wrap(df,pts,0.575,128),
                          'uga_tmp',res=0.02) %>% bound_raster
plot(uga_svm)
color_scatter(uga)

# How low does Uganda have to go before I see anything other than Kampala?
setValues(uga_svm,getValues(uga_svm) > 0.5) %>% plot
setValues(uga_svm,getValues(uga_svm) > 0.25) %>% plot

###############################################################################
# Select the relevant area and refine 
###############################################################################
better_shp <- function(shp_name,df,eps,c,target=0.4) {
  my_shp <- readOGR(dsn = shp_name, layer = shp_name)
  my_xy <- my_shp@polygons[[1]]@Polygons
  my_pts <- sapply(1:length(my_xy),function(i) {
    # check to see whether polygon is within ROI
    xy_i <- my_xy[[i]]@coords %>% 
      data.frame %>%
      rename(lat=X2,long=X1) %>% 
      mutate(poly=i)
    if (min(xy_i$lat) >= min(df$lat) &
        max(xy_i$lat) <= max(df$lat) &
        min(xy_i$long) >= min(df$long) &
        max(xy_i$long) <= max(df$long)) {
      xy_i
    } else {
      return(NULL)
    }
  }) %>% do.call(rbind,.) 
  my_svm <- svm(elect~.,df,epsilon=eps,cost=c)
  opt_wrap <- function(par) {
    d <- data.frame(long=par[1],lat=par[2])
    (predict(my_svm,d) - target)^2
  }
  my_opt <- apply(my_pts,1,function(x) optimx(par=x,method='BFGS',fn=opt_wrap)) %>% 
    do.call(rbind,.) %>% data.frame
  # turn back into a shapefile
  coordinates(my_opt)<-~long+lat
  fname <- paste0(shp_name,'_opt')
  paste0('Writing shapefile ',fname) %>% print
  writeOGR(my_opt, dsn=fname, layer=fname, driver='ESRI Shapefile')
  # show it off
  my_opt %>% data.frame %>%
    dplyr::select(long,lat) %>%
    mutate(opt=TRUE) %>%
    rbind(my_pts %>% dplyr::select(long,lat) %>% mutate(opt=FALSE)) %>%
    ggplot(aes(x=long,y=lat,color=opt)) +
    geom_point(size=1) +
    theme_classic()
}

better_shp('rwa_svm',rwa,0.52,900) # re-do; this was wrong before
better_shp('zmb_svm',zmb,0.5,500) # try again when I have lots of time.











