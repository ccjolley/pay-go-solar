# My goal here is to come up with something that will draw a (fairly simple?) 
# boundary around areas of high electrification, so that this can be added to my
# population/mobile maps without having to introduce a new color scale.

# Test out on Uganda

library(dplyr)
library(ggplot2)
library(raster)
library(rgeos)
library(rgdal)
library(maptools)
library(llamar)
library(RColorBrewer)

uga_data <- read.csv('uga_dhs_2011.csv') %>%
  filter(lat != 0)

uga <- uga_data %>% dplyr::select(lat,long,elect)

shp2df <- function(dsn,layer) {
  shp <- readOGR(dsn=dsn,layer=layer)
  shp@data$id <- rownames(shp@data)
  shp.points = fortify(shp, region="id")
  plyr::join(shp.points, shp@data, by="id")
}

uga_border <- shp2df(dsn="./uga_borders", layer="uga_borders")
uga_lakes <- shp2df(dsn="./uga_lakes", layer="uga_lakes") %>%
  mutate(island=factor(ifelse(piece==1,0,1)),
         gp=paste0(name,'_',piece))

colr <- colorRampPalette(brewer.pal(9, 'Greens'))

ggplot(uga_border) + 
  aes(long,lat) + 
  geom_polygon(fill='gray85') +
  geom_path(color="black") +
  coord_equal() +
  geom_polygon(data=uga_lakes,aes(group=gp,fill=island)) +
  scale_fill_manual(values=c('cornflowerblue','gray85'),guide=FALSE) +
  geom_point(data=uga_data,size=4,aes(color=elect)) +
  scale_color_gradient(name='Electrification',low=colr(2)[1],high=colr(2)[2]) +
  theme(title = element_blank(), axis.title = element_blank(), 
        axis.text = element_blank(), axis.ticks = element_blank(), 
        axis.ticks.length = unit(0, units = "points"), panel.border = element_blank(), 
        panel.grid = element_blank(), panel.background = element_blank(), 
        plot.background = element_blank()) 

###############################################################################
# make_elect_shp()
#   Assumes that df will have columns named 'lat','long', and 'elect'
#   wrapper should be a function that will return an array of predictions
###############################################################################
make_elect_shp <- function(df,wrapper,out_name=NULL,thresh=0.5,
                           enrich_pts=NULL) {
  # Raster setup
  xpad <- (max(df$long) - min(df$long))*0.05
  ypad <- (max(df$lat) - min(df$lat))*0.05
  res <- 0.008333333 # same as LandScan
  print('Making raster r1')
  r1 <- raster(vals=-1,res=res,
               xmn=min(df$long)-xpad,xmx=max(df$long)+xpad,
               ymn=min(df$lat)-ypad,ymx=max(df$lat)+ypad)
  r1pts <- rasterToPoints(r1) 
  if (!is.null(enrich_pts)) {
    print('Enriching raster points')
    r1pts <- enrich_pts(r1pts)
  }
  # Call wrapper function
  print('Caling wrapper')
  pred <- wrapper(df,r1pts)
  print('Finishing up')
  r1 <- setValues(r1,pred)
  if (!is.null(out_name)) {
    r2 <- setValues(r1,pred > 0.5)
    pg1 <- rasterToPolygons(r2,function(x) {x ==1},dissolve=TRUE)
    writeOGR(obj=pg1, dsn=out_name, layer=out_name, driver="ESRI Shapefile")
  }
  r1
}

###############################################################################
# Try KNN first -- keep it simple
###############################################################################
library(FNN)

# Tune k
k_rmse <- sapply(2:15,function(i) {
  knn_i <- knn.reg(uga[,c('lat','long')],y=uga$elect,
                   k=i,algorithm='kd_tree')
  rmse <- (uga$elect - knn_i$pred)^2 %>% mean %>% sqrt
  paste0(i,' neighbors, RMSLE = ',rmse) %>% print
  rmse
})
qplot(2:15,k_rmse)
# Not a ton of variation; best performance with k=7

kr <- knn.reg(uga[,c('lat','long')],y=uga$elect,
              k=7,algorithm='kd_tree')
uga$pred <- kr$pred
qplot(uga$elect,uga$pred)
(uga$elect - uga$pred)^2 %>% mean %>% sqrt 
# RMSE = 0.1768631 

knn_wrap <- function(df,pts) {
  vars <- setdiff(names(df),'elect')
  pts <- pts %>% as.data.frame %>% dplyr::rename(lat=y,long=x)
  my_knn <- knn.reg(train=df[,vars],test=pts[,vars],
                    y=df$elect,k=7,algorithm='kd_tree')
  my_knn$pred
}

knn_raster <- make_elect_shp(uga,knn_wrap,'uga_knn')
plot(knn_raster)

# try with ggplot

library(rasterVis)

uga_border_shp <- readOGR(dsn="./uga_borders", layer="uga_borders")
uga_lakes_shp <- readOGR(dsn="./uga_lakes", layer="uga_lakes")

uga_raster_plot <- function(r) {
  rasterVis::levelplot(r,
                       margin=FALSE,
                       colorkey=list(
                         space='bottom',                   # plot legend at bottom
                         labels=list(at=(0:10)/10, font=4)      # legend ticks and labels 
                       ),
                       par.settings=list(
                         axis.line=list(col='transparent') # suppress axes and legend outline
                       ),
                       scales=list(draw=FALSE),
                       xlab='',
                       ylab='',
                       col.regions=colr,                   # colour ramp
                       at=seq(0, 1, len=101)) +            # colour ramp breaks
    latticeExtra::layer(sp.polygons(uga_border_shp, lwd=1)) +
    latticeExtra::layer(sp.polygons(uga_lakes_shp, lwd=1))
}

uga_raster_plot(knn_raster)


###############################################################################
# Just for lulz, try lm. Terrible prediction
###############################################################################
lm_wrap <- function(df,pts) {
  my_lm <- lm(elect~.,data=df)
  pts_rename <- pts %>% as.data.frame %>%
    dplyr::rename(long=x,lat=y)
  predict(my_lm,pts_rename)
}

#lm_raster <- make_elect_shp(uga,lm_wrap,'uga_lm')
lm_raster <- make_elect_shp(uga,lm_wrap)
uga_raster_plot(lm_raster)

###############################################################################
# Curious about SVM now. I'm not going to worry about cross-validation at
# this stage.
###############################################################################
library(e1071)

test_svm <- svm(elect ~ .,uga)
(uga$elect - predict(test_svm,uga))^2 %>% mean %>% sqrt 
# 0.2078456; not as good as KNN

tuneResult <- tune(svm, elect ~ .,  data = uga,
                   ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9))
)
print(tuneResult) # epsilon=0.3, cost=512, RSME = 0.18995 
plot(tuneResult)

tuneResult2 <- tune(svm, elect ~ .,  data = uga,
                   ranges = list(epsilon = seq(0,0.4,0.05), cost = 100*(4:10))
)
print(tuneResult2) # epsilon=0.25, cost=900, RSME=0.18647
plot(tuneResult2)

tuneResult3 <- tune(svm, elect ~ .,  data = uga,
                    ranges = list(epsilon = seq(0.25,0.35,0.01), cost = 100*(8:12))
)
print(tuneResult3) # epsilon=0.32, cost=1200, RSME=0.18552
# can't seem to beat KNN
plot(tuneResult3)

cost_test <- function(c) {
  test_svm <- svm(elect ~ .,uga,epsilon=0.32,cost=c)
  (uga$elect - predict(test_svm,uga))^2 %>% mean %>% sqrt 
}
cost_test(1500) # 0.1725149
cost_test(3000) # 0.1707916
cost_test(10000) # 0.1675187
cost_test(50000) # 0.1642089 
# I'm a litttle concerned that very high costs might lead to overfitting


# Now let's see what the raster looks like
svm_wrap <- function(df,pts) {
  my_svm <- svm(elect~.,df,epsilon=0.32,cost=1500)
  pts_rename <- pts %>% as.data.frame %>%
    dplyr::rename(long=x,lat=y)
  predict(my_svm,pts_rename)
}

# svm_raster <- make_elect_shp(uga,svm_wrap,'uga_svm')
svm_raster <- make_elect_shp(uga,svm_wrap)
plot(svm_raster)
# It looks like SVM gives smoother countours than KNN, but is more inclined
# to crazy extrapolations in areas with no data.

# In the ROI, results for C=50000 look about the same as C=1200. Boundary
# effects are even crazier, though.

# Filter out values that are <0 or >1
svm_vals <- (svm_raster %>% rasterToPoints)[,3]
svm_filter <- ifelse(svm_vals>1,1,ifelse(svm_vals<0,0,svm_vals))
svm_filter_raster <- setValues(svm_raster,svm_filter)
uga_raster_plot(svm_filter_raster)

###############################################################################
# What about random forests?
###############################################################################
library(randomForest)

test_rf <- randomForest(elect ~ .,data=uga)
(uga$elect - test_rf$predicted)^2 %>% mean %>% sqrt # 0.1600594, beats KNN
# I could mess with parameters some, but stick with defaults for now.

rf_wrap <- function(df,pts) {
  x_train <- df %>% dplyr::select(-elect)
  x_pred <- pts %>% as.data.frame %>%
    dplyr::rename(long=x,lat=y) %>%
    dplyr::select(-layer)
  my_rf <- randomForest(x=x_train,y=df$elect,
                        xtest=x_pred)
  my_rf$test$predicted
}

#rf_raster <- make_elect_shp(uga,rf_wrap,'uga_rf')
rf_raster <- make_elect_shp(uga,rf_wrap)
uga_raster_plot(rf_raster)

###############################################################################
# Feature engineering: enrich the original dataset with distances from Uganda's
# major urban centers (which can be derived from lat/long).
###############################################################################
library(ggmap)
library(geosphere)

# Get coordinates of landmark cities

cities <- c('Kitgum','Arua','Gulu','Lira','Obalang','Soroti','Mbale','Jinja',
            'Kampala','Entebbe','Fort Portal','Kasese','Mitoma','Mbarara',
            'Rushenyi')
geo_cities <- data.frame(city=cities) %>%
  mutate(str=paste0(city,', Uganda'))
geo_cities <- cbind(geo_cities,geocode(geo_cities$str,output='latlon'))

# Add distances to data frame

distFlat <- function(x,y) {
  sqrt((x[1]-y[1])^2 + (x[2]-y[2])^2)
}

add_dists <- function(df,fun=distFlat) {
  res <- as.data.frame(df)
  if ('lat' %in% names(df)) {
    xy <- c('long','lat')
  } else {
    xy <- c('x','y')
  }
  for (i in 1:nrow(geo_cities)) {
    x <- df[,xy] %>% as.matrix
    y <- geo_cities[i,c('lon','lat')]
    c <- geo_cities$city[i] %>% as.character
    paste0('Adding ',c) %>% print
    #dim(x) %>% print
    #dim(res) %>% print
    #d <- distm(x,y,fun=fun) %>% as.numeric
    #colnames(d) <- c
    #res <- cbind(res,d)
    res[,c] <- distm(x,y,fun=fun)
  }
  res
}
uga_geo <- add_dists(uga)
uga_geo_cos <- add_dists(uga,fun=distCosine)
qplot(uga_geo$Kitgum,uga_geo_cos$Kitgum) # good enough for me

### Linear regression with new features

lm_raster2 <- make_elect_shp(uga_geo,lm_wrap,enrich_pts=add_dists)
# This is taking a really long time. Distance measurements are really slow
uga_raster_plot(lm_raster2)

### KNN with new features
knn_raster2 <- make_elect_shp(uga_geo,knn_wrap,enrich_pts=add_dists)
uga_raster_plot(knn_raster2)

### RF with new features
rf_raster2 <- make_elect_shp(uga_geo,rf_wrap,enrich_pts=add_dists)
uga_raster_plot(rf_raster2)

### SVM with new features
svm_raster2 <- make_elect_shp(uga_geo,svm_wrap,enrich_pts=add_dists)
svm_vals2 <- (svm_raster2 %>% rasterToPoints)[,3]
svm_filter2 <- ifelse(svm_vals2>1,1,ifelse(svm_vals2<0,0,svm_vals2))
svm_filter_raster2 <- setValues(svm_raster2,svm_filter2)
uga_raster_plot(svm_filter_raster2)

###############################################################################
# Lousy feature engineering
###############################################################################
islands <- c('Bugala Island','Kome Island','Sigulu Island','Lolui Island')
geo_cities <- data.frame(city=islands) %>%
  mutate(str=paste0(city,', Uganda'))
geo_cities <- cbind(geo_cities,geocode(geo_cities$str,output='latlon'))
uga_geo <- add_dists(uga)

svm_raster3 <- make_elect_shp(uga_geo,svm_wrap,enrich_pts=add_dists)
svm_vals3 <- (svm_raster3 %>% rasterToPoints)[,3]
svm_filter3 <- ifelse(svm_vals3>1,1,ifelse(svm_vals3<0,0,svm_vals3))
svm_filter_raster3 <- setValues(svm_raster3,svm_filter3)
uga_raster_plot(svm_filter_raster3)
