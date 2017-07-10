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
  filter(name=='Lake Victoria')

ggplot(uga_border) + 
  aes(long,lat) + 
  geom_path(color="black") +
  coord_equal() +
  #geom_polygon(data=uga_lakes,fill='cornflowerblue') +
  geom_point(data=uga_data,size=4,aes(color=elect)) +
  theme_classic()

# TODO: lake display doesn't look great
# clues here? https://github.com/tidyverse/ggplot2/wiki/plotting-polygon-shapefiles

###############################################################################
# make_elect_shp()
#   Assumes that df will have columns named 'lat','long', and 'elect'
#   wrapper should be a function that will return an array of predictions
###############################################################################
make_elect_shp <- function(df,wrapper,out_name,thresh=0.5) {
  # Raster setup
  xpad <- (max(df$long) - min(df$long))*0.05
  ypad <- (max(df$lat) - min(df$lat))*0.05
  res <- 0.008333333 # same as LandScan
  r1 <- raster(vals=-1,res=res,
               xmn=min(df$long)-xpad,xmx=max(df$long)+xpad,
               ymn=min(df$lat)-ypad,ymx=max(df$lat)+ypad)
  r1pts <- rasterToPoints(r1) 
  # Call wrapper function
  pred <- wrapper(df,r1pts)
  r1 <- setValues(r1,pred)
  r2 <- setValues(r1,pred > 0.5)
  pg1 <- rasterToPolygons(r2,function(x) {x ==1},dissolve=TRUE)
  writeOGR(obj=pg1, dsn=out_name, layer=out_name, driver="ESRI Shapefile")
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
  my_knn <- knn.reg(train=df[,c('lat','long')],test=pts[,c('y','x')],
                    y=df$elect,k=7,algorithm='kd_tree')
  my_knn$pred
}

knn_raster <- make_elect_shp(uga,knn_wrap,'uga_knn')
plot(knn_raster)

# try with ggplot

library(rasterVis)
library(RColorBrewer)

uga_border_shp <- readOGR(dsn="./uga_borders", layer="uga_borders")
uga_lakes_shp <- readOGR(dsn="./uga_lakes", layer="uga_lakes")
colr <- colorRampPalette(brewer.pal(11, 'Greens'))

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

lm_raster <- make_elect_shp(uga,lm_wrap,'uga_lm')
plot(lm_raster)

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

svm_raster <- make_elect_shp(uga,svm_wrap,'uga_svm')
plot(svm_raster)
# It looks like SVM gives smoother countours than KNN, but is more inclined
# to crazy extrapolations in areas with no data.

# In the ROI, results for C=50000 look about the same as C=1200. Boundary
# effects are even crazier, though.

# Filter out values that are <0 or >1
svm_vals <- (svm_raster %>% rasterToPoints)[,3]
svm_filter <- ifelse(svm_vals>1,1,ifelse(svm_vals<0,0,svm_vals))
svm_filter_raster <- setValues(svm_raster,svm_filter)
plot(svm_filter_raster)

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
    dplyr::select(lat,long)
  my_rf <- randomForest(x=x_train,y=df$elect,
                        xtest=x_pred)
  my_rf$test$predicted
}

rf_raster <- make_elect_shp(uga,rf_wrap,'uga_rf')
plot(rf_raster)

###############################################################################
# The big guns: XGBoost
###############################################################################
library(xgboost)
x <- uga[,c('lat','long')] %>% as.matrix
train_xgb <- xgboost(data=x,
                     label=uga$elect,
                     objective='reg:linear',
                     nrounds=500,
                     verbose=0)
(uga$elect - predict(train_xgb,x))^2 %>% mean %>% sqrt # RSME=0.00238 overfit.
# Actually, it overfits so much that none of the on-grid points being evaluated
# for the raster are >0.5 -- it's useless for interpolation because it just
# memorized the data.

# Cross-validation to the rescue
xgb_rmse <- function(df,in_test,nrounds,eta,max_depth,subsample,cbt) {
  df_test <- df[in_test,]
  df_train <- df[!in_test,]
  x <- df_train[,c('lat','long')] %>% as.matrix
  x_test <- df_test[,c('lat','long')] %>% as.matrix
  df_xgb <- xgboost(data=x,
                    label=df_train$elect,
                    objective='reg:linear',
                    nrounds=nrounds,
                    verbose=0,
                    eta=eta,max_depth=max_depth,subsample=subsample,
                    colsample_bytree=cbt)
  (df_test$elect - predict(df_xgb,x_test))^2 %>% mean %>% sqrt
}

xgb_xval <- function(df,verbose=FALSE,fold=10,nrounds=385, eta=0.05, max_depth=5, 
                     subsample=0.7,cbt=0.7) {
  xval_ind <- rep(1:fold,times=nrow(df)/fold+1)[1:nrow(df)] %>% sample
  res <- rep(NA,fold)
  for (i in 1:fold) {
    rmsle <- xgb_rmse(df,xval_ind==i,nrounds,eta,max_depth,subsample,cbt)
    if (verbose) {
      paste0('x-val ',i,': RMSLE = ',rmsle) %>% print
    }
    res[i] <- rmsle
  }
  mean(res)
}

xgb_xval(uga,FALSE,10,385,0.05,5,0.7,0.7) # 0.1944931
xgb_xval(uga,FALSE,10,200,0.05,5,0.7,0.7) # 0.1884459
xgb_xval(uga,FALSE,10,100,0.05,5,0.7,0.7) # 0.1842703
# Can't seem to improve on this much with small tweaks



# Now let's make a raster
xgb_wrap <- function(df,pts) {
  x <- df[,c('lat','long')] %>% as.matrix
  my_xgb <- xgboost(data=x,
                    label=df$elect,
                    objective='reg:linear',
                    nrounds=100,
                    eta=0.05,max_depth=5,
                    subsample=0.7,colsample_bytree=0.7,
                    verbose=0)
  pts_rename <- pts %>% as.data.frame %>%
    dplyr::rename(long=x,lat=y) %>%
    dplyr::select(lat,long) %>%
    as.matrix
  res <- predict(my_xgb,pts_rename)
  paste0('max = ',max(res)) %>% print
  paste0('min = ',min(res)) %>% print
  res
}

xgb_raster <- make_elect_shp(uga,xgb_wrap,'uga_xgb')
plot(xgb_raster)
# This looks horrible. This algorithm is definitely overkill.
