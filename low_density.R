### Better way to do this:
# Randomly sample points from LandScan raster
# keep only those that are within boundaries and have low pop.
# continue until reaching a specified number of unique points
# output dataframe with new points that can be fed into another
# round (to do this iteratively)

library(raster)
library(rgdal)
library(dplyr)

# define ROI
tzn_data <- read.csv('tzn_dhs_2016.csv') %>% filter(lat != 0)
xmin <- min(tzn_data$long)
xmax <- max(tzn_data$long)
xbuf <- 0.05*(xmax-xmin)
ymin <- min(tzn_data$lat)
ymax <- max(tzn_data$lat)
ybuf <- 0.05*(ymax-ymin)
# load LandScan raster and clip
f <- "C:/Users/Craig/Desktop/Live projects/bivar-raster/landscan-pop/w001001x.adf"
ls <- raster(f) %>%
  crop(extent(xmin-xbuf,xmax+xbuf,ymin-ybuf,ymax+ybuf))

tzn_data$pop_dens <- extract(ls,tzn_data[,c('long','lat')] %>% as.matrix)

quantile(tzn_data$pop_dens,na.rm=TRUE,probs=c(0,0.05,0.95,1))
sort(tzn_data$pop_dens) %>% head(50)
# Quite a few points actually have pop density zero. 

### Given a lat/long pair, find the distance to the closest point in a df
closest_dist <- function(lat_,long_,df,omit.zero=TRUE) {
  df_i <- df %>%
    mutate(d2=(lat-lat_)^2 + (long-long_)^2)
  if (omit.zero) {
    df_i <- filter(df_i,d2>0)
  }
  min(df_i$d2) %>% sqrt
}

# TODO: not sure if this is really the optimal way to nest the if/thens
# need to actually benchmark
get_zero_pts <- function(r,shp,npts,pt_df,cutoff=1,dist_cutoff=0.5,seed=12345) {
  set.seed(seed)
  res <- data.frame()
  while (nrow(res) < npts) {
    x <- runif(1,min=r@extent@xmin,max=r@extent@xmax)
    y <- runif(1,min=r@extent@ymin,max=r@extent@ymax)
    c <- closest_dist(y,x,pt_df)
    if (c > dist_cutoff) { # check if too close to sample points
      p <- extract(r,cbind(x,y))
      if (!is.na(p) && p < cutoff) { # check for zero population density
        pt <- data.frame(x=x,y=y)
        coordinates(pt) <- ~x+y
        proj4string(pt) <- proj4string(shp)
        if (!is.na(sp::over(pt,shp)$ADMIN)) { # check if inside polygon
          res <- rbind(res,data.frame(lat=y,long=x))
        }
      }
    }
  }
  res
}

tzn_shp <- readOGR(dsn="./tzn_borders", layer="tzn_borders")

sapply(1:nrow(zp),function(i) 
  closest_dist(zp[i,'lat'],zp[i,'long'],tzn_data))

zp <- get_zero_pts(ls,tzn_shp,200,tzn_data,cutoff=2)
zp$zero <- TRUE
tzn_data %>%
  dplyr::select(lat,long) %>% 
  mutate(zero=FALSE) %>%
  rbind(zp) %>%
  ggplot(aes(x=long,y=lat,color=zero)) +
    geom_point(size=4)

# TODO: Almost there! I just need a way to filter points and make sure 
# they're not too close to sampled DHS points. And possibly relax the cutoff
# a little.



### old stuff below here

library(curl)
library(jsonlite)
library(httr)
library(dplyr)
library(ggplot2)
library(optimx)

###############################################################################
# Get population density based on lat/long
# Impossibly slow. Possible to do this with landscan instead?
###############################################################################
options(stringsAsFactors = FALSE)
get_pop_dens <- function(lat,long) {
  url  <- "http://www.datasciencetoolkit.org"
  path <- paste0('coordinates2statistics/',lat,'%2c',long,
                 '?statistics=population_density')
  raw.result <- GET(url = url, path = path)
  if (raw.result$status_code != 200) {
    return(NULL)
  }
  res <- raw.result$content %>% rawToChar %>% fromJSON
  res$statistics$population_density$value
}

# test
# library(ggmap)
# geocode('1300 Pennsylvania Ave., Washington, DC, USA')
# get_pop_dens(38.89401,-77.03045)

###############################################################################
# Get country based on lat/long
# I'm getting rate-limited by the DSTK API; I need a better way to do this.
# https://gis.stackexchange.com/questions/64513/checking-if-lng-and-lat-fall-inside-polygons-from-esri-shapefile
###############################################################################
get_adm0 <- function(lat,long,method='google') {
  if (method=='google') {
    rgc <- revgeocode(location=c(long,lat),output='more')
    return(as.character(rgc$country))
  } else {
    url  <- "http://www.datasciencetoolkit.org"
    path <- paste0('coordinates2politics/',lat,'%2c',long)
    raw.result <- GET(url = url, path = path)
    if (raw.result$status_code != 200) {
      return(NULL)
    }
    res <- raw.result$content %>% rawToChar %>% fromJSON
    if (is.na(res$politics)) { return(NA) }
    rp <- res$politics[[1]]
    return(rp[rp$friendly_type=='country','name'])
  }
}

###############################################################################
# Add dummy points to map
###############################################################################
tzn_data <- read.csv('tzn_dhs_2016.csv') %>% filter(lat != 0)

ggplot(tzn_data,aes(x=long,y=lat,color=mobile)) +
  geom_point(size=4) +
  scale_color_gradient(name='Electrification',low='#F7FCF5',high='#00441B') 
# Tanzania map has some pretty big holes in it

df <- tzn_data %>%
  select(lat,long) %>%
  mutate(dummy=0)
x_spacing <- (max(df$long)-min(df$long))/sqrt(nrow(df))
y_spacing <- (max(df$lat)-min(df$lat))/sqrt(nrow(df))
buffer2 <- x_spacing^2+y_spacing^2
dr <- buffer2/100
x.min <- min(df$long)-x_spacing
x.max <- max(df$long)+x_spacing
y.min <- min(df$lat)-y_spacing
y.max <- max(df$lat)+y_spacing

dummies <- data.frame()
for (x in seq(x.min,x.max,x_spacing)) {
  for (y in seq(y.min,y.max,y_spacing)) {
    new_dummy <- data.frame(lat=y,long=x,dummy=1)
    dummies <- rbind(dummies,new_dummy)
  }
}

closest_dist2 <- function(i) {
  df_i <- df %>%
    mutate(d=(lat-dummies[i,'lat'])^2 + (long-dummies[i,'long'])^2)
  min(df_i$d)
}

dummies$d2 <- sapply(1:nrow(dummies),closest_dist2)
dummies <- dummies %>%
  filter(d2 > buffer2/2) %>%
  select(lat,long,dummy)

all <- rbind(df,dummies)
ggplot(all,aes(x=long,y=lat,color=dummy)) +
  geom_point(size=4)

# Query only dummies in Tanzania
dummy_adm0 <- sapply(1:nrow(dummies),
                     function(i) {
                       print(i)
                       get_adm0(dummies$lat[i],dummies$long[i])}) 

dummies$query <- sapply(dummy_adm0,
                         function(x) ('Tanzania' %in% x))


all <- df %>%
  mutate(query=FALSE) %>%
  rbind(dummies)

all$adm0 <- NA
all[all$dummy==0,'adm0'] <- 'Tanzania'
all[all$dummy==1,'adm0'] <- sapply(dummy_adm0,function(x) 
  ifelse('Tanzania' %in% x,'Tanzania',x[1]))

ggplot(all,aes(x=long,y=lat,color=adm0)) +
  geom_point(size=4)

tzn <- all %>% filter(adm0=='Tanzania')
tzn$pop_dens <- sapply(1:nrow(tzn),function(i)
  get_pop_dens(dummies$lat[i],dummies$long[i]))

# For set of sampling locations, find the point that is located within borders
#   but has the largest possible radius around it with no points in it. (Rough heuristic OK.)
# --> do this using a potential function, maybe? Inverse of distance to nearest data point or border
# --> resources here: https://cran.r-project.org/web/views/Optimization.html
# Do this iteratively to get a collection of unsampled locations, continuing until 
#   specified avg density is reached
# Get pop. density for each, keep only the low-density ones
# Augment real DHS data with dummy points 



# Related task: make sure kriged surface extends all the way to borders
# given shapefile, get a set of points along boundary (with defined spacing)
# construct KNN model based on known points
# use KNN model to predict values for border points
# Augment real DHS data with these border points

# Can also do something similar so that I get zero access in bodies of water 
# and don't have predictions bleeding across to islands
