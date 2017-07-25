library(curl)
library(jsonlite)
library(httr)
library(dplyr)
library(ggplot2)
library(optimx)

###############################################################################
# Get population density based on lat/long
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
###############################################################################
get_adm0 <- function(lat,long) {
  url  <- "http://www.datasciencetoolkit.org"
  path <- paste0('coordinates2politics/',lat,'%2c',long)
  raw.result <- GET(url = url, path = path)
  if (raw.result$status_code != 200) {
    return(NULL)
  }
  res <- raw.result$content %>% rawToChar %>% fromJSON
  if (is.na(res$politics)) { return(NA) }
  rp <- res$politics[[1]]
  #paste0(lat,',',long,': ',rp[rp$friendly_type=='country','name']) %>% print
  rp[rp$friendly_type=='country','name']
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
seed(12345)
# add random noise to dummy points for better optimization start
for (x in seq(x.min,x.max,x_spacing)) {
  for (y in seq(y.min,y.max,y_spacing)) {
    rx <- runif(1,-dr,dr)
    ry <- runif(1,-dr,dr)
    new_dummy <- data.frame(lat=y+ry,long=x+rx,dummy=1)
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

###############################################################################
# Optimize spacing between points
#
# I want to try doing this using optimx(), which requires an objective
# function takes the input parameters as a vector and returns a scalar value.
# Can use the gradient and Hessian as well if I'm feeling ambitious
###############################################################################
# Freeze all points that are not dummies or not in Tanzania
dummy_adm0 <- sapply(1:nrow(dummies),
                     function(i) get_adm0(dummies$lat[i],dummies$long[i])) 

dummies$freeze <- sapply(dummy_adm0,
                         function(x) !('United Republic of Tanzania' %in% x))
all <- df %>%
  mutate(freeze=TRUE) %>%
  rbind(dummies)
ggplot(all,aes(x=long,y=lat,color=freeze)) +
  geom_point(size=4)

melted <- c(all[!all$freeze,'lat'],all[!all$freeze,'long'])
frozen <- c(all[all$freeze,'lat'],all[all$freeze,'long'])

### Objective function: sum up the inverse of the distance to the nearest 
### point (melted or frozen) for each melted point
## For both sets of variables, assume that the first half of the vector is 
## latitude values and the second half is longitude values

# TODO: does it speed things up when I move this inside obj_fun()?
closest_dist <- function(df,rows) {
  sapply(rows,function(i) {
    lati <- df[i,'lat']
    longi <- df[i,'long']
    df_i <- df[-i,] %>%
      mutate(d2 = (lat-lati)^2 + (long-longi)^2) 
    d2_min <- min(df_i$d2) %>% sqrt
    d2_min
  })
}

closest_wid <- function(df,rows) {
  plyr::ldply(rows,function(i) {
    lati <- df[i,'lat']
    longi <- df[i,'long']
    df_i <- df %>%
      mutate(d2 = (lat-lati)^2 + (long-longi)^2) 
    d2_min <- min(df_i$d2[df_i$d2 > 0]) 
    closest <- which(df_i$d2 == d2_min) %>% head(1)
    # The head(1) there is sort of arbitrary, but hopefully will only cause
    # issues at the highly symmetric starting grid.
    data.frame(r=sqrt(d2_min),id=closest)
  })
}

obj_fun <- function(x,frozen=NULL) {
  pts <- data.frame(lat=x[1:(length(x)/2)],
                    long=x[(length(x)/2+1):length(x)],
                    freeze=FALSE)
  if (!is.null(frozen)) {
    frzpts <- data.frame(lat=frozen[1:(length(frozen)/2)],
                         long=frozen[(length(frozen)/2+1):length(frozen)],
                         freeze=TRUE)
    pts <- rbind(pts,frzpts)
  }
  val <- pts %>%
    mutate(rn=row_number()) %>%
    filter(!freeze) %>%
    mutate(score=1/closest_dist(pts,rn)) %>%
    select(score) %>%
    sum
  #print(val)
  val
}

obj_fun(melted,frozen)

x2df <- function(x,frozen=NULL) {
  pts <- data.frame(lat=x[1:(length(x)/2)],
                    long=x[(length(x)/2+1):length(x)],
                    freeze=FALSE)
  if (!is.null(frozen)) {
    frzpts <- data.frame(lat=frozen[1:(length(frozen)/2)],
                         long=frozen[(length(frozen)/2+1):length(frozen)],
                         freeze=TRUE)
    pts <- rbind(pts,frzpts)
  }
  pts %>% cbind(closest_wid(pts,1:nrow(pts)))
}

### Gradient function
# For each melted point i, dV/dxi will have a contribution of
# -(xi-xj)/(rij)^3 for the point j that is closest to i (melted or frozen)
obj_fun.g <- function(x,frozen=NULL) {
  neigh <- x2df(x,frozen)
  lat_grad <- sapply(which(!neigh$freeze), function(i) {
    j <- neigh[i,'id']
    j_grad <- -(neigh[i,'lat']-neigh[j,'lat'])/neigh[i,'r']^3
    k_list <- which(neigh$id == i)
    if (length(k_list) > 0) {
      k_grad <- sapply(k_list,function(k) {
        -(neigh[i,'lat']-neigh[k,'lat'])/neigh[k,'r']^3
      }) %>% sum
    } else {k_grad <- 0}
    j_grad + k_grad
  }) 
  long_grad <- sapply(which(!neigh$freeze), function(i) {
    j <- neigh[i,'id']
    j_grad <- -(neigh[i,'long']-neigh[j,'long'])/neigh[i,'r']^3
    k_list <- which(neigh$id == i)
    if (length(k_list) > 0) {
      k_grad <- sapply(k_list,function(k) {
        -(neigh[i,'long']-neigh[k,'long'])/neigh[k,'r']^3
      }) %>% sum
    } else {k_grad <- 0}
    j_grad + k_grad
  }) 
  c(lat_grad,long_grad)
}

grad <- obj_fun.g(melted,frozen) 

# TODO: Calculate Hessian. I suspect most off-diagonal elements will be zero

#### Hessian function
obj_fun.h <- function(x,frozen=NULL) {
  neigh <- x2df(x,frozen)
  n <- length(x)
  m <- matrix(0,nrow=n,ncol=n)
  for (i in 1:(n/2)) {
    xi <- neigh[i,'long']
    yi <- neigh[i,'lat']
    j <- neigh[i,'id'] # j is i's nearest neighbor
    xj <- neigh[j,'long']
    yj <- neigh[j,'lat']
    rij <- neigh[i,'r']
    k_list <- which(neigh$id == i)
    ## latitude first
    # diagonal elements
    # paste0('i = ',i) %>% print
    # paste0('j = ',j) %>% print
    # paste0('rij = ',rij) %>% print
    # paste0('yi = ',yi) %>% print
    # paste0('yi = ',yj) %>% print
    mii <- -1/rij^3 + 3*(yi-yj)^2/rij^5
    if (length(k_list) > 0) {
      # neighbor contributions to diagonal
      # TODO: do this in one loop instead of two
      k_diag <- sapply(k_list,function(k) {
        yk <- neigh[k,'lat']
        rik <- neigh[k,'r']
        -1/neigh[k,'r']^3 + 3*(yi-yk)^2/rik^5
      }) %>% sum
      mii <- mii + k_diag
      # off-diagonal (latitude only)
      for (k in k_list) {
        yk <- neigh[k,'lat']
        rik <- neigh[k,'r']
        mik <- 1/rik^3 + 3*(yi-yk)^2/rik^5
        if (neigh[i,'id']==k) { mik <- mik*2 }
        m[i,k] <- mik
      }
    }
    m[i,i] <- mii
    ## now longitude
    mii <- -1/rij^3 + 3*(xi-xj)^2/rij^5
    if (length(k_list) > 0) {
      # neighbor contributions to diagonal
      k_diag <- sapply(k_list,function(k) {
        xk <- neigh[k,'long']
        rik <- neigh[k,'r']
        -1/rik^3 + 3*(xi-xk)^2/rik^5
      }) %>% sum
      mii <- mii + k_diag
      # off-diagonal (longitude only)
      for (k in k_list) {
        xk <- neigh[k,'long']
        rik <- neigh[k,'r']
        mik <- 1/rik^3 + 3*(xi-xk)^2/rik^5
        if (neigh[i,'id']==k) mik <- mik*2
        m[n/2+i,n/2+k] <- mik
      }
    }
    m[n/2+i,n/2+i] <- mii
    ## latitude-longitude cross terms
    mii <- 3*(xi-xj)*(yi-yj)/rij^5
    if (length(k_list) > 0) {
      k_diag <- sapply(k_list,function(k) {
        xk <- neigh[k,'long']
        yk <- neigh[k,'lat']
        rik <- neigh[k,'r']
        3*(xi-xk)*(yi-yk)/rik^5
      }) %>% sum
      mii <- mii + k_diag
      # off-diagonal cross terms
      for (k in k_list) {
        xk <- neigh[k,'long']
        yk <- neigh[k,'lat']
        rik <- neigh[k,'r']
        mik <- 3*(xi-xk)*(yi-yk)/rik^5
        if (neigh[i,'id']==k) mik <- mik*2
        m[i,n/2+k] <- mik
        m[n/2+i,k] <- mik
      }
    }
    m[n/2+i,i] <- mii
    m[i,n/2+i] <- mii
  }
  m
}

### Numeric test of Hessian
n = 20
melted <- runif(n)
hess <- obj_fun.h(melted)
m <- matrix(nrow=n,ncol=n)
v <- obj_fun(melted)
# get Hessian using numDeriv::hessian()
# https://cran.r-project.org/web/packages/numDeriv/numDeriv.pdf

opt <- optimx(melted,obj_fun,frozen=frozen)
# This takes for-e-ver, because the default algorithm is Nelder-Mead simplex
# if I want to use the more advanced L-BFGS-B, then I need to supply gradient
# and Hessian.

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
