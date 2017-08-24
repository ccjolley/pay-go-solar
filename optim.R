# I think this was probably overkill... stashing it here in case I need it later.

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
  print(val)
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
  print('Calling gradient...')
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
  print('Calling Hessian...')
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
        mik <- 1/rik^3 - 3*(yi-yk)^2/rik^5
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
        mik <- 1/rik^3 - 3*(xi-xk)^2/rik^5
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
        mik <- 3*(xi-xk)*(yk-yi)/rik^5
        if (neigh[i,'id']==k) mik <- mik*2
        m[i,n/2+k] <- m[n/2+i,k] <-mik
      }
    }
    m[n/2+i,i] <- mii
    m[i,n/2+i] <- mii
  }
  m
}


### Test optimization
startx <- runif(8)
df <- x2df(startx)
qplot(df$long,df$lat)
opt <- optimx(startx,
              fn=obj_fun,gr=obj_fun.g,hess=obj_fun.h,
              lower=0,upper=1,method='L-BFGS-B')

low <- c(rep(min(all$lat),length(melted)/2),rep(min(all$long),length(melted)/2))
hi <- c(rep(max(all$lat),length(melted)/2),rep(max(all$long),length(melted)/2))

system.time(opt <- optimx(melted,fn=obj_fun,gr=obj_fun.g,hess=obj_fun.h,
                          lower=low,upper=hi,method='L-BFGS-B',frozen=frozen))

system.time(opt <- optimx(melted,fn=obj_fun,gr=obj_fun.g,hess=obj_fun.h,
                          lower=low,upper=hi,method='CG',frozen=frozen))