# Functions needed for 'Life expectancy and equality: a long run relationship'

# Some useful fucntions: for ax and life table
AKm02a0        <- function(m0, sex = "m"){
  sex <- rep(sex, length(m0))
  ifelse(sex == "m", 
         ifelse(m0 < .0230, {0.14929 - 1.99545 * m0},
                ifelse(m0 < 0.08307, {0.02832 + 3.26201 * m0},.29915)),
         # f
         ifelse(m0 < 0.01724, {0.14903 - 2.05527 * m0},
                ifelse(m0 < 0.06891, {0.04667 + 3.88089 * m0}, 0.31411))
  )
}

LifeExpectancy <- compiler::cmpfun(function(mx,sex = "f"){
  i.openage <- length(mx)
  OPENAGE   <- i.openage - 1
  RADIX     <- 1
  ax        <- mx * 0 + .5
  ax[1]     <- AKm02a0(m0 = mx[1], sex = sex)
  qx        <- mx / (1 + (1 - ax) * mx)
  qx[i.openage]       <- ifelse(is.na(qx[i.openage]), NA, 1)
  ax[i.openage]       <- 1 / mx[i.openage]                   
  px 				    <- 1 - qx
  px[is.nan(px)]      <- 0
  lx 			        <- c(RADIX, RADIX * cumprod(px[1:OPENAGE]))
  dx 				    <- lx * qx
  Lx 				    <- lx - (1 - ax) * dx
  Lx[i.openage ]	    <- lx[i.openage ] * ax[i.openage ]
  Tx 				    <- c(rev(cumsum(rev(Lx[1:OPENAGE]))),0) + Lx[i.openage]
  ex 				    <- Tx / lx
  ex[1]
})

LifeTable      <- function(mx,sex = "f"){
  mx <- as.matrix(mx)
  i.openage <- nrow(mx)
  ax        <- mx * 0 + .5
  ax[1, ]   <- AKm02a0(m0 = mx[1, ], sex = sex)
  qx        <- mx / (1 + (1 - ax) * mx)        
  qx[i.openage, ]       <- ifelse(is.na(qx[i.openage, ]), NA, 1)
  ax[i.openage, ]       <- 1 / mx[i.openage, ]                   
  px 				      <- 1 - qx 																				
  px[is.nan(px)]  <- 0 
  lx 			        <- apply(px, 2, function(px., RADIX, OPENAGE){ 		
    if (all(is.na(px.))) {
      px.
    } else {
      c(RADIX, RADIX * cumprod(px.[1:OPENAGE]))
    }
  }, RADIX = 1, OPENAGE = i.openage - 1
  )
  rownames(lx)    <- 0:(i.openage - 1) 
  dx 				      <- lx * qx 																				
  Lx 				      <- lx - (1 - ax) * dx 														
  Lx[i.openage, ]	<- lx[i.openage, ] * ax[i.openage, ]
  Tx 				      <- apply(Lx, 2, function(Lx., i.openage, OPENAGE){
    c(rev(cumsum(rev(Lx.[1:OPENAGE]))),0) + Lx.[i.openage]	
  }, OPENAGE = i.openage - 1, i.openage = i.openage
  )
  rownames(Tx)    <- rownames(lx)
  ex 				      <- Tx / lx 	                              
  list(e0=ex[1,],ex=ex,lx=lx,mx=mx)
}



h.frommx <- function(mx,sex){
  i.openage <- length(mx)
  OPENAGE   <- i.openage - 1
  RADIX     <- 1
  ax        <- mx * 0 + .5
  ax[1]     <- AKm02a0(m0 = mx[1], sex = sex)
  qx        <- mx / (1 + (1 - ax) * mx)
  qx[i.openage]       <- ifelse(is.na(qx[i.openage]), NA, 1)
  ax[i.openage]       <- 1 / mx[i.openage]                   
  px 				    <- 1 - qx
  px[is.nan(px)]      <- 0
  lx 			        <- c(RADIX, RADIX * cumprod(px[1:OPENAGE]))
  dx 				    <- lx * qx
  Lx 				    <- lx - (1 - ax) * dx
  Lx[i.openage ]	    <- lx[i.openage ] * ax[i.openage ]
  Tx 				    <- c(rev(cumsum(rev(Lx[1:OPENAGE]))),0) + Lx[i.openage]
  ex 				    <- Tx / lx
  l <- length(ex)
  v <- (sum(dx[-l]* (ex[-l] + ax[-l]*(ex[-1]-ex[-l]) )) + ex[l])
  k <- v/ex[1]
  eq <- -log(k)
  return(eq)
}

my.cv.frommx <- function(mx,sex){
  i.openage <- length(mx)
  OPENAGE   <- i.openage - 1
  RADIX     <- 1
  ax        <- mx * 0 + .5
  ax[1]     <- AKm02a0(m0 = mx[1], sex = sex)
  qx        <- mx / (1 + (1 - ax) * mx)
  qx[i.openage]       <- ifelse(is.na(qx[i.openage]), NA, 1)
  ax[i.openage]       <- 1 / mx[i.openage]                   
  px 				    <- 1 - qx
  px[is.nan(px)]      <- 0
  lx 			        <- c(RADIX, RADIX * cumprod(px[1:OPENAGE]))
  dx 				    <- lx * qx
  Lx 				    <- lx - (1 - ax) * dx
  Lx[i.openage ]	    <- lx[i.openage ] * ax[i.openage ]
  Tx 				    <- c(rev(cumsum(rev(Lx[1:OPENAGE]))),0) + Lx[i.openage]
  ex 				    <- Tx / lx
  age           <- 0:(i.openage-1)
  vx <- sum(dx*(age+ax-ex[1L])^2)
  cv <- sqrt(vx)/ex[1L]
  cv.inv <- -log(cv)
  cv.inv
}

log.G.frommx           <- function(mx,sex="f"){
  i.openage <- length(mx)
  OPENAGE   <- i.openage - 1
  RADIX     <- 1
  ax        <- mx * 0 + .5
  ax[1]     <- AKm02a0(m0 = mx[1], sex = sex)
  qx        <- mx / (1 + (1 - ax) * mx)
  qx[i.openage]       <- ifelse(is.na(qx[i.openage]), NA, 1)
  ax[i.openage]       <- 1 / mx[i.openage]                   
  px 				    <- 1 - qx
  px[is.nan(px)]      <- 0
  lx 			        <- c(RADIX, RADIX * cumprod(px[1:OPENAGE]))
  dx 				    <- lx * qx
  Lx 				    <- lx - (1 - ax) * dx
  Lx[i.openage ]	    <- lx[i.openage ] * ax[i.openage ]
  Tx 				    <- c(rev(cumsum(rev(Lx[1:OPENAGE]))),0) + Lx[i.openage]
  ex 				    <- Tx / lx
  age           <- 0:(i.openage-1) + ax
  e             <- rep(1, length(age))
  D             <- outer(dx, dx)
  X_            <- abs(e%*%t(age) - age%*%t(e))
  G             <- sum(D*X_)/(2*ex[1L])
  g             <- -log(G)
  g
}

### Some functions to create evaluating graphs

# A function that returns the year range of the differences according to the lag
Y <- function(Year,lag.2){  
  seq(min(Year),max(Year)-lag.2,1)
}


W_h <- function(ex,ax,dx,lx){
  l     <- length(ax)
  vx    <- dx*(ax*c(ex[-1L], 0) + (1-ax)*ex)
  vx[l] <-  ex[l]
  v     <- sum(vx)
  
  H.plus<- vx/(ex*lx)
  
  W_eta <- 1/ex[1L]+(1/v)*(1+log(lx)-H.plus)
  W_eta[is.infinite(W_eta)] <- NA
  W_eta
}

W_v <- function(ex,ax,x,dx,lx){
  V      <- sum(dx*(x+ax-ex[1L])^2)
  C_plus <- rev(cumsum(rev(lx*(x+ax-ex[1L]))))/rev(cumsum(rev(lx)))
  W_cv <- (1/ex[1L])-(1/V)*(C_plus)
  W_cv[is.infinite(W_cv)] <- NA
  W_cv
}

W_g <- function(ex,ax,x,dx,lx){
  v       <- sum(lx^2, na.rm = T)
  n1      <- 2*rev(cumsum(rev(lx^2)))/(ex*lx)
  W_gamma <-  (v/(ex[1L]-v))*(n1/v-1/ex[1L])
  W_gamma[is.infinite(W_gamma)] <- NA
  W_gamma
}



#get the modal age
#Dx = SWE.DxEx[Year == 1900,]$Deaths
#Ex = SWE.DxEx[Year == 1900,]$Exposures
#x = 30:105

modal.age.function <- function(Dx,Ex,x = 30:105){
  Dx <- Dx[(x+1)]
  Ex <- Ex[(x+1)]
  
  m <- length(x)
  
  ## expanded ages at finer grid ages
  delta <- 0.1
  xs    <- seq(min(x), max(x), delta)
  ms    <- length(xs)
  
  ## regression weights in case of zero exposures
  w <- Dx*0 + 1
  w[Ex==0] <- 0
  
  xl <- min(x)
  xr <- max(x)
  xmin <- round(xl - 0.01 * (xr - xl),3)
  xmax <- round(xr + 0.01 * (xr - xl),3)
  ndx <- floor(m/3)
  deg <- 3
  
  B <- MortSmooth_bbase(x, xmin, xmax, ndx, deg)
  Bs <- MortSmooth_bbase(xs, xmin, xmax, ndx, deg)
  
  if (any(is.na(Dx))) Dx[is.na(Dx)] <- 0
  
  fit1D <- Mort1Dsmooth(x=x, y=Dx, offset=log(Ex),w=w,
                        ndx = ndx, deg = deg, pord = 2)
  
  lMX.smooth <- Bs %*% fit1D$coef
  FXs <- matrix(0, nrow=ms, ncol=1)
  I <- diag(ms)
  C <- lower.tri(I, diag = TRUE)
  C[C==1] <- -delta 
  
  hx  <- exp(lMX.smooth)
  dx <-  hx * exp(C%*%hx)
  
  M <- xs[which.max(dx)]
  M
}


get.contrib.function <- function(contrib,age.x){
  
  #get contributions below and above
  if (age.x == -1)  age.x <- 25
  a_i   <- as.integer(trunc(age.x))
  b_i   <- age.x - a_i
  
  Below <- sum(contrib[1:(a_i+1)]) + contrib[(a_i+2)]*b_i
  
  Above <- sum(contrib[(a_i+3):length(contrib)]) + contrib[(a_i+2)]*(1- b_i)
  
  y <- c(Below,Above)
  
  y
}


R.sqr <- function(lbar,e0){
  r1 <- lm(lbar ~ e0)
  v  <- summary(r1)$r.squared
  v
}


dw <- function(lbar,e0){
  r1 <- lm(lbar ~ e0)
  v  <- dwtest(r1)$statistic
  v
}

t.v <- function(dif.lbar,dif.e0){
  r1 <- lm(dif.lbar ~ dif.e0)
  v  <- summary(r1)
  v$coefficients[4]
}

# Some graphical settings
my.settings1 <- list(  
  strip.background=list(col="grey"),
  strip.border=list(col="black")  
)

# A function to make colors transparent
makeTransparent <- function(someColor, alpha=100){
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                              blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}


