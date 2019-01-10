02\_spTimer
================
Logan Wu
1/9/2019

From <https://github.com/cran/spTimer/blob/master/demo/nyExample.R>

New York data example
=====================

``` r
# Read data 
data(NYdata)
s<-c(8,11,12,14,18,21,24,28)
DataFit<-spT.subset(data=NYdata, var.name=c("s.index"), s=s, reverse=TRUE) 
DataFit<-subset(DataFit, with(DataFit, !(Day %in% c(30, 31) & Month == 8)))
DataValPred<-spT.subset(data=NYdata, var.name=c("s.index"), s=s) 
DataValPred<-subset(DataValPred, with(DataValPred, !(Day %in% c(30, 31) & Month == 8)))


# Figure 7
coords<-as.matrix(unique(cbind(DataFit[,2:3])))
pred.coords<-as.matrix(unique(cbind(DataValPred[,2:3])))
map(database="state",regions="new york")
points(coords,pch=19,col=3)
points(coords,pch=1,col=1)
points(pred.coords,pch=3,col=4)
legend(x=-77.5,y=41.5,col=c(3,4),pch=c(19,3),cex=0.8,legend=c("Fitted sites","Validation sites"))
```

![](02_spTimer_files/figure-markdown_github/unnamed-chunk-1-1.png)

``` r
# Fit GP model 
set.seed(11)
post.gp <- spT.Gibbs(formula=o8hrmax ~cMAXTMP+WDSP+RH,data=DataFit, 
                     model="GP", coords=~Longitude+Latitude, scale.transform="SQRT",
                     spatial.decay=spT.decay(distribution=Gamm(2,1),tuning=0.1))
```

    ## 
    ##  Output: GP models 
    ## ---------------------------------------------------------------
    ##  Sampled: 5000 of 5000, 100.00%.
    ##  Batch Acceptance Rate (phi): 32.33%
    ##  Checking Parameters: 
    ##    phi: 0.0098, sig2eps: 0.0131, sig2eta: 0.5065
    ##    beta[1]: 2.5802   beta[2]: 0.1578   beta[3]: 0.0492   beta[4]: -0.0411
    ## ---------------------------------------------------------------
    ## ## 
    ## # nBurn =  1000 , Iterations =  5000 . 
    ## # Overall Acceptance Rate (phi) =  32.32 % 
    ## ## 
    ## ##
    ## # Elapsed time: 14.62 Sec.
    ## ##
    ## 
    ## # Model: GP

``` r
print(post.gp)
```

    ## -----------------------------------------------------
    ## Model: GP
    ## Call: o8hrmax ~ cMAXTMP + WDSP + RH
    ## Iterations: 5000
    ## nBurn: 1000
    ## Acceptance rate for phi (%): 32.32
    ## -----------------------------------------------------
    ##         Goodness.of.fit Penalty   PMCC
    ## values:          258.53  636.13 894.66
    ## -----------------------------------------------------
    ## Computation time: 14.62  - Sec.

``` r
summary(post.gp)
```

    ## -----------------------------------------------------
    ## Model: GP
    ## Call: o8hrmax ~ cMAXTMP + WDSP + RH
    ## Iterations: 5000
    ## nBurn: 1000
    ## Acceptance rate for phi (%): 32.32
    ## -----------------------------------------------------
    ##         Goodness.of.fit Penalty   PMCC
    ## values:          258.53  636.13 894.66
    ## -----------------------------------------------------
    ## Computation time: 14.62  - Sec.
    ## -----------------------------------------------------
    ## Parameters:
    ##                Mean  Median     SD Low2.5p Up97.5p
    ## (Intercept)  3.1667  3.0961 0.8458  1.7290  5.0605
    ## cMAXTMP      0.1339  0.1370 0.0261  0.0714  0.1756
    ## WDSP         0.0757  0.0771 0.0308  0.0113  0.1326
    ## RH          -0.0502 -0.0593 0.0982 -0.2116  0.1772
    ## sig2eps      0.0166  0.0159 0.0042  0.0105  0.0265
    ## sig2eta      0.8468  0.6991 0.4935  0.4803  2.3478
    ## phi          0.0068  0.0070 0.0022  0.0017  0.0107
    ## -----------------------------------------------------

``` r
# Spatial prediction for the GP model
set.seed(11)
pred.gp <- predict(post.gp, newdata=DataValPred, newcoords=~Longitude+Latitude)
```

    ## 
    ##  Prediction: GP models 
    ## #
    ## # Tolerance Limit (unit): 2
    ## # Location distances are alright 
    ## #
    ## -------------------------------------------------
    ##   Sampled: 400 of 4000, 10.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 800 of 4000, 20.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 1200 of 4000, 30.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 1600 of 4000, 40.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 2000 of 4000, 50.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 2400 of 4000, 60.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 2800 of 4000, 70.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 3200 of 4000, 80.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 3600 of 4000, 90.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 4000 of 4000, 100.00%
    ## -------------------------------------------------
    ## ## 
    ## # Predicted samples and summary statistics are given.
    ## # nBurn =  1000 . Iterations =  5000 . 
    ## ## 
    ## ##
    ## # Elapsed time: 7.62 Sec.
    ## ##

``` r
print(pred.gp)
```

    ## --------------------------------------
    ## Spatial prediction with Model: GP 
    ## Covariance function: exponential 
    ## Distance method: geodetic:km 
    ## Computation time:  7.62  - Sec. 
    ## --------------------------------------

``` r
names(pred.gp)
```

    ##  [1] "pred.samples"         "pred.coords"          "distance.method"     
    ##  [4] "Distance.matrix.pred" "cov.fnc"              "predN"               
    ##  [7] "Mean"                 "Median"               "SD"                  
    ## [10] "Low"                  "Up"                   "computation.time"    
    ## [13] "model"                "type"

``` r
# model summary
summary(post.gp)
```

    ## -----------------------------------------------------
    ## Model: GP
    ## Call: o8hrmax ~ cMAXTMP + WDSP + RH
    ## Iterations: 5000
    ## nBurn: 1000
    ## Acceptance rate for phi (%): 32.32
    ## -----------------------------------------------------
    ##         Goodness.of.fit Penalty   PMCC
    ## values:          258.53  636.13 894.66
    ## -----------------------------------------------------
    ## Computation time: 14.62  - Sec.
    ## -----------------------------------------------------
    ## Parameters:
    ##                Mean  Median     SD Low2.5p Up97.5p
    ## (Intercept)  3.1667  3.0961 0.8458  1.7290  5.0605
    ## cMAXTMP      0.1339  0.1370 0.0261  0.0714  0.1756
    ## WDSP         0.0757  0.0771 0.0308  0.0113  0.1326
    ## RH          -0.0502 -0.0593 0.0982 -0.2116  0.1772
    ## sig2eps      0.0166  0.0159 0.0042  0.0105  0.0265
    ## sig2eta      0.8468  0.6991 0.4935  0.4803  2.3478
    ## phi          0.0068  0.0070 0.0022  0.0017  0.0107
    ## -----------------------------------------------------

``` r
# validation criteria
spT.validation(DataValPred$o8hrmax,c(pred.gp$Median))  
```

    ##     MSE    RMSE     MAE    MAPE    BIAS   rBIAS   rMSEP 
    ## 43.5794  6.6015  5.0941 11.8964  0.6683  0.0143  0.2636

``` r
###############################
## For surface plots         ##
## Press Enter:              ##
###############################
pause()
```

    ## NULL

``` r
nItr=100
nBurn=50
# Predict on grids
data(NYgrid)
set.seed(11)
post.gp2 <- spT.Gibbs(formula=o8hrmax ~cMAXTMP+WDSP+RH,data=NYdata, 
                      model="GP", coords=~Longitude+Latitude, scale.transform="SQRT",
                      spatial.decay=spT.decay(distribution=Gamm(2,1),tuning=0.1))
```

    ## 
    ##  Output: GP models 
    ## ---------------------------------------------------------------
    ##  Sampled: 5000 of 5000, 100.00%.
    ##  Batch Acceptance Rate (phi): 30.19%
    ##  Checking Parameters: 
    ##    phi: 0.0142, sig2eps: 0.0196, sig2eta: 0.7475
    ##    beta[1]: 1.8909   beta[2]: 0.1714   beta[3]: 0.1157   beta[4]: -0.0728
    ## ---------------------------------------------------------------
    ## ## 
    ## # nBurn =  1000 , Iterations =  5000 . 
    ## # Overall Acceptance Rate (phi) =  30.18 % 
    ## ## 
    ## ##
    ## # Elapsed time: 24.79 Sec.
    ## ##
    ## 
    ## # Model: GP

``` r
set.seed(11)
grid.pred <- predict(post.gp2, newdata=NYgrid, newcoords=~Longitude+Latitude)
```

    ## 
    ##  Prediction: GP models 
    ## #
    ## # Tolerance Limit (unit): 2
    ## # Location distances are alright 
    ## #
    ## -------------------------------------------------
    ##   Sampled: 400 of 4000, 10.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 800 of 4000, 20.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 1200 of 4000, 30.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 1600 of 4000, 40.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 2000 of 4000, 50.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 2400 of 4000, 60.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 2800 of 4000, 70.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 3200 of 4000, 80.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 3600 of 4000, 90.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 4000 of 4000, 100.00%
    ## -------------------------------------------------
    ## ## 
    ## # Predicted samples and summary statistics are given.
    ## # nBurn =  1000 . Iterations =  5000 . 
    ## ## 
    ## ##
    ## # Elapsed time: 1 Min. 48.46 Sec.
    ## ##

``` r
coords<-unique(NYdata[,c("Longitude","Latitude")])
grid.coords<-unique(NYgrid[,c("Longitude","Latitude")])
true.val<-matrix(NYdata$o8hrmax,62,28)
grid.val<-matrix(grid.pred$Median,62,dim(grid.coords)[[1]])
grid.sd<-matrix(grid.pred$SD,62,dim(grid.coords)[[1]])

surfplot<-function(day=60, val, ...)
{
  z <- val
  surf<-cbind(grid.coords,z[day,])
  surf<-mba.surf(surf,200,200)$xyz
  surf<-fnc.delete.map.XYZ(xyz=surf)
  #map(database="state",regions="new york")
  image.plot(surf, xlab="Longitude",ylab="Latitude",axes=F, ...)
  contour(surf,nlevels=10,lty=3,add=T)
  map(database="state",regions="new york",add=T)
  axis(1);axis(2)
}

# prediction for day 60
day<-60
surfplot(day, val=grid.val, col = rainbow_hcl(100, start = 200, end = 0))
text(coords,labels=round(true.val[day,],1),cex=0.8,col=1)
```

![](02_spTimer_files/figure-markdown_github/unnamed-chunk-1-2.png)

``` r
# sd for day 60
# Press Enter:
pause()
```

    ## NULL

``` r
# sd for day 60
day<-60
surfplot(day, val=grid.sd,col = diverge_hcl(100, h = c(246, 40), c = 96, l = c(65, 90)))
points(coords,pch=19,cex=1,col=2)
points(coords,pch=1,cex=1,col=1)
```

![](02_spTimer_files/figure-markdown_github/unnamed-chunk-1-3.png)

``` r
###############################################################
## To run more code on model and MCMC diagnosis press Enter: ##
###############################################################
pause()
```

    ## NULL

``` r
rm(post.gp2);

# More code for summary and plots
summary(post.gp, digits = 4)
```

    ## -----------------------------------------------------
    ## Model: GP
    ## Call: o8hrmax ~ cMAXTMP + WDSP + RH
    ## Iterations: 5000
    ## nBurn: 1000
    ## Acceptance rate for phi (%): 32.32
    ## -----------------------------------------------------
    ##         Goodness.of.fit Penalty   PMCC
    ## values:          258.53  636.13 894.66
    ## -----------------------------------------------------
    ## Computation time: 14.62  - Sec.
    ## -----------------------------------------------------
    ## Parameters:
    ##                Mean  Median     SD Low2.5p Up97.5p
    ## (Intercept)  3.1667  3.0961 0.8458  1.7290  5.0605
    ## cMAXTMP      0.1339  0.1370 0.0261  0.0714  0.1756
    ## WDSP         0.0757  0.0771 0.0308  0.0113  0.1326
    ## RH          -0.0502 -0.0593 0.0982 -0.2116  0.1772
    ## sig2eps      0.0166  0.0159 0.0042  0.0105  0.0265
    ## sig2eta      0.8468  0.6991 0.4935  0.4803  2.3478
    ## phi          0.0068  0.0070 0.0022  0.0017  0.0107
    ## -----------------------------------------------------

``` r
summary(post.gp,pack="coda") # mcmc summary statistics using coda package
```

    ## 
    ## #### MCMC summary statistics using coda package ####

    ## 
    ## Iterations = 1:4000
    ## Thinning interval = 1 
    ## Number of chains = 1 
    ## Sample size per chain = 4000 
    ## 
    ## 1. Empirical mean and standard deviation for each variable,
    ##    plus standard error of the mean:
    ## 
    ##                 Mean       SD  Naive SE Time-series SE
    ## (Intercept)  3.16670 0.845772 1.337e-02      4.581e-02
    ## cMAXTMP      0.13393 0.026125 4.131e-04      1.842e-03
    ## WDSP         0.07571 0.030845 4.877e-04      1.192e-03
    ## RH          -0.05023 0.098164 1.552e-03      6.258e-03
    ## sig2eps      0.01659 0.004218 6.670e-05      7.615e-05
    ## sig2eta      0.84676 0.493471 7.802e-03      5.897e-02
    ## phi          0.00676 0.002218 3.506e-05      2.398e-04
    ## 
    ## 2. Quantiles for each variable:
    ## 
    ##                 2.5%       25%      50%      75%   97.5%
    ## (Intercept)  1.73321  2.617359  3.09615 3.630797 5.06122
    ## cMAXTMP      0.07147  0.121258  0.13699 0.151514 0.17556
    ## WDSP         0.01155  0.056104  0.07714 0.096396 0.13261
    ## RH          -0.21154 -0.116956 -0.05933 0.002279 0.17725
    ## sig2eps      0.01048  0.013519  0.01588 0.018782 0.02655
    ## sig2eta      0.48029  0.588800  0.69909 0.902017 2.34830
    ## phi          0.00176  0.005264  0.00695 0.008415 0.01070

``` r
confint(post.gp)
```

    ##                    2.5%      97.5%
    ## (Intercept)  1.73321406 5.06121759
    ## cMAXTMP      0.07147048 0.17555850
    ## WDSP         0.01155116 0.13260725
    ## RH          -0.21153839 0.17724623
    ## sig2eps      0.01047848 0.02654640
    ## sig2eta      0.48028874 2.34829723
    ## phi          0.00176027 0.01070057

``` r
# MCMC chains
plot(post.gp)
```

![](02_spTimer_files/figure-markdown_github/unnamed-chunk-1-4.png)![](02_spTimer_files/figure-markdown_github/unnamed-chunk-1-5.png)

``` r
# residual plot
plot(post.gp, residuals=TRUE)
```

![](02_spTimer_files/figure-markdown_github/unnamed-chunk-1-6.png)![](02_spTimer_files/figure-markdown_github/unnamed-chunk-1-7.png)

``` r
# fitted surface 
plot.spT<-function(x, residuals=FALSE, surface=NULL, time=c(1), a3d=FALSE, 
                   points=FALSE, title=TRUE, ...){
  if(is.null(surface) & a3d==FALSE){
    if(as.logical(residuals)==FALSE){
      tmp<-as.mcmc(x)
      plot(tmp, ...)}
    else{
      plot(x$fitted[,1],residuals(x),ylab="Residuals",xlab="Fitted values")
      abline(h=0,lty=2);title("Residuals vs Fitted")
      par(ask=TRUE)
      qqnorm(residuals(x));qqline(residuals(x),lty=2)}} 
  else {
    if(is.null(surface)){
      stop("\n# Error: surface should be defined as 'Mean' or 'SD'. \n")}   
    if(!surface %in% c("Mean","SD")){
      stop("\n# Error: surface only takes 'Mean' or 'SD'. \n")}
    z<-array(fitted(x)[,paste(surface)],dim=c(x$T*x$r,x$n))
    xyz<-cbind(x$coords,c(z[time,]))
    xyz<-interp(x=xyz[,1],y=xyz[,2],z=xyz[,3],
                xo=seq(min(xyz[,1]),max(xyz[,1]),length=150),
                yo=seq(min(xyz[,2]), max(xyz[,2]), length = 150))
    if(a3d==TRUE){
      persp(x=xyz$x,y=xyz$y,z=xyz$z, xlab="x",ylab="y",zlab="z", ...)->res}
    else{
      image.plot(xyz, ...) 
      if(points != FALSE){
        points(x$coords,pch=16,cex=0.8)}}
    if(title==TRUE){
      title(main=paste("Time point: (t=",time,")",sep=""))}}}
contour.spT<-function(x, surface="Mean", time=c(1), ...){
  z<-array(fitted(x)[,paste(surface)],dim=c(x$T*x$r,x$n))
  xyz<-cbind(x$coords,c(z[time,]))
  xyz<-interp(x=xyz[,1], y=xyz[,2], z=xyz[,3], xo=seq(min(xyz[,1]), max(xyz[,1]), length = 150),
              yo=seq(min(xyz[,2]), max(xyz[,2]), length = 150),linear = TRUE, extrap=FALSE, 
              duplicate = "error", dupfun = NULL, ncp = NULL)
  contour(xyz, ...) 
}
plot(post.gp, surface="Mean")
```

![](02_spTimer_files/figure-markdown_github/unnamed-chunk-1-8.png)

``` r
# fitted surface 3d plot
plot(post.gp, surface="Mean", a3d=TRUE)
```

![](02_spTimer_files/figure-markdown_github/unnamed-chunk-1-9.png)

``` r
# some other R functions 
coef(post.gp)
```

    ## (Intercept)     cMAXTMP        WDSP          RH     sig2eps     sig2eta 
    ##      3.1667      0.1339      0.0757     -0.0502      0.0166      0.8468 
    ##         phi 
    ##      0.0068

``` r
formula(post.gp)
```

    ## o8hrmax ~ cMAXTMP + WDSP + RH

``` r
terms(post.gp)
```

    ## o8hrmax ~ cMAXTMP + WDSP + RH
    ## attr(,"variables")
    ## list(o8hrmax, cMAXTMP, WDSP, RH)
    ## attr(,"factors")
    ##         cMAXTMP WDSP RH
    ## o8hrmax       0    0  0
    ## cMAXTMP       1    0  0
    ## WDSP          0    1  0
    ## RH            0    0  1
    ## attr(,"term.labels")
    ## [1] "cMAXTMP" "WDSP"    "RH"     
    ## attr(,"order")
    ## [1] 1 1 1
    ## attr(,"intercept")
    ## [1] 1
    ## attr(,"response")
    ## [1] 1
    ## attr(,".Environment")
    ## <environment: R_GlobalEnv>

``` r
head(model.frame(post.gp))
```

    ##   o8hrmax  cMAXTMP     WDSP       RH
    ## 1   53.88 27.85772 5.459953 2.766221
    ## 2   57.13 30.11563 8.211767 3.197750
    ## 3   72.00 30.00001 4.459581 3.225186
    ## 4   36.63 27.89656 3.692225 4.362334
    ## 5   42.63 25.65698 4.374314 3.950320
    ## 6   30.88 24.61968 4.178086 3.420533

``` r
head(model.matrix(post.gp))
```

    ##   (Intercept)  cMAXTMP     WDSP       RH
    ## 1           1 27.85772 5.459953 2.766221
    ## 2           1 30.11563 8.211767 3.197750
    ## 3           1 30.00001 4.459581 3.225186
    ## 4           1 27.89656 3.692225 4.362334
    ## 5           1 25.65698 4.374314 3.950320
    ## 6           1 24.61968 4.178086 3.420533

``` r
# Model selection criteria
post.gp$PMCC 
```

    ##         Goodness.of.fit Penalty   PMCC
    ## values:          258.53  636.13 894.66

``` r
# MCMC diagnostics using coda
# autocorr diagnostics
autocorr.diag(as.mcmc(post.gp))
```

    ##        (Intercept)    cMAXTMP       WDSP         RH      sig2eps   sig2eta
    ## Lag 0   1.00000000 1.00000000 1.00000000 1.00000000  1.000000000 1.0000000
    ## Lag 1   0.15204800 0.33325200 0.10776542 0.29741728  0.088564871 0.7455544
    ## Lag 5   0.11103062 0.27830150 0.09406967 0.24215049  0.008313396 0.6872080
    ## Lag 10  0.11501732 0.25836345 0.08671756 0.22422662 -0.014550731 0.5963846
    ## Lag 50  0.02065186 0.07996277 0.04406383 0.08551031  0.009812689 0.1506704
    ##              phi
    ## Lag 0  1.0000000
    ## Lag 1  0.9431997
    ## Lag 5  0.7566721
    ## Lag 10 0.6155580
    ## Lag 50 0.2520160

``` r
# Raftery and Lewis's diagnostic
raftery.diag(post.gp)
```

    ## 
    ## Quantile (q) = 0.025
    ## Accuracy (r) = +/- 0.005
    ## Probability (s) = 0.95 
    ##                                                    
    ##              Burn-in  Total Lower bound  Dependence
    ##              (M)      (N)   (Nmin)       factor (I)
    ##  (Intercept) 2        3635  3746          0.970    
    ##  cMAXTMP     20       21144 3746          5.640    
    ##  WDSP        3        4112  3746          1.100    
    ##  RH          2        3561  3746          0.951    
    ##  sig2eps     2        3866  3746          1.030    
    ##  sig2eta     6        7990  3746          2.130    
    ##  phi         64       69371 3746         18.500

``` r
# Diagnostics using more than one chain
set.seed(22)
post.gp2 <- NULL
post.gp2 <- spT.Gibbs(formula=o8hrmax ~cMAXTMP+WDSP+RH,data=DataFit, 
                      model="GP", coords=~Longitude+Latitude, scale.transform="SQRT",
                      initials=spT.initials(model="GP",phi=1,sig2eta=1),
                      spatial.decay=spT.decay(distribution=Gamm(2,1),tuning=0.1))
```

    ## 
    ##  Output: GP models 
    ## ---------------------------------------------------------------
    ##  Sampled: 5000 of 5000, 100.00%.
    ##  Batch Acceptance Rate (phi): 31.31%
    ##  Checking Parameters: 
    ##    phi: 0.0088, sig2eps: 0.0294, sig2eta: 0.7169
    ##    beta[1]: 2.7399   beta[2]: 0.1537   beta[3]: 0.0868   beta[4]: -0.1008
    ## ---------------------------------------------------------------
    ## ## 
    ## # nBurn =  1000 , Iterations =  5000 . 
    ## # Overall Acceptance Rate (phi) =  31.3 % 
    ## ## 
    ## ##
    ## # Elapsed time: 14.08 Sec.
    ## ##
    ## 
    ## # Model: GP

``` r
mcobj<-list(as.mcmc(post.gp),as.mcmc(post.gp2))
mcobj<-as.mcmc.list(mcobj)
# acf plot
acfplot(mcobj)
```

![](02_spTimer_files/figure-markdown_github/unnamed-chunk-1-10.png)

``` r
# Geweke's convergence diagnostic
geweke.diag(mcobj)
```

    ## [[1]]
    ## 
    ## Fraction in 1st window = 0.1
    ## Fraction in 2nd window = 0.5 
    ## 
    ## (Intercept)     cMAXTMP        WDSP          RH     sig2eps     sig2eta 
    ##      -2.044       2.111       2.710      -2.223       1.031      -1.921 
    ##         phi 
    ##       1.691 
    ## 
    ## 
    ## [[2]]
    ## 
    ## Fraction in 1st window = 0.1
    ## Fraction in 2nd window = 0.5 
    ## 
    ## (Intercept)     cMAXTMP        WDSP          RH     sig2eps     sig2eta 
    ##       2.737      -2.315      -5.142       3.465       1.693       3.137 
    ##         phi 
    ##      -3.485

``` r
# Gelman and Rubin's diagnostic
gelman.diag(mcobj)
```

    ## Potential scale reduction factors:
    ## 
    ##             Point est. Upper C.I.
    ## (Intercept)       1.01       1.02
    ## cMAXTMP           1.01       1.04
    ## WDSP              1.00       1.01
    ## RH                1.01       1.05
    ## sig2eps           1.00       1.00
    ## sig2eta           1.01       1.03
    ## phi               1.03       1.14
    ## 
    ## Multivariate psrf
    ## 
    ## 1.03

``` r
gelman.plot(mcobj)
```

![](02_spTimer_files/figure-markdown_github/unnamed-chunk-1-11.png)

``` r
#####################################################
## For Temporal prediction/forecast using GP model ##
## 1. In the unobserved locations                  ## 
## Press Enter:                                    ##
#####################################################
pause()
```

    ## NULL

``` r
# Temporal  prediction/forecast for the GP model
# 1. In the unobserved locations
# Read data
data(NYdata)
DataValFore<-spT.subset(data=NYdata, var.name=c("s.index"), s=c(8,11,12,14,18,21,24,28)) 
DataValFore<-subset(DataValFore, with(DataValFore, (Day %in% c(30, 31) & Month == 8)))
# Two-step ahead forecast, i.e., in day 61 and 62 
# in the unobserved locations using output from spT.Gibbs
set.seed(11)
fore.gp <- predict(post.gp, newdata=DataValFore, newcoords=~Longitude+Latitude, 
                   type="temporal", foreStep=2)
```

    ## 
    ##  Forecast: GP models 
    ## -------------------------------------------------
    ##   Sampled: 400 of 4000, 10.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 800 of 4000, 20.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 1200 of 4000, 30.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 1600 of 4000, 40.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 2000 of 4000, 50.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 2400 of 4000, 60.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 2800 of 4000, 70.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 3200 of 4000, 80.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 3600 of 4000, 90.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 4000 of 4000, 100.00%
    ## -------------------------------------------------
    ## ## 
    ## # Forecast samples and summary statistics are given.
    ## # nBurn =  0 . Iterations =  4000 . 
    ## ## 
    ## ##
    ## # Elapsed time: 1.06 Sec.
    ## ##

``` r
print(fore.gp)
```

    ## --------------------------------------
    ## Temporal prediction/forecast with Model: GP 
    ## Covariance function: exponential 
    ## Distance method: geodetic:km 
    ## Computation time:  1.06  - Sec. 
    ## --------------------------------------

``` r
names(fore.gp)
```

    ##  [1] "fore.samples"     "fore.coords"      "distance.method" 
    ##  [4] "cov.fnc"          "obsData"          "fittedData"      
    ##  [7] "residuals"        "Mean"             "Median"          
    ## [10] "SD"               "Low"              "Up"              
    ## [13] "computation.time" "model"            "type"

``` r
# Forecast validations 
spT.validation(DataValFore$o8hrmax,c(fore.gp$Median)) 
```

    ##     MSE    RMSE     MAE    MAPE    BIAS   rBIAS   rMSEP 
    ## 26.6857  5.1658  4.7630 16.5490  4.3982  0.1471  1.1794

``` r
#####################################################
## For Temporal prediction/forecast using GP model ##
## 2. In the observed/fitted locations             ##
## Press Enter:                                    ##
#####################################################
pause()
```

    ## NULL

``` r
# Temporal  prediction/forecast for the GP model
# 2. In the observed/fitted locations
# Read data
s <-c(8,11,12,14,18,21,24,28)
DataFitFore<-spT.subset(data=NYdata, var.name=c("s.index"), s=s, reverse=TRUE) 
DataFitFore<-subset(DataFitFore, with(DataFitFore, (Day %in% c(30, 31) & Month == 8)))


# Two-step ahead forecast, i.e., in day 61 and 62, 
# in the fitted locations using output from spT.Gibbs
set.seed(11)
fore.gp <- predict(post.gp, newdata=DataFitFore, newcoords=~Longitude+Latitude, 
                   type="temporal", foreStep=2)
```

    ## 
    ##  Forecast: GP models 
    ## -------------------------------------------------
    ##   Sampled: 400 of 4000, 10.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 800 of 4000, 20.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 1200 of 4000, 30.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 1600 of 4000, 40.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 2000 of 4000, 50.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 2400 of 4000, 60.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 2800 of 4000, 70.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 3200 of 4000, 80.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 3600 of 4000, 90.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 4000 of 4000, 100.00%
    ## -------------------------------------------------
    ## ## 
    ## # Forecast samples and summary statistics are given.
    ## # nBurn =  0 . Iterations =  4000 . 
    ## ## 
    ## ##
    ## # Elapsed time: 1.5 Sec.
    ## ##

``` r
print(fore.gp)
```

    ## --------------------------------------
    ## Temporal prediction/forecast with Model: GP 
    ## Covariance function: exponential 
    ## Distance method: geodetic:km 
    ## Computation time:  1.5  - Sec. 
    ## --------------------------------------

``` r
names(fore.gp)
```

    ##  [1] "fore.samples"     "fore.coords"      "distance.method" 
    ##  [4] "cov.fnc"          "obsData"          "fittedData"      
    ##  [7] "residuals"        "Mean"             "Median"          
    ## [10] "SD"               "Low"              "Up"              
    ## [13] "computation.time" "model"            "type"

``` r
# Forecast validations 
spT.validation(DataFitFore$o8hrmax,c(fore.gp$Median)) # 
```

    ##     MSE    RMSE     MAE    MAPE    BIAS   rBIAS   rMSEP 
    ## 59.7160  7.7276  6.5397 24.3583  6.3839  0.2234  1.0981

``` r
#######################################
## Model data with spacetime classes ##
#######################################
pause()
```

    ## NULL

``` r
rm(post.gp); rm(post.gp2); rm(pred.gp); rm(fore.gp); rm(mcobj)

# Create a dataset with spacetime class
site<-unique(NYdata[,c("Longitude","Latitude")])
row.names(site)<-paste("point",1:nrow(site),sep="")
site <- SpatialPoints(site)
ymd<-as.POSIXct(seq(as.Date("2006-07-01"),as.Date("2006-08-31"),by=1))
# introduce class STFDF
newNYdata<-STFDF(sp=site, time=ymd, data=NYdata) # full lattice
class(newNYdata)
```

    ## [1] "STFDF"
    ## attr(,"package")
    ## [1] "spacetime"

``` r
# model with GP
set.seed(11)
post.gp <- spT.Gibbs(formula=o8hrmax ~cMAXTMP+WDSP+RH,   
                     data=newNYdata, model="GP", scale.transform="SQRT")
```

    ## 
    ##  Output: GP models 
    ## ---------------------------------------------------------------
    ##  Sampled: 5000 of 5000, 100.00%.
    ##  Batch Acceptance Rate (phi): 0.00%
    ##  Checking Parameters: 
    ##    phi: 0.0051, sig2eps: 0.0143, sig2eta: 1.1303
    ##    beta[1]: 3.1649   beta[2]: 0.1341   beta[3]: 0.1136   beta[4]: -0.0560
    ## ---------------------------------------------------------------
    ## ## 
    ## # nBurn =  1000 , Iterations =  5000 . 
    ## # Overall Acceptance Rate (phi) =  0 % 
    ## ## 
    ## ##
    ## # Elapsed time: 23.11 Sec.
    ## ##
    ## 
    ## # Model: GP

``` r
summary(post.gp)
```

    ## -----------------------------------------------------
    ## Model: GP
    ## Call: o8hrmax ~ cMAXTMP + WDSP + RH
    ## Iterations: 5000
    ## nBurn: 1000
    ## Acceptance rate for phi (%): 0
    ## -----------------------------------------------------
    ##         Goodness.of.fit Penalty PMCC
    ## values:          443.79 1300.21 1744
    ## -----------------------------------------------------
    ## Computation time: 23.11  - Sec.
    ## -----------------------------------------------------
    ## Parameters:
    ##               Mean Median     SD Low2.5p Up97.5p
    ## (Intercept) 2.6616 2.6600 0.9261  0.8737  4.5331
    ## cMAXTMP     0.1374 0.1379 0.0252  0.0872  0.1860
    ## WDSP        0.1117 0.1113 0.0340  0.0437  0.1770
    ## RH          0.0008 0.0022 0.0975 -0.1929  0.1902
    ## sig2eps     0.0183 0.0179 0.0039  0.0120  0.0270
    ## sig2eta     1.2092 1.2084 0.0406  1.1317  1.2905
    ## phi         0.0051 0.0051 0.0000  0.0051  0.0051
    ## -----------------------------------------------------

``` r
############################################
## Model spatial only data using GP model ##
############################################
pause()
```

    ## NULL

``` r
# spatial only data 
# we use meuse data from sp package
data(meuse)
# model with GP
set.seed(11)
post.gp <- spT.Gibbs(formula=zinc ~ sqrt(dist),   
                     data=meuse, model="GP", coords=~x+y, nItr=500, nBurn=100,
                     spatial.decay=spT.decay(distribution=Gamm(2,1), tuning=0.5),
                     distance.method="euclidean",scale.transform="LOG")
```

    ## 
    ##  Output: GP models 
    ## ---------------------------------------------------------------
    ##  Sampled: 500 of 500, 100.00%.
    ##  Batch Acceptance Rate (phi): 33.27%
    ##  Checking Parameters: 
    ##    phi: 0.0026, sig2eps: 0.0180, sig2eta: 0.3869
    ##    beta[1]: 6.9164   beta[2]: -1.9114
    ## ---------------------------------------------------------------
    ## ## 
    ## # nBurn =  100 , Iterations =  500 . 
    ## # Overall Acceptance Rate (phi) =  33.2 % 
    ## ## 
    ## ##
    ## # Elapsed time: 29.4 Sec.
    ## ##
    ## 
    ## # Model: GP

``` r
summary(post.gp)
```

    ## -----------------------------------------------------
    ## Model: GP
    ## Call: zinc ~ sqrt(dist)
    ## Iterations: 500
    ## nBurn: 100
    ## Acceptance rate for phi (%): 33.2
    ## -----------------------------------------------------
    ##         Goodness.of.fit Penalty   PMCC
    ## values:           30.59   74.44 105.03
    ## -----------------------------------------------------
    ## Computation time: 29.4  - Sec.
    ## -----------------------------------------------------
    ## Parameters:
    ##                Mean  Median     SD Low2.5p Up97.5p
    ## (Intercept)  6.9533  6.9678 0.2603  6.4208  7.3666
    ## sqrt(dist)  -2.4803 -2.5106 0.3979 -3.2197 -1.6376
    ## sig2eps      0.0177  0.0176 0.0021  0.0140  0.0225
    ## sig2eta      0.4986  0.3092 0.6094  0.1657  1.7491
    ## phi          0.0051  0.0045 0.0030  0.0012  0.0104
    ## -----------------------------------------------------

``` r
plot(post.gp, surface="Mean", title=FALSE)
```

![](02_spTimer_files/figure-markdown_github/unnamed-chunk-1-12.png)

``` r
###################################
## To run more code on AR model: ##
## Press Enter:                  ##
###################################
pause()
```

    ## NULL

``` r
rm(post.gp); 

##  AR model
# MCMC via Gibbs
set.seed(11)
post.ar <- spT.Gibbs(formula=o8hrmax ~cMAXTMP+WDSP+RH,   
                     data=DataFit, model="AR", coords=~Longitude+Latitude, 
                     scale.transform="SQRT")
```

    ## 
    ##  Output: AR models 
    ## ---------------------------------------------------------------
    ##  Sampled: 5000 of 5000, 100.00%.
    ##  Batch Acceptance Rate (phi): 0.00%
    ##  Checking Parameters: 
    ##    phi: 0.0051, rho: 0.4099, sig2eps: 0.0134, sig2eta: 0.7287
    ##    beta[1]: 2.2754   beta[2]: 0.0724   beta[3]: 0.0138   beta[4]: -0.0249
    ## ---------------------------------------------------------------
    ## ## 
    ## # nBurn =  1000 , Iterations =  5000 . 
    ## # Overall Acceptance Rate (phi) =  0 % 
    ## ## 
    ## ##
    ## # Elapsed time: 14.71 Sec.
    ## ##
    ## 
    ## # Model: AR

``` r
print(post.ar)
```

    ## -----------------------------------------------------
    ## Model: AR
    ## Call: o8hrmax ~ cMAXTMP + WDSP + RH
    ## Iterations: 5000
    ## nBurn: 1000
    ## Acceptance rate for phi (%): 0
    ## -----------------------------------------------------
    ##         Goodness.of.fit Penalty   PMCC
    ## values:          311.15  451.48 762.63
    ## -----------------------------------------------------
    ## Computation time: 14.71  - Sec.

``` r
# Summary and plots
summary(post.ar)
```

    ## -----------------------------------------------------
    ## Model: AR
    ## Call: o8hrmax ~ cMAXTMP + WDSP + RH
    ## Iterations: 5000
    ## nBurn: 1000
    ## Acceptance rate for phi (%): 0
    ## -----------------------------------------------------
    ##         Goodness.of.fit Penalty   PMCC
    ## values:          311.15  451.48 762.63
    ## -----------------------------------------------------
    ## Computation time: 14.71  - Sec.
    ## -----------------------------------------------------
    ## Parameters:
    ##                Mean  Median     SD Low2.5p Up97.5p
    ## (Intercept)  2.0631  2.0710 0.7491  0.5715  3.5470
    ## cMAXTMP      0.0849  0.0847 0.0206  0.0446  0.1249
    ## WDSP         0.0221  0.0217 0.0275 -0.0310  0.0756
    ## RH          -0.1112 -0.1110 0.0794 -0.2662  0.0426
    ## rho          0.4185  0.4185 0.0263  0.3682  0.4702
    ## sig2eps      0.0138  0.0134 0.0028  0.0094  0.0206
    ## sig2eta      0.7076  0.7068 0.0288  0.6533  0.7660
    ## phi          0.0051  0.0051 0.0000  0.0051  0.0051
    ## -----------------------------------------------------

``` r
summary(post.ar,pack="coda")
```

    ## 
    ## #### MCMC summary statistics using coda package ####

    ## 
    ## Iterations = 1:4000
    ## Thinning interval = 1 
    ## Number of chains = 1 
    ## Sample size per chain = 4000 
    ## 
    ## 1. Empirical mean and standard deviation for each variable,
    ##    plus standard error of the mean:
    ## 
    ##                  Mean       SD  Naive SE Time-series SE
    ## (Intercept)  2.063088 0.749144 0.0118450      1.160e-02
    ## cMAXTMP      0.084873 0.020567 0.0003252      3.172e-04
    ## WDSP         0.022138 0.027457 0.0004341      4.341e-04
    ## RH          -0.111226 0.079411 0.0012556      1.293e-03
    ## rho          0.418459 0.026347 0.0004166      4.134e-04
    ## sig2eps      0.013756 0.002821 0.0000446      4.991e-05
    ## sig2eta      0.707633 0.028751 0.0004546      4.546e-04
    ## phi          0.005079 0.000000 0.0000000      0.000e+00
    ## 
    ## 2. Quantiles for each variable:
    ## 
    ##                  2.5%       25%       50%       75%    97.5%
    ## (Intercept)  0.572282  1.553078  2.070981  2.567370 3.547050
    ## cMAXTMP      0.044584  0.071202  0.084746  0.099297 0.124932
    ## WDSP        -0.030915  0.003868  0.021709  0.040192 0.075555
    ## RH          -0.266042 -0.164609 -0.110959 -0.056770 0.042552
    ## rho          0.368226  0.400218  0.418521  0.436202 0.470188
    ## sig2eps      0.009420  0.011731  0.013360  0.015376 0.020552
    ## sig2eta      0.653285  0.687885  0.706766  0.726053 0.766003
    ## phi          0.005079  0.005079  0.005079  0.005079 0.005079

``` r
plot(post.ar)
```

![](02_spTimer_files/figure-markdown_github/unnamed-chunk-1-13.png)![](02_spTimer_files/figure-markdown_github/unnamed-chunk-1-14.png)

``` r
plot(post.ar,residuals=TRUE)
```

![](02_spTimer_files/figure-markdown_github/unnamed-chunk-1-15.png)![](02_spTimer_files/figure-markdown_github/unnamed-chunk-1-16.png)

``` r
# Model selection criteria
post.ar$PMCC 
```

    ##         Goodness.of.fit Penalty   PMCC
    ## values:          311.15  451.48 762.63

``` r
# Spatial prediction/interpolation for the AR model
# Define prediction coordinates
set.seed(11)
pred.ar <- predict(post.ar, newdata=DataValPred, newcoords=~Longitude+Latitude)
```

    ## 
    ##  Prediction: AR models 
    ## #
    ## # Tolerance Limit (unit): 2
    ## # Location distances are alright 
    ## #
    ## -------------------------------------------------
    ##   Sampled: 400 of 4000, 10.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 800 of 4000, 20.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 1200 of 4000, 30.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 1600 of 4000, 40.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 2000 of 4000, 50.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 2400 of 4000, 60.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 2800 of 4000, 70.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 3200 of 4000, 80.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 3600 of 4000, 90.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 4000 of 4000, 100.00%
    ## -------------------------------------------------
    ## ## 
    ## # Predicted samples and summary statistics are given.
    ## # nBurn =  1000 . Iterations =  5000 . 
    ## ## 
    ## ##
    ## # Elapsed time: 7.4 Sec.
    ## ##

``` r
print(pred.ar)
```

    ## --------------------------------------
    ## Spatial prediction with Model: AR 
    ## Covariance function: exponential 
    ## Distance method: geodetic:km 
    ## Computation time:  7.4  - Sec. 
    ## --------------------------------------

``` r
names(pred.ar)
```

    ##  [1] "pred.samples"         "pred.coords"          "distance.method"     
    ##  [4] "Distance.matrix.pred" "cov.fnc"              "predN"               
    ##  [7] "Mean"                 "Median"               "SD"                  
    ## [10] "Low"                  "Up"                   "computation.time"    
    ## [13] "model"                "type"

``` r
# validation criteria
spT.validation(DataValPred$o8hrmax,c(pred.ar$Median))  
```

    ##     MSE    RMSE     MAE    MAPE    BIAS   rBIAS   rMSEP 
    ## 47.8695  6.9188  5.3768 12.5958  0.8614  0.0184  0.2891

``` r
####################################################
## Temporal  prediction/forecast for the AR model ##
## 1. In the unobserved locations                 ##
## Press Enter:                                   ##
####################################################
pause()
```

    ## NULL

``` r
# Temporal  prediction/forecast for the AR model
# 1. In the unobserved locations
# Two-step ahead forecast, i.e., in day 61 and 62 
# in the unobserved locations using output from spT.Gibbs
set.seed(11)
fore.ar <- predict(post.ar, newdata=DataValFore, newcoords=~Longitude+Latitude, 
                   type="temporal", foreStep=2, predAR=pred.ar)
```

    ## 
    ##  Forecast: AR models 
    ## -------------------------------------------------
    ##   Sampled: 400 of 4000, 10.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 800 of 4000, 20.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 1200 of 4000, 30.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 1600 of 4000, 40.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 2000 of 4000, 50.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 2400 of 4000, 60.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 2800 of 4000, 70.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 3200 of 4000, 80.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 3600 of 4000, 90.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 4000 of 4000, 100.00%
    ## -------------------------------------------------
    ## ## 
    ## # Forecast samples and summary statistics are given.
    ## # nBurn =  0 . Iterations =  4000 . 
    ## ## 
    ## ##
    ## # Elapsed time: 2.35 Sec.
    ## ##

``` r
print(fore.ar)
```

    ## --------------------------------------
    ## Temporal prediction/forecast with Model: AR 
    ## Covariance function: exponential 
    ## Distance method: geodetic:km 
    ## Computation time:  2.35  - Sec. 
    ## --------------------------------------

``` r
names(fore.ar)
```

    ##  [1] "fore.samples"     "fore.coords"      "distance.method" 
    ##  [4] "cov.fnc"          "obsData"          "fittedData"      
    ##  [7] "residuals"        "Mean"             "Median"          
    ## [10] "SD"               "Low"              "Up"              
    ## [13] "computation.time" "model"            "type"

``` r
# Forecast validations 
spT.validation(DataValFore$o8hrmax,c(fore.ar$Median)) 
```

    ##     MSE    RMSE     MAE    MAPE    BIAS   rBIAS   rMSEP 
    ## 21.2866  4.6137  3.5671 12.0428  0.7176  0.0240  3.0924

``` r
####################################################
## Temporal  prediction/forecast for the AR model ##
## 2. In the observed/fitted locations            ## 
## Press Enter:                                   ##
####################################################
pause()
```

    ## NULL

``` r
# Temporal  prediction/forecast for the AR model
# 2. In the observed/fitted locations
# Two-step ahead forecast, i.e., in day 61 and 62, 
# in the fitted locations using output from spT.Gibbs
set.seed(11)
fore.ar <- predict(post.ar, newdata=DataFitFore, newcoords=~Longitude+Latitude, 
                   type="temporal", foreStep=2)
```

    ## 
    ##  Forecast: AR models 
    ## -------------------------------------------------
    ##   Sampled: 400 of 4000, 10.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 800 of 4000, 20.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 1200 of 4000, 30.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 1600 of 4000, 40.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 2000 of 4000, 50.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 2400 of 4000, 60.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 2800 of 4000, 70.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 3200 of 4000, 80.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 3600 of 4000, 90.00%
    ## -------------------------------------------------
    ## -------------------------------------------------
    ##   Sampled: 4000 of 4000, 100.00%
    ## -------------------------------------------------
    ## ## 
    ## # Forecast samples and summary statistics are given.
    ## # nBurn =  0 . Iterations =  4000 . 
    ## ## 
    ## ##
    ## # Elapsed time: 3.93 Sec.
    ## ##

``` r
print(fore.ar)
```

    ## --------------------------------------
    ## Temporal prediction/forecast with Model: AR 
    ## Covariance function: exponential 
    ## Distance method: geodetic:km 
    ## Computation time:  3.93  - Sec. 
    ## --------------------------------------

``` r
names(fore.ar)
```

    ##  [1] "fore.samples"     "fore.coords"      "distance.method" 
    ##  [4] "cov.fnc"          "obsData"          "fittedData"      
    ##  [7] "residuals"        "Mean"             "Median"          
    ## [10] "SD"               "Low"              "Up"              
    ## [13] "computation.time" "model"            "type"

``` r
# Forecast validations 
spT.validation(DataFitFore$o8hrmax,c(fore.ar$Median)) # 
```

    ##     MSE    RMSE     MAE    MAPE    BIAS   rBIAS   rMSEP 
    ## 46.3071  6.8049  4.9455 17.5391  3.3490  0.1172  1.8638
