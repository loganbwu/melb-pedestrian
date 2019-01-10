Exploratory Data Analysis
================
Logan Wu
7 January 2019

Load data
---------

``` r
# read in a manageable portion of the dataset for now
raw.ts <- fread("data/Pedestrian_volume__updated_monthly_.csv", nrows=25000)
raw.ts[,Date_Time := as.POSIXct(Date_Time, format="%m/%d/%Y %I:%M:%S %p")]

raw.geo <- read.csv("data/Pedestrian_sensor_locations.csv")
```

Process TS data
---------------

``` r
data.ts = raw.ts %>%
  select(-ID)

ggplot(data.ts %>% filter(Sensor_ID %in% seq(6)), aes(x=Date_Time, y=Hourly_Counts, color=Sensor_ID)) +
  geom_line(alpha=0.5) +
  facet_grid(Sensor_Name~.) +
  guides(color=F)
```

![](00_EDA_files/figure-markdown_github/unnamed-chunk-2-1.png)

Data is incomplete for some sensors. Possible reasons include maintenance.

``` r
ggplot(data.ts, aes(x=Hourly_Counts)) +
  geom_density(alpha=0.5, fill="red", color=NA)
```

![](00_EDA_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
ggplot(data.ts, aes(x=log(Hourly_Counts+1))) +
  geom_density(alpha=0.5, fill="blue", color=NA)
```

![](00_EDA_files/figure-markdown_github/unnamed-chunk-3-2.png)

``` r
data.ts2 <- data.ts %>%
  select(Date_Time, Sensor_Name, Hourly_Counts) %>%
  spread(key=Sensor_Name, value=Hourly_Counts) %>%
  select(-Date_Time) %>%
  as.ts %>%
  na.contiguous # analysis does not include missing data
  # stcenter# %>%

ggcorrplot(cor(data.ts2[,1:16]), show.legend=F, type="lower", lab=T, lab_size=1.5)
```

![](00_EDA_files/figure-markdown_github/unnamed-chunk-4-1.png)

Spatial correlations have varying levels -- factors other than time of day/year lead to correlation.

Experiment with models
----------------------

Fit a simple time series without any cross-correlation

``` r
# arima.result = auto.arima(data.ts2[,1])
arima.result = arima(data.ts2[,1], order=c(1,1,1),
                     seasonal=list(order=c(0,1,1), period=24))
# result <- lapply(data.ts2, function(x) forecast(auto.arima(x), h=120))
# plot(result[[1]])
plot(forecast(arima.result, h=120))
```

![](00_EDA_files/figure-markdown_github/unnamed-chunk-6-1.png)

Packages for spatio-temporal regression
=======================================

lagsarlm (Spatial Simultaneous Autoregressive Lag Model Estimation)
-------------------------------------------------------------------

lagsarlm is for spatial effects and does not help with time series.

SpatioTemporal
--------------

Tutorial script is in 02\_spTimer.Rmd

Multiple ARIMA
--------------

MARIMA requires connectivity between observation nodes to be specified. The data contains 58 sensors, but only 18 geographic locations are available with multiple sensors per location. Sensors have to be manually assigned a coordinate.

Seasonal differencing
=====================

The data contains both daily and weekly seasonality. Some R functions, e.g. arima, do not handle multiple seasonalities. We can pre-process to remove the known seasonalities.

``` r
test1 = data.frame(y=data.ts2[,1]) %>%
  mutate(t = row_number(),
         y.diff = y - lag(y, 24),
         y.diff2 = y.diff - lag(y.diff, 168))
test = test1 %>%
  gather(key="key", value="value", -t)

ggplot(test, aes(x=t, y=value, color=key)) +
  geom_line()
```

    ## Don't know how to automatically pick scale for object of type ts. Defaulting to continuous.

    ## Warning: Removed 216 rows containing missing values (geom_path).

![](00_EDA_files/figure-markdown_github/unnamed-chunk-7-1.png)

The final differenced data is mostly stationary. Spikes could be due to public holidays, which will be ignored for now. Intuitively, we also need at least a first differencing (AR1) but this can be done in the ARIMA fit.

``` r
result = auto.arima(test1$y.diff2)
result.forecast = forecast(result, h=200)$mean
y.diff2 = c(test1$y.diff2, result.forecast)
y.diff2[is.na(y.diff2)] = 0

y.diff = as.numeric(test1$y.diff)
y.diff[is.na(y.diff)] = 0
for (i in (24+169):length(y.diff2)) {
  y.diff[i] = y.diff[i-168] + y.diff2[i]
}
y = as.numeric(test1$y)
for (i in 25:length(y.diff)) {
  y[i] = y[i-24] + y.diff[i]
}

seasonal.result = data.frame(y, y.diff, y.diff2) %>%
  mutate(t=row_number(),
         prediction=t>nrow(test1)) %>%
  gather(key="key", value="value", -t, -prediction)
ggplot(seasonal.result, aes(x=t, y=value, color=prediction)) +
  geom_line() +
  facet_grid(key~.)
```

![](00_EDA_files/figure-markdown_github/unnamed-chunk-8-1.png)

auto.arima has fit an ARIMA(5,0,0) model.

One issue is that pedestrian counts should be non-negative whereas ARIMA models are unbounded (although no issues are seen in this example). Perhaps a transformation will help.

``` r
pacf(test1$y, lag.max=200)
```

![](00_EDA_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
pacf(test1$y.diff, lag.max=200, na.action=na.pass)
```

![](00_EDA_files/figure-markdown_github/unnamed-chunk-9-2.png)

``` r
pacf(test1$y.diff2, na.action=na.pass)
```

![](00_EDA_files/figure-markdown_github/unnamed-chunk-9-3.png)

Despite differencing, there remains a large partial autocorrelation with 24 hours. However, an AR(1) model is probably still the most appropriate on the differenced data.

``` r
# experiment on logged series
# offset of 1 to keep log supported
test2 = data.frame(logy=log(data.ts2[,1]+1)) %>%
  mutate(t = row_number(),
         logy.diff = logy - lag(logy, 24),
         logy.diff2 = logy.diff - lag(logy.diff, 168))
test = test1 %>%
  gather(key="key", value="value", -t)

result = auto.arima(test2$logy.diff2)
result.forecast = forecast(result, h=200)$mean
logy.diff2 = c(test2$logy.diff2, result.forecast)
logy.diff2[is.na(logy.diff2)] = 0

logy.diff = as.numeric(test2$logy.diff)
logy.diff[is.na(logy.diff)] = 0
for (i in (24+169):length(logy.diff2)) {
  logy.diff[i] = logy.diff[i-168] + logy.diff2[i]
}
logy = as.numeric(test2$logy)
for (i in 25:length(logy.diff)) {
  logy[i] = logy[i-24] + logy.diff[i]
}

seasonal.result = data.frame(logy, logy.diff, logy.diff2) %>%
  mutate(y=exp(logy)-1,
         t=row_number(),
         prediction=t>nrow(test2)) %>%
  gather(key="key", value="value", -t, -prediction)
ggplot(seasonal.result, aes(x=t, y=value, color=prediction)) +
  geom_line() +
  facet_grid(key~., scales="free_y")
```

![](00_EDA_files/figure-markdown_github/unnamed-chunk-10-1.png)

The final series looks correct.

Issue: No confidence intervals. A Bayesian approach may be required.

Ignore below: example from website

``` r
# Create a 5x5 regular grid which will be our lattice
sites <- matrix(0, 25, 2)
for (i in 1:5) {
    for (j in 1:5)
        sites[(i-1)*5 + j, ] <- c(i, j) - .5
}
plot(sites)

# Create a uniform first order neighbourhood
knb <- dnearneigh(sites, 0, 1)
plot(knb, sites)

# Lag the neighbourhood to create other order matrices
knb <- nblag(knb, 4)
klist <- list(order0=diag(25),
           order1=nb2mat(knb[[1]]),
           order2=nb2mat(knb[[2]]),
           order3=nb2mat(knb[[3]]),
           order4=nb2mat(knb[[4]]))
           
# Simulate a STARMA(2;1) process
eps <- matrix(rnorm(200*25), 200, 25)
star <- eps
for (t in 3:200) {
    star[t,] <- (.4*klist[[1]] + .25*klist[[2]]) %*% star[t-1,] + 
      (.25*klist[[1]]) %*% star[t-2,] + (-.3*klist[[2]]) %*% eps[t-1,] + 
      eps[t, ]
}

star <- star[101:200,]  # Remove first observations
star <- stcenter(star)  # Center and scale the dataset
                                           
# Identify the process
stacf(star, klist)
stpacf(star, klist)

# Estimate the process
ar <- matrix(c(1, 1, 1, 0), 2, 2)
ma <- matrix(c(0, 1), 1, 2)
model <- starma(star, klist, ar, ma)
model
summary(model)

# Diagnose the process
stcor.test(model$residuals, klist, fitdf=4)
stacf(model$residuals, klist)
stpacf(model$residuals, klist)

# starma(matrix.ts, c(diag(18)), ar=0, ma=0)
```

Process geographic data
-----------------------

``` r
data.geo = raw.geo %>%
  mutate(Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude)) %>%
  # st_as_sf(coords=c("Latitude", "Longitude")) %>%
  select(-Status, -Upload.Date, -Geometry, -Location.Type)

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(data.geo$Longitude, data.geo$Latitude,
                   label=data.geo$Sensor.Description,
                   weight=0, radius=4, fillOpacity=0.5)
```

    ## PhantomJS not found. You can install it with webshot::install_phantomjs(). If it is installed, please make sure the phantomjs executable can be found via the PATH variable.

<!--html_preserve-->

<script type="application/json" data-for="htmlwidget-0f26196a18aa70690d66">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addProviderTiles","args":["CartoDB.Positron",null,null,{"errorTileUrl":"","noWrap":false,"detectRetina":false}]},{"method":"addCircleMarkers","args":[[-37.8110152342489,-37.8123967869915,-37.8134486106919,-37.8240177591352,-37.8168607425204,-37.8138066724228,-37.811729134669,-37.8187647327542,-37.8073006699971,-37.8031027095484,-37.8124470265268,-37.8099934057793,-37.8201782771032,-37.806068879192,-37.806315806617,-37.8188796214196,-37.8136254240211,-37.8106437830727,-37.7969047300547,-37.8153798501116,-37.8040239999553,-37.8187424894411,-37.8123720170999,-37.8137974851005,-37.8166863320591,-37.8201124166947,-37.8125215691035,-37.8116033115848,-37.8016968046535,-37.8000856548236,-37.8191170445268,-37.8100617233511,-37.8086954214778,-37.817673499533,-37.8157374166874,-37.8212992424763,-37.8148325157834,-37.8190925593958,-37.8172343724998,-37.8141407401773,-37.7980819129784,-37.8183237506589,-37.819658083823,-37.8156418999105,-37.8084181419925,-37.8156498888822,-37.8123477497748,-37.8165252682678,-37.8178644478193,-37.8157342252752,-37.8229354263742,-37.8024071877061,-37.7984452524968,-37.8134944036338,-37.8198299169874,-37.8112184919515,-37.8148798759503,-37.8145798719868],[144.964294851728,144.956526528442,144.973053538633,144.956044263919,144.95358075251,144.965167185971,144.968246601442,144.947105451213,144.959560550793,144.966714517396,144.967787571661,144.972275879457,144.965088773616,144.956446929732,144.958666979097,144.95449198131,144.973235916778,144.964471322694,144.964403781053,144.974150495806,144.963083999809,144.967876562712,144.965506710649,144.969957454141,144.966897334535,144.96291897546,144.961940101874,144.962200774628,144.966589114895,144.963864121768,144.965582558609,144.96142334572,144.960494038903,144.950255946636,144.966857177706,144.968793093492,144.974540556573,144.954527490176,144.967150328493,144.9660937975,144.967210138302,144.971414820121,144.968634533137,144.965499000498,144.959063166657,144.939706947708,144.961533113969,144.961210625343,144.965068228214,144.965210438559,144.947175106953,144.961567307798,144.964117822271,144.965153237914,144.951025557542,144.966568066001,144.966087801437,144.942923981029],4,null,null,{"interactive":true,"className":"","stroke":true,"color":"#03F","weight":0,"opacity":0.5,"fill":true,"fillColor":"#03F","fillOpacity":0.5},null,null,null,null,["Melbourne Central","Flagstaff Station","Collins Place (North)","Melbourne Convention Exhibition Centre","Bourke St - Spencer St (North)","Bourke Street Mall (South)","Chinatown-Lt Bourke St (South)","Victoria Point","QVM-Therry St (South)","Lygon St (East)","Bourke St-Russell St (West)","Lonsdale St-Spring St (West)","Southbank","QV Market-Peel St","QVM-Queen St (East)","Spencer St-Collins St (North)","Collins Place (South)","State Library","Tin Alley-Swanston St (West)","Flinders St-Spark La","Lincoln-Swanston (West)","Princes Bridge","Chinatown-Swanston St (North)","Alfred Place","Flinders La-Swanston St (West)","Sandridge Bridge","Elizabeth St-Lonsdale St (South)","Melbourne Central-Elizabeth St (East)Melbourne Central-Elizabeth St (East)","Lygon St (West)","Grattan St-Swanston St (West)","Flinders Street Station Underpass","Elizabeth St-La Trobe St (East)","QV Market-Elizabeth St (West)","Bourke St Bridge","City Square","The Arts Centre","Flinders St-Spring St (West)","Spencer St-Collins St (South)","Flinders St-Swanston St (West)","Little Collins St-Swanston St (East)","Faraday St-Lygon St (West)","Birrarung Marr","St Kilda Rd-Alexandra Gardens","Collins Street (North)","QVM-Franklin St (North)","Waterfront City","Lonsdale St - Elizabeth St (North)","Queen St (West)","Flinders St-Elizabeth St (East)","Australia on Collins","Webb Bridge","Pelham St (South)","Monash Rd-Swanston St (West)","Bourke Street Mall (North)","Southern Cross Station","Lonsdale St (South)","Town Hall (West)","New Quay"],{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]}],"limits":{"lat":[-37.8240177591352,-37.7969047300547],"lng":[144.939706947708,144.974540556573]}},"evals":[],"jsHooks":[]}</script>
<!--/html_preserve-->
Estimate neighborhoods based on the example
-------------------------------------------

``` r
sites = data.geo %>% select(Latitude, Longitude) %>% as.matrix
knb <- dnearneigh(sites, 0, 0.006)
plot(knb, sites)
```

![](00_EDA_files/figure-markdown_github/unnamed-chunk-13-1.png)

``` r
# Lag the neighbourhood to create other order matrices
knb <- nblag(knb, 3)
klist <- list(order0=diag(58),
           order1=nb2mat(knb[[1]]),
           order2=nb2mat(knb[[2]]),
           order3=nb2mat(knb[[3]]))

# Identify the process
stacf(data.ts2, klist)
stpacf(data.ts2, klist)
```
