## INTRODUCTION

<div align="justify">

The aim of this study is to figure out how much risk is associated with
changes in the value of the GBP/VND currency pair over a period of 5
years using the GARCH VaR approach by Ionas Kelepouris & Dimos
Kelepouris. The time series used in this research consists of GBP/VND
returns over the past 5 years. To break it down, I use the GARCH model
to estimate how much the volatility of these returns varies over time
then make a forecast about potential future losses (VaR) associated with
the GBP/VND currency pair. The final output will be used to create a
scenario for hedging in the FX market.

## DATA EXPLORATORY

<div align="justify">

This research used a dataset of 1250 observations, reflecting the
fluctuations in the GBP to VND exchange rate over a span of 5 years, on
a daily basis.The first 1000 obs will be used in the model and 250 obs
will be used in backtesting progress.

``` r
library(zoo)
library(ggplot2)
library(tseries)
library(FinTS)
library(rugarch)
df <- read.csv("GBP_VND.csv")
df$date <- as.Date(df$date, format = "%d/%m/%Y")
train_percentage <- 0.8
train_rows <- round(nrow(df) * train_percentage)
train_data <- df[1:train_rows, ]
test_data <- df[(train_rows + 1):nrow(df), ]
```

``` r
head(train_data,5)
```

    ##         date   price       return
    ## 1 2017-03-20 28167.0           NA
    ## 2 2017-03-21 28429.0  0.009258671
    ## 3 2017-03-22 28431.0  0.000070300
    ## 4 2017-03-23 28540.5  0.003844032
    ## 5 2017-03-24 28418.5 -0.004283790

``` r
ggplot(train_data, aes(x = date, y = price)) +
  geom_line(color = 'red') +
  labs(x = '', y = 'Price (per GBP)', title = 'GBP/VND')
```

![](MarkdownScript_files/figure-markdown_github/code%203-1.png)

``` r
adf.test(train_data$price)
```

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  train_data$price
    ## Dickey-Fuller = -2.6508, Lag order = 9, p-value = 0.3028
    ## alternative hypothesis: stationary

<div align="justify">

The ADF test result with a p-value of 0.3028, which is higher than the
significance level of 0.05, we cannot reject the null hypothesis. This
suggests that the exchange rate between GBP and VND is likely not
stationary. This conclusion aligns with the graphical analysis of the
price graph.Non-stationary processes have means, variances and
covariances that change over time. Using non-stationary time series data
leads to unreliable forecasting. For the stationarity transformation, we
prefer to calculate the simple daily returns:  
$$ r_t = \frac{{\text{{price}}\_t - \text{{price}}\_{t-1}}}{{\text{{price}}\_{t-1}}} $$

``` r
ggplot(train_data, aes(x = date, y = return)) +
  geom_line(color = 'blue') +
  labs(x = '', y = 'Return', title = 'GBP/VND')
```

![](MarkdownScript_files/figure-markdown_github/code%20-1.png)

``` r
adf.test(na.omit(train_data$return))
```

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  na.omit(train_data$return)
    ## Dickey-Fuller = -12.002, Lag order = 9, p-value = 0.01
    ## alternative hypothesis: stationary

<div align="justify">

The ADF test result with a p-value of 0.01, we can reject the null
hypothesis: The return series does not have a unit root and is
stationary. Hence, it could be used in time series forecasting.

## GARCH Implementation

### Applying ARIMA to find the best fit of a Time Series model

# `{r code 4, message= FALSE, warning = FALSE} # jarque.bera.test(na.omit(df$return)) #`

``` r
ArchTest(train_data$return, lags = 1, demean = T)
```

    ## 
    ##  ARCH LM-test; Null hypothesis: no ARCH effects
    ## 
    ## data:  train_data$return
    ## Chi-squared = 37.931, df = 1, p-value = 7.33e-10

``` r
data_fit_sqrt <- train_data$return^2
plot(data_fit_sqrt, type = "l")
```

![](MarkdownScript_files/figure-markdown_github/code%2010-1.png)

``` r
pacf(na.omit(data_fit_sqrt))
```

![](MarkdownScript_files/figure-markdown_github/code%2010-2.png)

# `{r code 11, message= FALSE, warning = FALSE} # model_spec = ugarchspec(variance.model =  #                           list(model = 'sGARCH',  #                                garchOrder = c(1 , 1)),  #                           mean.model = list(armaOrder = c(0 , 0))) # model_fit = ugarchfit(spec = model_spec ,  #                       data = data_fit ,  #                       solver = 'solnp', #                       out.sample=250) # options(scipen = 999) # model_fit@fit$matcoef #`

# 

# `{r code 12, message= FALSE, warning = FALSE} # model_roll = ugarchroll(spec = model_spec,  #                         data = data_fit,  #                         n.start = 999,  #                         refit.every = 1, #                         refit.window = 'moving') #`

# 

# `{r code 13, message= FALSE, warning = FALSE} # ArchTest(data_fit, lags = 1, demean = T) #`

# `{r code 7, message= FALSE, warning = FALSE} # fitdist(distribution = 'std' , x = data_fit)$pars #`
