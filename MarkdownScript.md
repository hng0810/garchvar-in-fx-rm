# INTRODUCTION

<div align="justify">

# DATA & LIBRARIES

<div align="justify">


``` r
library(zoo)
library(ggplot2)
library(tseries)
library(FinTS)
library(rugarch)
df <- read.csv("GBP_VND.csv")
df$date <- as.Date(df$date, format = "%d/%m/%Y")
```

``` r
head(df,5)
```

    ##         date   price       return
    ## 1 2017-03-20 28167.0           NA
    ## 2 2017-03-21 28429.0  0.009258671
    ## 3 2017-03-22 28431.0  0.000070300
    ## 4 2017-03-23 28540.5  0.003844032
    ## 5 2017-03-24 28418.5 -0.004283790

# DATA VISUALIZATION

<div align="justify">


``` r
ggplot(df, aes(x = date, y = price)) +
  geom_line(color = 'blue') +
  labs(x = '', y = 'Price (per GBP)', title = 'GBP/VND')
```

![](MarkdownScript_files/figure-markdown_github/code%203-1.png)

``` r
ggplot(df, aes(x = date, y = return)) +
  geom_line(color = 'blue') +
  labs(x = '', y = 'Return', title = 'GBP/VND')
```

![](MarkdownScript_files/figure-markdown_github/code%203-2.png)

``` r
jarque.bera.test(na.omit(df$return))
```

    ## 
    ##  Jarque Bera Test
    ## 
    ## data:  na.omit(df$return)
    ## X-squared = 500.67, df = 2, p-value < 2.2e-16

``` r
summary(df$return)
```

    ##       Min.    1st Qu.     Median       Mean    3rd Qu.       Max.       NA's 
    ## -0.0351436 -0.0032667  0.0000996  0.0000736  0.0031763  0.0276862          1

``` r
data_fit <- na.omit(df$return[1:1000])
data_backtest <- df$return[1000:1250]
```

``` r
fitdist(distribution = 'std' , x = data_fit)$pars
```

    ##           mu        sigma        shape 
    ## 5.641171e-05 5.628474e-03 5.416408e+00

``` r
adf.test(data_fit)
```

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  data_fit
    ## Dickey-Fuller = -12.002, Lag order = 9, p-value = 0.01
    ## alternative hypothesis: stationary

``` r
ArchTest(data_fit, lags = 1, demean = T)
```

    ## 
    ##  ARCH LM-test; Null hypothesis: no ARCH effects
    ## 
    ## data:  data_fit
    ## Chi-squared = 37.931, df = 1, p-value = 7.33e-10

``` r
data_fit_2 <- data_fit^2
plot(data_fit_2, type = "l")
```

![](MarkdownScript_files/figure-markdown_github/code%2010-1.png)

``` r
pacf(data_fit_2)
```

![](MarkdownScript_files/figure-markdown_github/code%2010-2.png)

# `{r code 11, message= FALSE, warning = FALSE} # model_spec = ugarchspec(variance.model =  #                           list(model = 'sGARCH',  #                                garchOrder = c(1 , 1)),  #                           mean.model = list(armaOrder = c(0 , 0))) # model_fit = ugarchfit(spec = model_spec ,  #                       data = data_fit ,  #                       solver = 'solnp', #                       out.sample=250) # options(scipen = 999) # model_fit@fit$matcoef #`

# 

# `{r code 12, message= FALSE, warning = FALSE} # model_roll = ugarchroll(spec = model_spec,  #                         data = data_fit,  #                         n.start = 999,  #                         refit.every = 1, #                         refit.window = 'moving') #`

# 

# `{r code 13, message= FALSE, warning = FALSE} # ArchTest(data_fit, lags = 1, demean = T) #`
