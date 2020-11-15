writing\_functions
================
Kailey Rishovd
11/15/2020

## Do something simple

``` r
x_vec = rnorm(30, mean = 5, sd = 3)
## vector that is normal, 30 values, mean 5, sd 3

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  0.63348530 -0.75490205  1.98814334 -0.63198394  0.85447812 -1.75649100
    ##  [7]  1.66174737 -0.18374996 -1.38195144  0.77236483  1.35045160  2.07177880
    ## [13] -0.53395251  0.95803561 -0.50076705 -0.20248761 -0.41999905 -0.28109745
    ## [19]  0.09856713 -0.69671565 -0.03599033  0.69501595 -0.56011216 -1.59637971
    ## [25] -1.14277327 -0.82993857 -0.15922422  0.58606567 -0.49503271  0.49341496

``` r
## gives you z scores that you can look at and scan for sd 
```

I want a function to compute z-scores

``` r
z_scores = function(x) {
  
  if (!is.numeric(x)){
    stop("Input must be numeric")
  }
  
  if (length(x)  < 3) {
    stop("Input must have at least 3 numbers")
  }
  
  z = (x - mean(x) / sd(x))
  
  return(z)
}

z_scores(x_vec)
```

    ##  [1]  4.7759391  0.4060417  9.0396748  0.7929218  5.4715058 -2.7464222
    ##  [7]  8.0123556  2.2037216 -1.5675729  5.2130572  7.0325638  9.3029143
    ## [13]  1.1014721  5.7974491  1.2059221  2.1447455  1.4601364  1.8973240
    ## [19]  3.0923041  0.5891812  2.6687895  4.9696045  1.0191356 -2.2424779
    ## [25] -0.8147685  0.1698670  2.2809154  4.6266876  1.2239707  4.3350729

Try my fucntion on some other things — these should give errors…

``` r
z_scores(3)
```

    ## Error in z_scores(3): Input must have at least 3 numbers

``` r
## doesnt work because sd will not return 

z_scores("my name is kailey")
```

    ## Error in z_scores("my name is kailey"): Input must be numeric

``` r
## cannot take mean of character 

z_scores(mtcars)
```

    ## Error in z_scores(mtcars): Input must be numeric

``` r
## cant take the mean of a dataset 

z_scores(c(TRUE, TRUE, FALSE, TRUE))
```

    ## Error in z_scores(c(TRUE, TRUE, FALSE, TRUE)): Input must be numeric

``` r
## works becasue it turns logical into a sequence of 0 and 1s (coercion)

## in each of these, the function does something wrong
```

update the function (done in the function step above with adding if
statements)

## Multiple outputs

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Input must be numeric")
  }
  
  if (length(x)  < 3) {
    stop("Input must have at least 3 numbers")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  tibble(
    mean = mean_x, 
    sd = sd_x
  )
}
## tibble makes it a dataframe 
```

Check that the function works

``` r
x_vec = rnorm(100, mean = 3, sd = 4)
## can do it this way or can make a function!! 

mean_and_sd(x_vec)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.25  3.81

## Multiple inputs

``` r
sim_data = 
  tibble(
    x = rnorm(n = 100, mean = 4, sd = 3)
  )

sim_data %>% 
  summarize(
    mean = mean(x), 
    sd = sd(x)
  )
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.19  3.32

I’d like to do this with a function…..

``` r
sim_mean_sd = function(samp_size, mu, sigma) {
  
  sim_data = 
    tibble(
      x = rnorm(n = samp_size, mean = mu, sd = sigma)
    )

sim_data %>% 
  summarize(
    mean = mean(x), 
    sd = sd(x)
  )
}

sim_mean_sd(100, 6, 3)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  6.21  3.14

``` r
## n = 100, mean = 6, sd = 3
## try to use named matching instead of purely position matching
## you can also set default values in the function; which can be overwritten with named matching 
```
