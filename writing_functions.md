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

    ##  [1] -1.47736535 -1.14244527  0.97405838 -2.35176264 -0.83786881 -0.45021565
    ##  [7]  1.31316193  0.14701429  1.03941400  1.60648228  0.05336157 -0.24357279
    ## [13] -0.17080767 -0.21386548 -1.13634547  0.54722910 -0.27581672  1.90881723
    ## [19] -1.26005316  0.23780086 -0.06750161 -1.63397862  0.88632107  0.44398982
    ## [25]  0.41579802 -0.02459075  0.88923872  0.64983528  0.62079575 -0.44712832

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

    ##  [1] -1.3989667 -0.3667038  6.1566079 -4.0939614  0.5720364  1.7668288
    ##  [7]  7.2017647  3.6075613  6.3580416  8.1058123  3.3189127  2.4037263
    ## [13]  2.6279969  2.4952877 -0.3479035  4.8410702  2.3043467  9.0376440
    ## [19] -0.7291851  3.8873761  2.9463982 -1.8816671  5.8861913  4.5228746
    ## [25]  4.4359842  3.0786545  5.8951838  5.1573144  5.0678112  1.7763443

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
