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

    ##  [1]  1.521932294  0.105487534 -0.768526182 -0.275680549 -0.997868978
    ##  [6]  1.676111053  0.917963860  1.535114726  1.499463451 -1.346366791
    ## [11]  0.525215642 -1.211066857  0.976994792 -2.016943561 -0.875597020
    ## [16]  0.812571104 -0.541635928 -0.001446507 -0.911547661  0.763851306
    ## [21] -0.488969248  0.108175867  0.812980305  0.559535087 -0.309040766
    ## [26]  0.405866524 -1.806181918 -0.511381985  0.018567734 -0.177577329

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

    ##  [1]  7.73331154  3.22505652  0.44324897  2.01187619 -0.28670241  8.22403112
    ##  [7]  5.81100310  7.77526854  7.66179780 -1.39589993  4.56096556 -0.96526781
    ## [13]  5.99888653 -3.53020914  0.10246431  5.47555943  1.16539441  2.88470726
    ## [19] -0.01195925  5.32049424  1.33302172  3.23361294  5.47686184  4.67019735
    ## [25]  1.90569742  4.18110163 -2.85939778  1.26168655  2.94840851  2.32411874

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
    ## 1  3.65  3.43

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
    ## 1  3.78  3.11

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
    ## 1  6.09  2.94

``` r
## n = 100, mean = 6, sd = 3
## try to use named matching instead of purely position matching
## you can also set default values in the function; which can be overwritten with named matching 
```

## Let’s review Napoleon Dynamite

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_nodes(".a-text-bold span") %>%
  html_text()

review_stars = 
  dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-rating") %>%
  html_text() %>%
  str_extract("^\\d") %>%
  as.numeric()

review_text = 
  dynamite_html %>%
  html_nodes(".review-text-content span") %>%
  html_text() %>% 
  str_replace_all("\n", "") %>% 
  str_trim()

reviews_page1 = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

What about the next page of reviews…

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=2"

dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_nodes(".a-text-bold span") %>%
  html_text()

review_stars = 
  dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-rating") %>%
  html_text() %>%
  str_extract("^\\d") %>%
  as.numeric()

review_text = 
  dynamite_html %>%
  html_nodes(".review-text-content span") %>%
  html_text() %>% 
  str_replace_all("\n", "") %>% 
  str_trim()

reviews_page2 = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

BUT, we don’t want to have to do this for each page… turn it into a
function

``` r
read_page_reviews = function(url){
  
  html = read_html(url)

  review_titles = 
    dynamite_html %>%
    html_nodes(".a-text-bold span") %>%
    html_text()
  
  review_stars = 
    dynamite_html %>%
    html_nodes("#cm_cr-review_list .review-rating") %>%
    html_text() %>%
    str_extract("^\\d") %>%
    as.numeric()
  
  review_text = 
    dynamite_html %>%
    html_nodes(".review-text-content span") %>%
    html_text() %>% 
    str_replace_all("\n", "") %>% 
    str_trim()
  
  reviews = 
    tibble(
      title = review_titles,
      stars = review_stars,
      text = review_text
    )
  
  reviews
}
```

Try the function

``` r
dynamite_url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=2"

read_page_reviews(dynamite_url)
```

    ## # A tibble: 10 x 3
    ##    title                               stars text                               
    ##    <chr>                               <dbl> <chr>                              
    ##  1 "Boo"                                   1 "We rented this movie because our ~
    ##  2 "Movie is still silly fun....amazo~     1 "We are getting really frustrated ~
    ##  3 "Brilliant and awkwardly funny."        5 "I've watched this movie repeatedl~
    ##  4 "Great purchase price for great mo~     5 "Great movie and real good digital~
    ##  5 "Movie for memories"                    5 "I've been looking for this movie ~
    ##  6 "Love!"                                 5 "Love this movie. Great quality"   
    ##  7 "Hilarious!"                            5 "Such a funny movie, definitely br~
    ##  8 "napoleon dynamite"                     5 "cool movie"                       
    ##  9 "Top 5"                                 5 "Best MOVIE ever! Funny one liners~
    ## 10 "\U0001f44d"                            5 "Exactly as described and came on ~

Let’s read a few pages of reviews.

``` r
dynamite_url_base = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="

dynamite_urls = str_c(dynamite_url_base, 1:5)
## this is a vector 

dynamite_urls[1]
```

    ## [1] "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

``` r
all_reviews = 
  bind_rows(
    read_page_reviews(dynamite_urls[1]),
    read_page_reviews(dynamite_urls[2]),
    read_page_reviews(dynamite_urls[3]),
    read_page_reviews(dynamite_urls[4]),
    read_page_reviews(dynamite_urls[5])
  )
```

## Mean scoping example

``` r
f = function(x) {
  z = x + y
  z
}

x = 1
y = 2

f(x = y)
```

    ## [1] 4

## Functions as arguments

``` r
my_summary = function(x, sum_func) {
  
  sum_func(x)

}

x_vec = rnorm(100, 3, 7)

mean(x_vec)
```

    ## [1] 3.330829

``` r
median(x_vec)
```

    ## [1] 3.148795

``` r
my_summary(x_vec, mean)
```

    ## [1] 3.330829
