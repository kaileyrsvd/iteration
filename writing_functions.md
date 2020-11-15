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

    ##  [1] -0.40688717 -1.07003118  1.62639872  0.68645719 -1.28224493  1.34125225
    ##  [7]  0.79959731 -0.02804287  1.30640389  0.30671582 -1.89283543 -0.84862574
    ## [13]  0.78837850  1.13499227  0.21547039 -1.52351441  0.48689493  0.84157081
    ## [19]  0.66361087  0.24639885 -0.71804193 -1.86862298  0.70079443  0.33349509
    ## [25] -0.36673812  0.26863679 -1.39090183  0.93414194 -0.96915090 -0.31557253

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

    ##  [1]  2.3103086  0.3540590  8.3084243  5.5356327 -0.2719634  7.4672531
    ##  [7]  5.8693917  3.4278848  7.3644518  4.4154101 -2.0731821  1.0071967
    ## [13]  5.8362967  6.8587940  4.1462396 -0.9836992  4.9469316  5.9932119
    ## [19]  5.4682369  4.2374774  1.3924139 -2.0017563  5.5779270  4.4944080
    ## [25]  2.4287467  4.3030784 -0.5924971  6.2662932  0.6516520  2.5796833

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
    ## 1  2.98  3.98

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
    ## 1  4.23  2.78

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
    ## 1  6.03  3.14

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
