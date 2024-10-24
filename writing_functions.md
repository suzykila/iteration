writing_functions
================
ww2745
2024-10-24

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
library(readxl)
set.seed(1)
```

\##writing my first function

as an example, here’s a z-score computation.

``` r
x_vec= rnorm(n=25, mean=10, sd=3.5)

(x_vec - mean(x_vec))/ sd(x_vec)
```

    ##  [1] -0.83687228  0.01576465 -1.05703126  1.50152998  0.16928872 -1.04107494
    ##  [7]  0.33550276  0.59957343  0.42849461 -0.49894708  1.41364561  0.23279252
    ## [13] -0.83138529 -2.50852027  1.00648110 -0.22481531 -0.19456260  0.81587675
    ## [19]  0.68682298  0.44756609  0.78971253  0.64568566 -0.09904161 -2.27133861
    ## [25]  0.47485186

Now i’ll write a function to do this.

``` r
z_scores=function(x){
  
  if (!is.numeric(x)){
    stop("x needs to be numeric")
  }
  
  if (length(x)<5){
    stop("you need at least 5 numbers to compute z scores")
  }
  
  z= (x-mean(x))/sd(x)
  
  return(z)
}

z_scores(x=x_vec)
```

    ##  [1] -0.83687228  0.01576465 -1.05703126  1.50152998  0.16928872 -1.04107494
    ##  [7]  0.33550276  0.59957343  0.42849461 -0.49894708  1.41364561  0.23279252
    ## [13] -0.83138529 -2.50852027  1.00648110 -0.22481531 -0.19456260  0.81587675
    ## [19]  0.68682298  0.44756609  0.78971253  0.64568566 -0.09904161 -2.27133861
    ## [25]  0.47485186

does this always work?

``` r
z_scores(x=3)
```

    ## Error in z_scores(x = 3): you need at least 5 numbers to compute z scores

``` r
z_scores(x=c("My","name","is"))
```

    ## Error in z_scores(x = c("My", "name", "is")): x needs to be numeric

## A new function!

``` r
mean_and_sd = function(x){
  mean_x=mean(x)
  sd_x=sd(x)
  
  out_df=
    tibble(
      mean=mean_x,
      sd=sd_x
    )
  
  return(out_df)
}

mean_and_sd(x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.6  3.33

## Check stuff using a simulation

``` r
sim_df=
  tibble(
    x=rnorm(30,10,5)
  )

sim_df |> 
  summarize(
    mean=mean(x),
    sd=sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.2  3.71

Simulation function to check sample mean and sd.

``` r
sim_mean_sd = function(sample_size=30, true_mean=10, true_sd=2){
  sim_df=
  tibble(
    x=rnorm(sample_size,true_mean,true_sd)
  )

 out_df =
  sim_df |> 
  summarize(
    mean=mean(x),
    sd=sd(x)
  )
  
  return(out_df)
}

sim_mean_sd(sample_size=30, true_mean=4, true_sd =12) ##can rewrite default num
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.03  12.4

``` r
sim_mean_sd()
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.4  1.77

## Revists

``` r
lotr_import = function(cell_range, movie_title){
  movie_df=
    read_excel("data/LotR_Words.xlsx", range=cell_range) |> 
    mutate(movie="movie_title") |> 
    janitor::clean_names() |> 
    pivot_longer(
      female:male,
      names_to ="sex",
      values_to = "words"
    ) |> 
    select(movie,everything())
  
  return(movie_df)
}

lotr_df=
  bind_rows(
    lotr_import("B3:D6","fellowship"),
    lotr_import("F3:H6","two_towers"),
    lotr_import("J3:L6","return_king")
  )
```

## NSDUH

``` r
nsduh_url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm" ##put outside the function because each time the function would download from html

nsduh_html = read_html(nsduh_url)

drug_import=function(name, num_table,html){
  drug_df=
    nsduh_html |>  ##looking outside the function body, add into input
    html_table() |> 
    nth(num_table) |> 
    slice(-1) |> 
    mutate(drug ="name") |> 
    select(-contains("P Value"))
  
  return(drug_df)
}

bind_rows(
  drug_import(html=nsduh_html,"marj",1),
  drug_import(html=nsduh_html,"cocaine",5)
  
)
```

    ## # A tibble: 112 × 12
    ##    State `12+(2013-2014)` `12+(2014-2015)` `12-17(2013-2014)` `12-17(2014-2015)`
    ##    <chr> <chr>            <chr>            <chr>              <chr>             
    ##  1 Tota… 12.90a           13.36            13.28b             12.86             
    ##  2 Nort… 13.88a           14.66            13.98              13.51             
    ##  3 Midw… 12.40b           12.76            12.45              12.33             
    ##  4 South 11.24a           11.64            12.02              11.88             
    ##  5 West  15.27            15.62            15.53a             14.43             
    ##  6 Alab… 9.98             9.60             9.90               9.71              
    ##  7 Alas… 19.60a           21.92            17.30              18.44             
    ##  8 Ariz… 13.69            13.12            15.12              13.45             
    ##  9 Arka… 11.37            11.59            12.79              12.14             
    ## 10 Cali… 14.49            15.25            15.03              14.11             
    ## # ℹ 102 more rows
    ## # ℹ 7 more variables: `18-25(2013-2014)` <chr>, `18-25(2014-2015)` <chr>,
    ## #   `26+(2013-2014)` <chr>, `26+(2014-2015)` <chr>, `18+(2013-2014)` <chr>,
    ## #   `18+(2014-2015)` <chr>, drug <chr>

do this instead: dont need to see this function in r file, use for many
rmd source
