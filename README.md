
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ffpros

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/ffpros)](https://CRAN.R-project.org/package=ffpros)

<!-- badges: end -->

Helps scrape FantasyPros.com by handling rate-limiting/caching/parsing
and returning tidy dataframes which can be easily connected to other
data sources.

## Installation

Install the development version from GitHub with:

``` r
 
 # install.packages('remotes')
 
 remotes::install_github("dynastyprocess/ffpros")
```

## Example

All `ffpros` functions start with fp.

The fp\_rankings function retrieves fantasy ranks from fantasypros.com
and right now is only tested with NFL.

``` r
library(ffpros)

fp_rankings(page = "consensus-cheatsheets", sport = "nfl")
#> # A tibble: 459 x 27
#>   player_id player_name         sportsdata_id             player_team_id
#>       <int> <chr>               <chr>                     <chr>         
#> 1     16393 Christian McCaffrey f96db0af-5e25-42d1-a07a-~ CAR           
#> 2     16374 Dalvin Cook         8960d61e-433b-41ea-a7ad-~ MIN           
#> 3     15514 Derrick Henry       87c481c7-7414-43cc-82df-~ TEN           
#> 4     16421 Alvin Kamara        d9c857b2-97da-4fb8-a527-~ NO            
#> 5     17246 Nick Chubb          4bd60b33-9fbf-4156-ba2b-~ CLE           
#> # ... with 454 more rows, and 23 more variables: player_position_id <chr>,
#> #   player_positions <chr>, player_short_name <chr>, player_eligibility <chr>,
#> #   player_yahoo_positions <chr>, player_page_url <chr>, player_filename <chr>,
#> #   player_square_image_url <chr>, player_image_url <chr>,
#> #   player_yahoo_id <chr>, cbs_player_id <chr>, player_bye_week <lgl>,
#> #   player_owned_avg <dbl>, player_owned_espn <dbl>, player_owned_yahoo <int>,
#> #   player_ecr_delta <lgl>, rank_ecr <int>, rank_min <chr>, rank_max <chr>,
#> #   rank_ave <chr>, rank_std <chr>, pos_rank <chr>, tier <int>
```
