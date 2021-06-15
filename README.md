
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ffpros <a href='#'><img src="man/figures/logo.png" align="right" width="25%" min-width="120px"/></a>

<!-- badges: start -->
<!-- [![CRAN status](https://img.shields.io/cran/v/ffpros?style=flat-square&logo=R&label=CRAN)](https://CRAN.R-project.org/package=ffpros)  -->

[![Dev
status](https://img.shields.io/github/r-package/v/dynastyprocess/ffpros/main?label=dev%20version&style=flat-square&logo=github)](https://ffpros.dynastyprocess.com/)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg?style=flat-square)](https://lifecycle.r-lib.org/articles/stages.html)
[![R build
status](https://img.shields.io/github/workflow/status/dynastyprocess/ffpros/R-CMD-check?label=R%20check&style=flat-square&logo=github)](https://github.com/DynastyProcess/ffpros/actions)
[![Codecov test
coverage](https://img.shields.io/codecov/c/github/dynastyprocess/ffpros?label=codecov&style=flat-square&logo=codecov)](https://codecov.io/gh/DynastyProcess/ffpros?branch=main)
[![nflverse
discord](https://img.shields.io/discord/591914197219016707.svg?color=5865F2&label=nflverse%20discord&logo=discord&logoColor=5865F2&style=flat-square)](https://discord.com/invite/5Er2FBnnQa)

<!-- badges: end -->

Helps scrape FantasyPros.com by handling rate-limiting/caching/parsing
and returning tidy dataframes which can be easily connected to other
data sources.

*Renamed from fpscrapr to ffpros on 2021-06-14 so that all ffverse
packages start with ff*

## Installation

Install the development version from GitHub with:

``` r
 # install.packages('remotes')
 remotes::install_github("dynastyprocess/ffpros")
```

## Example

All `ffpros` functions start with fp\_.

The fp\_rankings function retrieves fantasy ranks from fantasypros.com
and right now is only tested with NFL.

``` r
library(ffpros)

fp_rankings(page = "consensus-cheatsheets") # defaults to nfl!
#> # A tibble: 494 x 27
#>   fantasypros_id sportradar_id             player_name         pos   team   rank
#>            <int> <chr>                     <chr>               <chr> <chr> <int>
#> 1          16393 f96db0af-5e25-42d1-a07a-~ Christian McCaffrey RB    CAR       1
#> 2          16374 8960d61e-433b-41ea-a7ad-~ Dalvin Cook         RB    MIN       2
#> 3          15514 87c481c7-7414-43cc-82df-~ Derrick Henry       RB    TEN       3
#> 4          17240 9811b753-347c-467a-b3cb-~ Saquon Barkley      RB    NYG       4
#> 5          17246 4bd60b33-9fbf-4156-ba2b-~ Nick Chubb          RB    CLE       5
#> # ... with 489 more rows, and 21 more variables: ecr <chr>, sd <chr>,
#> #   min <chr>, max <chr>, yahoo_id <chr>, cbs_id <chr>, player_positions <chr>,
#> #   player_short_name <chr>, player_eligibility <chr>,
#> #   player_yahoo_positions <chr>, player_page_url <chr>, player_filename <chr>,
#> #   player_square_image_url <chr>, player_image_url <chr>,
#> #   player_bye_week <chr>, player_owned_avg <dbl>, player_owned_espn <dbl>,
#> #   player_owned_yahoo <int>, player_ecr_delta <int>, pos_rank <chr>,
#> #   tier <int>

# fp_rankings(page = "ros-overall", sport = "nhl")
```

## Options (and defaults)

``` r
options(ffpros.cache = "memory") # one of "memory", "filesystem", or "off"
options(ffpros.sport = "nfl") # one of "nfl", "nba", "nhl", "mlb"
options(ffpros.include_metadata = FALSE) # TRUE to return a list including associated metadata
```
