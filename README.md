
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
[![Live test
status](https://img.shields.io/github/workflow/status/dynastyprocess/ffpros/TestLiveSite?label=Live%20check&style=flat-square&logo=github)](https://github.com/DynastyProcess/ffpros/actions)
[![Codecov test
coverage](https://img.shields.io/codecov/c/github/dynastyprocess/ffpros?label=codecov&style=flat-square&logo=codecov)](https://codecov.io/gh/DynastyProcess/ffpros?branch=main)
[![nflverse
discord](htthttps://img.shields.io/discord/789805604076126219?color=7289da&label=nflverse%20discord&logo=discord&logoColor=fff&style=flat-square)](https://discord.com/invite/5Er2FBnnQa)

<!-- badges: end -->

Helps scrape FantasyPros.com by handling rate-limiting/caching/parsing
and returning tidy dataframes which can be easily connected to other
data sources.

*Renamed from fpscrapr to ffpros on 2021-06-14 so that all ffverse
packages start with ff*

## Installation

Install the stable version of this package from the [ffverse r-universe
repository](https://ffverse.r-universe.dev):

``` r
install.packages("ffpros", repos = "https://ffverse.r-universe.dev")
```

Install the development version with either [DynastyProcess’s
r-universe](https://dynastyprocess.r-universe.dev) or remotes + GitHub:

``` r
# ffverse's r-universe
install.packages("ffpros", repos = "https://dynastyprocess.r-universe.dev")

# or via GitHub c/o remotes/devtools: # install.packages('remotes')
remotes::install_github("dynastyprocess/ffpros")
```

## Usage

All `ffpros` functions start with fp\_ for ease of autocomplete.

The fp\_rankings function retrieves fantasy ranks from FantasyPros.com.

``` r
library(ffpros)

fp_rankings(page = "consensus-cheatsheets") # defaults to nfl!
#> # A tibble: 495 x 27
#>   fantasypros_id player_name         pos   team   rank   ecr    sd  best worst
#>            <int> <chr>               <chr> <chr> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1          16393 Christian McCaffrey RB    CAR       1  1.2   0.72     1     6
#> 2          16374 Dalvin Cook         RB    MIN       2  2.2   0.57     1     4
#> 3          15514 Derrick Henry       RB    TEN       3  2.99  0.63     1     7
#> 4          17240 Saquon Barkley      RB    NYG       4  5.35  2.24     2    22
#> 5          17246 Nick Chubb          RB    CLE       5  6.35  2.33     3    20
#> # ... with 490 more rows, and 18 more variables: sportradar_id <chr>,
#> #   yahoo_id <chr>, cbs_id <chr>, player_positions <chr>,
#> #   player_short_name <chr>, player_eligibility <chr>,
#> #   player_yahoo_positions <chr>, player_page_url <chr>, player_filename <chr>,
#> #   player_square_image_url <chr>, player_image_url <chr>,
#> #   player_bye_week <chr>, player_owned_avg <dbl>, player_owned_espn <dbl>,
#> #   player_owned_yahoo <int>, player_ecr_delta <int>, pos_rank <chr>,
#> #   tier <int>

fp_rankings(page = "ros-overall", sport = "nhl")
#> # A tibble: 420 x 9
#>   fantasypros_id player_name     team  pos    rank   ecr    sd  best worst
#>   <chr>          <chr>           <chr> <chr> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 16664          Connor McDavid  EDM   C,F       1   1.5   0.5     1     2
#> 2 17201          Auston Matthews TOR   C,F       2   2.5   1.5     1     4
#> 3 16669          Mitchell Marner TOR   RW,F      3   8.5   5.5     3    14
#> 4 16588          Leon Draisaitl  EDM   C,LW      4  10     7       3    17
#> 5 15373          Patrick Kane    CHI   RW,F      5  12     1      11    13
#> # ... with 415 more rows
```

Default configuration options are detailed in the [configuration options
vignette](https://ffpros.dynastyprocess.com/articles).

## Support

The best places to get help on this package are:

-   the [nflverse discord](https://discord.com/invite/5Er2FBnnQa) (for
    both this package as well as anything R/NFL related)
-   opening [an
    issue](https://github.com/DynastyProcess/ffpros/issues/new/choose)

## Contributing

Many hands make light work! Here are some ways you can contribute to
this project:

-   You can [open an
    issue](https://github.com/DynastyProcess/ffpros/issues/new/choose)
    if you’d like to request specific data or report a bug/error.

-   You can [sponsor this project with
    donations](https://github.com/sponsors/tanho63)!

-   If you’d like to contribute code, please check out [the contribution
    guidelines](https://ffpros.dynastyprocess.com/CONTRIBUTING.html).

## Terms of Use

The R code for this package is released as open source under the [MIT
license](https://ffpros.dynastyprocess.com/LICENSE.html).

The data accessed by this package belongs to FantasyPros.com, and is
governed by their terms of use.
