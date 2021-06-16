
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
discord](https://img.shields.io/discord/591914197219016707.svg?color=5865F2&label=nflverse%20discord&logo=discord&logoColor=5865F2&style=flat-square)](https://discord.com/invite/5Er2FBnnQa)

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
#> No encoding supplied: defaulting to UTF-8.
#> # A tibble: 495 x 27
#>   fantasypros_id sportradar_id             player_name         pos   team   rank
#>            <int> <chr>                     <chr>               <chr> <chr> <int>
#> 1          16393 f96db0af-5e25-42d1-a07a-~ Christian McCaffrey RB    CAR       1
#> 2          16374 8960d61e-433b-41ea-a7ad-~ Dalvin Cook         RB    MIN       2
#> 3          15514 87c481c7-7414-43cc-82df-~ Derrick Henry       RB    TEN       3
#> 4          17240 9811b753-347c-467a-b3cb-~ Saquon Barkley      RB    NYG       4
#> 5          17246 4bd60b33-9fbf-4156-ba2b-~ Nick Chubb          RB    CLE       5
#> # ... with 490 more rows, and 21 more variables: ecr <chr>, sd <chr>,
#> #   best <chr>, worst <chr>, yahoo_id <chr>, cbs_id <chr>,
#> #   player_positions <chr>, player_short_name <chr>, player_eligibility <chr>,
#> #   player_yahoo_positions <chr>, player_page_url <chr>, player_filename <chr>,
#> #   player_square_image_url <chr>, player_image_url <chr>,
#> #   player_bye_week <chr>, player_owned_avg <dbl>, player_owned_espn <dbl>,
#> #   player_owned_yahoo <int>, player_ecr_delta <int>, pos_rank <chr>,
#> #   tier <int>

fp_rankings(page = "ros-overall", sport = "nhl")
#> No encoding supplied: defaulting to UTF-8.
#> # A tibble: 420 x 8
#>   player_name     player_id team  pos     ecr    sd  best worst
#>   <chr>           <chr>     <chr> <chr> <dbl> <dbl> <int> <int>
#> 1 Connor McDavid  16664     EDM   C,F     1.5   0.5     1     2
#> 2 Auston Matthews 17201     TOR   C,F     2.5   1.5     1     4
#> 3 Mitchell Marner 16669     TOR   RW,F    8.5   5.5     3    14
#> 4 Leon Draisaitl  16588     EDM   C,LW   10     7       3    17
#> 5 Patrick Kane    15373     CHI   RW,F   12     1      11    13
#> # ... with 415 more rows
```

## Default Options

ffpros uses the following options to help configure default arguments:

``` r
options(ffpros.cache = "memory") # one of "memory", "filesystem", or "off", must be set *prior* to loading the package
options(ffpros.sport = "nfl") # one of "nfl", "nba", "nhl", "mlb"
options(ffpros.include_metadata = FALSE) # TRUE to return a list including associated metadata
```

There are also helper functions prefixed with `fp_set_` that help
administer these options.

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
