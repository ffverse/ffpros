#### FantasyPros Rankings ####

# https://www.fantasypros.com/nfl/rankings/flex.php?week=3&year=2017
# page <- "flex"
# week <- 3
# year <- 2017
# x <- fp_rankings(page, week = week, year = year)

#' Import latest FantasyPros Rankings
#'
#' Scrapes FantasyPros ranks from the specified page and returns metadata
#'
#' @param page which page
#' @param ... named arguments to pass as query parameters
#' @param sport which sport, defaults to NFL
#' @param .return one of c("table","all"), defaults to table
#'
#' @seealso \url{https://github.com/DynastyProcess/data/}
#' @seealso \url{https://www.fantasypros.com}
#'
#' @examples
#' \donttest{
#' fp_rankings(page = "dynasty-overall", sport = "nfl", return = "table")
#' }
#'
#' @return rankings data from FantasyPros
#'
#' @export
fp_rankings <- function(page,
                        ...,
                        sport = c("nfl","nba","nhl","mlb"),
                        .return = c("table","metadata","all")){

  .return <- match.arg(.return)
  sport <- match.arg(sport)

  base_url <- glue::glue("https://www.fantasypros.com/{sport}/rankings/{page}.php")

  query <- httr::modify_url(base_url,
                            query = list(...)
                            )

  response <- httr::GET(query)

  if(response$url != query) stop(glue::glue("Could not find {query} - please recheck page name"))

  fp_html <- httr::content(response)

  ecr_js <- .fp_extract_ecrjs(fp_html)

  if(length(ecr_js)>0) {
    ecr_data <- .fp_string_to_json(ecr_js)

    clean_ecr <- .fp_clean_ecr(ecr_data, .return)
  }

  if(length(ecr_js)==0){
    clean_ecr <- .fp_rvest_ecr(fp_html)
  }

  return(clean_ecr)
}
