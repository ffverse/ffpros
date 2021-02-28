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
#' @param .return one of c("table","metadata","all"), defaults to table
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


fp_rankings <- function(page, ..., sport = "nfl", .return = "table"){

  # browser()

  if(!.return %in% c("table","metadata","all")) {
    stop('.return should be one of "table", "metadata", "all"!')}

  base_url <- glue::glue("https://www.fantasypros.com/{sport}/rankings/{page}.php")

  query <- httr::modify_url(base_url, query = list(...))

  response <- httr::GET(query)

  fp_html <- httr::content(response)

  ecr_js <- .fp_extract_ecrjs(fp_html)

  ecr_data <- .fp_string_to_json(ecr_js)

  clean_ecr <- .fp_clean_ecr(ecr_data, .return)

  return(clean_ecr)
}

#' ECR data script selector
#'
#' @keywords internal

.fp_extract_ecrjs <- function(fp_html, extractor = "ecrData") {

  script_nodes <- fp_html %>%
    rvest::html_nodes("script")

  selectors <- script_nodes %>%
    purrr::map(as.character) %>%
    stringr::str_detect(extractor)

  x <- script_nodes[selectors] %>%
    rvest::html_text()

  return(x)
}

#' Extract JS from ECR
#'
#' @keywords internal




#' Converts FantasyPros string to JSON
#'
#' @keywords internal

.fp_string_to_json <- function(string){

  js <- V8::v8()
  js$eval(string)

  ecr_data <- js$get('ecrData')

  return(ecr_data)
}

.fp_clean_ecr <- function(ecr_data, .return){

  players <- ecr_data$players %>%
    tibble::tibble() %>%
    tidyr::unnest_longer(1)

  metadata <- ecr_data[names(ecr_data)!="players"]

  if(.return == "table") return(players)

  if(.return == "metadata") return(metadata)

  return(list(ecr = players,
              metadata = metadata))
}

