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

.fp_rvest_ecr <- function(fp_html){

  data_table <- fp_html %>%
    rvest::html_nodes("#data")

  df_name_nodes <- data_table %>%
    rvest::html_nodes(".player-label") %>%
    tail(-1)

  player_names <- df_name_nodes %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("fp-player-name") %>%
    purrr::discard(is.na)

  player_ids <- df_name_nodes %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("class") %>%
    purrr::keep(~stringr::str_detect(.x,"fp\\-id")) %>%
    stringr::str_extract_all("[0-9]+") %>%
    unlist()

  player_teams <- df_name_nodes %>%
    rvest::html_nodes("small") %>%
    rvest::html_text() %>%
    tibble::tibble() %>%
    tidyr::extract(1,
                   into = c("team", "pos"),
                   regex = "([A-z]+) - ([A-z,\\,]+)") %>%
    dplyr::bind_cols(
      player_name = player_names,
      player_id = player_ids)

  clean_ecr <- data_table %>%
    rvest::html_table() %>%
    purrr::pluck(1) %>%
    dplyr::bind_cols(player_teams) %>%
    dplyr::select(
      "player_name",
      "player_id",
      "team",
      "pos",
      "avg" = "Avg",
      "sd" = "Std Dev",
      "best" = "Best",
      "worst" = "Worst"
    )

  return(clean_ecr)
}




