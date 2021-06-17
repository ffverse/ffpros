#' Import FantasyPros Projections
#'
#' Scrapes FantasyPros ranks from the specified page (and optionally, returns metadata).
#'
#' @param page which page
#' @param ... named arguments to pass as query parameters
#' @param sport the sport to look up, defaults to nfl
#' @param include_metadata logical - default FALSE (only return tibble), if TRUE returns list with both tibble and metadata
#'
#' @seealso \url{https://github.com/dynastyprocess/data}
#' @seealso \url{https://www.fantasypros.com}
#'
#' @examples
#' \donttest{
#' fp_projections("qb")
#' fp_projections("wr", year=2020, week=2)
#' }
#'
#' @export
fp_projections <- function(page,
                           ...,
                           sport = NULL,
                           include_metadata = NULL){

  if(is.null(sport)) sport <- getOption("ffpros.sport")

  stopifnot(sport %in% c("nfl","mlb","nba","nhl"))

  if(is.null(include_metadata)) include_metadata <- getOption("ffpros.include_metadata")

  stopifnot(is.logical(include_metadata))

  url_query <- glue::glue("https://www.fantasypros.com/{sport}/projections/{page}.php") %>%
    httr::modify_url(query = list(...))

  response <- .fp_get(url_query, sport)

  parsed_rankings <- fp_projections_parse(response)

  if(!include_metadata) return(parsed_rankings$projections)

  return(parsed_rankings)
}

#' Dispatch fp_projections_parse
#'
#' @keywords internal
fp_projections_parse <- function(response) UseMethod("fp_projections_parse", response)

#' NFL method for fp_rankings
#' @describeIn fp_rankings_parse
#'
#' @keywords internal
fp_projections_parse.fp_nfl <- function(response){

  table_html <- response$content %>%
    rvest::html_node("#data")

  player_labels <- table_html %>%
    rvest::html_nodes(".player-label")

  player_info <- tibble::tibble(
    fantasypros_id =
      player_labels %>%
      rvest::html_nodes(".fp-player-link") %>%
      rvest::html_attr("class") %>%
      stringr::str_extract_all("[0-9]+$") %>%
      unlist(),
    player_name =
      player_labels %>%
      rvest::html_nodes(".fp-player-link") %>%
      rvest::html_attr("fp-player-name"),
    team = rvest::html_text(player_labels) %>% tail(-1)
  ) %>%
    dplyr::mutate(
      team = purrr::map2(.data$player_name,
                         .data$team,
                         ~stringr::str_remove_all(.y,.x) %>%
                           stringr::str_squish()) %>% unlist()
    )

  projections <- rvest::html_table(table_html)

  table_names <- projections %>%
    dplyr::slice(1:2) %>%
    as.matrix() %>%
    apply(2,paste, collapse = "_") %>%
    janitor::make_clean_names()

  table_out <- projections %>%
    tail(-2) %>%
    setNames(table_names) %>%
    dplyr::bind_cols(player_info) %>%
    dplyr::mutate(
      dplyr::across(dplyr::matches("passing_|rushing_|receiving_|misc_"),
                    ~ stringr::str_remove(.x,",") %>% as.numeric())
    ) %>%
    dplyr::select(
      "fantasypros_id",
      "player_name",
      "team",
      dplyr::starts_with("passing_"),
      dplyr::starts_with("rushing_"),
      dplyr::starts_with("receiving_"),
      dplyr::starts_with("misc_")
    )

  return(list(projections = table_out, response = response$response))
}

