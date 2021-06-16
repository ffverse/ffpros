#' Import latest FantasyPros Rankings
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
#' fp_rankings(page = "dynasty-overall")
#' fp_rankings(page = "consensus-cheatsheets", year = 2017)
#' fp_rankings(page = "overall", sport = "nba")
#' fp_rankings(page = "ros-lw", sport = "nhl")
#' fp_rankings(page = "nl-only-ros-overall", sport = "mlb")
#' }
#'
#' @export

fp_rankings <- function(page,
                        ...,
                        sport = NULL,
                        include_metadata = NULL){

  if(is.null(sport)) sport <- getOption("ffpros.sport")

  stopifnot(sport %in% c("nfl","mlb","nba","nhl"))

  if(is.null(include_metadata)) include_metadata <- getOption("ffpros.include_metadata")

  stopifnot(is.logical(include_metadata))

  url_query <- glue::glue("https://www.fantasypros.com/{sport}/rankings/{page}.php") %>%
    httr::modify_url(query = list(...))

  response <- .fp_get(url_query, sport)

  parsed_rankings <- fp_rankings_parse(response)

  if(!include_metadata) return(parsed_rankings$ecr)

  return(parsed_rankings)
}

#' Dispatch fp_rankings_parse
#'
#' @keywords internal
fp_rankings_parse <- function(response) UseMethod("fp_rankings_parse", response)

#' NFL method for fp_rankings
#' @describeIn fp_rankings_parse
#'
#' @keywords internal
fp_rankings_parse.fp_nfl <- function(response){

  ecr_js <- .fp_extract_ecrjs(response$content)

  if(length(ecr_js)==0) stop("Unable to parse page for JSON ecr data.")

  ecr_data <- .fp_string_to_json(string = ecr_js, extractor = "ecrData")

  players <- ecr_data$players %>%
    tibble::tibble() %>%
    tidyr::unnest_longer(1) %>%
    dplyr::select(
      dplyr::any_of(c(
        "fantasypros_id"="player_id",
        "player_name",
        "pos"="player_position_id",
        "team"="player_team_id",
        "age" = "player_age",
        "rank" = "rank_ecr",
        "ecr" = "rank_ave",
        "sd" = "rank_std",
        "best" = "rank_min",
        "worst" = "rank_max",
        "sportradar_id"="sportsdata_id",
        "yahoo_id" = "player_yahoo_id",
        "cbs_id" = "cbs_player_id"
      )),
      dplyr::everything()
    ) %>%
    dplyr::mutate_at(c("ecr","best","worst","rank","sd"), as.numeric) %>%
    dplyr::mutate_at("fantasypros_id", as.character)

  metadata <- ecr_data[names(ecr_data)!="players"]

  return(list(ecr = players, metadata = metadata, response = response$response))
}

#' NHL method for fp_rankings
#'
#' @describeIn fp_rankings_parse
#'
#' @seealso \url{https://www.fantasypros.com/nhl/rankings/overall.php}
#'
#' @keywords internal
fp_rankings_parse.fp_nhl <- function(response){

  data_table <- response$content %>%
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
    dplyr::select(dplyr::any_of(c(
      "fantasypros_id"="player_id",
      "player_name",
      "team",
      "pos",
      "rank" = "Rank",
      "ecr" = "Avg",
      "sd" = "Std Dev",
      "best" = "Best",
      "worst" = "Worst"
    ))) %>%
    dplyr::mutate_at(c("ecr","best","worst","rank","sd"), as.numeric) %>%
    dplyr::mutate_at("fantasypros_id", as.character)

  return(list(ecr = clean_ecr, response = response$response))
}

#' NBA method for fp_rankings
#'
#' @describeIn fp_rankings_parse
#'
#' @seealso \url{https://www.fantasypros.com/nba/rankings/ros-overall.php}
#'
#' @keywords internal
fp_rankings_parse.fp_nba <- function(response){

  data_table <- response$content %>%
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
    tidyr::extract(col = 1,
                   into = c("team", "pos"),
                   regex = "([A-z]+) - ([A-z,\\,]+)") %>%
    dplyr::filter(!is.na(.data$team), !is.na(.data$pos)) %>%
    dplyr::bind_cols(player_name = player_names,
                     fantasypros_id = player_ids)

  clean_ecr <- data_table %>%
    rvest::html_table() %>%
    purrr::pluck(1) %>%
    dplyr::bind_cols(player_teams) %>%
    dplyr::select(dplyr::any_of(c(
      "player_name",
      "fantasypros_id",
      "team",
      "pos",
      "rank" = "Rank",
      "ecr" = "Avg",
      "sd" = "Std Dev",
      "best" = "Best",
      "worst" = "Worst"
    )))%>%
    dplyr::mutate_at(c("ecr","best","worst","rank","sd"), as.numeric) %>%
    dplyr::mutate_at("fantasypros_id", as.character)

  return(list(ecr = clean_ecr, response = response$response))
}

#' MLB method for fp_rankings
#'
#' @describeIn fp_rankings_parse
#'
#' @seealso \url{https://www.fantasypros.com/nba/rankings/ros-overall.php}
#' @keywords internal
fp_rankings_parse.fp_mlb <- function(response){

  data_table <- response$content %>%
    rvest::html_nodes("#data")

  df_name_nodes <- data_table %>%
    rvest::html_nodes(".player-label") %>%
    tail(-1)

  player_link <- df_name_nodes %>%
    rvest::html_nodes(".fp-player-link")

  player_names <- player_link %>%
    rvest::html_attr("fp-player-name")

  player_ids <- player_link %>%
    rvest::html_attr("class") %>%
    stringr::str_extract_all("[0-9]+$") %>%
    unlist()

  table_rows <- data_table %>%
    rvest::html_nodes("tr") %>%
    tail(-1)

  player_teams <- table_rows %>%
    rvest::html_attr("data-team")

  player_league <- table_rows %>%
    rvest::html_attr("data-league")

  player_pos <- table_rows %>%
    rvest::html_attr("data-pos")

  ecr <- data_table %>%
    rvest::html_table(header = TRUE,trim = TRUE) %>%
    purrr::pluck(1) %>%
    dplyr::bind_cols(
      player_name = player_names,
      fantasypros_id = player_ids,
      pos = player_pos,
      team = player_teams,
      league = player_league
    ) %>%
    dplyr::select(dplyr::any_of(c(
      "fantasypros_id",
      "player_name",
      "pos",
      "team",
      "league",
      "rank" = "Rank",
      "ecr" = "Avg",
      "sd" = "Std Dev",
      "best" = "Best",
      "worst" = "Worst",
      "adp" = "ADP"
    ))) %>%
    dplyr::mutate_at(c("ecr","best","worst","rank","sd","adp"), as.numeric) %>%
    dplyr::mutate_at("fantasypros_id", as.character)

  return(list(ecr = ecr, response = response$response))
}
