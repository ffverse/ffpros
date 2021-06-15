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
#' fp_rankings(page = "dynasty-overall", sport = "nfl")
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
    httr::modify_url(query = ...)

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
fp_rankings_parse.fp_nfl <- function(fp_html){

  ecr_js <- .fp_extract_ecrjs(fp_html$content)

  if(length(ecr_js)>0) {

    ecr_data <- .fp_string_to_json(string = ecr_js, extractor = "ecrData")

    players <- ecr_data$players %>%
      tibble::tibble() %>%
      tidyr::unnest_longer(1) %>%
      dplyr::select(
        dplyr::any_of(c(
          "fantasypros_id"="player_id",
          "sportradar_id"="sportsdata_id",
          "player_name",
          "pos"="player_position_id",
          "team"="player_team_id",
          "age" = "player_age",
          "rank" = "rank_ecr",
          "ecr" = "rank_ave",
          "sd" = "rank_std",
          "min" = "rank_min",
          "max" = "rank_max",
          "yahoo_id" = "player_yahoo_id",
          "cbs_id" = "cbs_player_id"
        )),
        dplyr::everything()
      )

    metadata <- ecr_data[names(ecr_data)!="players"]

    return(list(ecr = players, metadata = metadata))
  }

  warning("Unable to parse page for JSON ecr data, falling back to html_table.")

}

#' NHL method for fp_rankings
#'
#' @describeIn fp_rankings_parse
#'
#' @keywords internal
fp_rankings_parse.fp_nhl <- function(response){

}

#' NBA method for fp_rankings
#'
#' @describeIn fp_rankings_parse
#'
#' @keywords internal
fp_rankings_parse.fp_nba <- function(response){}

#' MLB method for fp_rankings
#'
#' @describeIn fp_rankings_parse
#'
#' @keywords internal
fp_rankings_parse.fp_mlb <- function(response){}
