#' Set user agent
#'
#' Configures the user agent for the ffpros scraping function.
#'
#' @param user_agent A character string to use as the user agent
#'
#' @export
fp_set_useragent <- function(user_agent) {

  stopifnot(length(user_agent)==1, is.character(user_agent))

  user_agent <- httr::user_agent(user_agent)
  assign("user_agent", user_agent, envir = .ffpros_env)

  invisible(user_agent)
}

#' Set rate limit
#'
#' A helper function that adjusts rate limits for ffpros.
#'
#' @param rate_number number of calls per `rate_seconds`
#' @param rate_seconds number of seconds
#' @param rate_limit a logical to turn rate limiting on if TRUE and off if FALSE
#'
#' @export
fp_set_ratelimit <- function(rate_number = 1, rate_seconds = 5, rate_limit = TRUE) {

  stopifnot(length(rate_number)==1,
            is.numeric(rate_number),
            length(rate_seconds) == 1,
            is.numeric(rate_seconds),
            length(rate_limit) == 1,
            is.logical(rate_limit)
  )


  if (rate_limit) fn_get <- ratelimitr::limit_rate(.retry_get, ratelimitr::rate(rate_number, rate_seconds))

  if (!rate_limit) fn_get <- .retry_get

  assign("get", fn_get, envir = .ffpros_env)

  invisible(fn_get)
}

#' Set default sport
#'
#' A helper function that sets the default sport for the ffpros functions.
#' Can be overridden directly.
#'
#' @param sport one of c("nfl","nba","nhl","mlb")
#'
#' @export
fp_set_sport <- function(sport) {

  stopifnot(
    length(sport)==1,
    is.character(sport),
    sport %in% c("nfl", "nba", "nhl", "mlb")
  )

  options(ffpros.sport = sport)

  invisible(sport)
}

#' Set whether metadata is returned
#'
#' This option allows you to configure whether the output of functions includes metadata,
#' or is simplified to only the dataframe. Defaults to simplifying (i.e. include = FALSE)
#'
#' @param include logical
#'
#' @export
fp_set_metadata <- function(include = FALSE){

  stopifnot(length(include) == 1,
            is.logical(include))

  options(ffpros.include_metadata = include)

  invisible(include)
}
