#' FP GET
#'
#' @param url_query fully formed URL
#' @param sport sport
#'
#' @keywords internal
.fp_get <- function(url_query, sport){

  fn_get <- get("get", envir = .ffpros_env)

  user_agent <- getOption("ffpros.user_agent")

  response <- fn_get(url_query, httr::user_agent(user_agent))

  if (httr::http_error(response)) {
    warning(glue::glue("FantasyPros.com request failed with error: <{httr::status_code(response)}> \n
                    while calling <{url_query}>"), call. = FALSE)
  }

  if(response$url != url_query) stop(glue::glue("Could not find {url_query} - please recheck page name and arguments!"),
                                     call. = FALSE)

  structure(
    list(
      content = httr::content(response),
      query = url_query,
      response = response
    ),
    class = c(paste0("fp_",sport), "fp_response")
  )

}

## PRINT METHOD FP RESPONSE ##
#' @noRd
#' @export
print.fp_response <- function(x, ...){

  cat("FP query <", httr::http_status(x$response)$message,">\n", sep = "")
  cat("URL: <",x$query,">\n", sep = "")

  str(x$content, max.level = 1)

  invisible(x)
}
