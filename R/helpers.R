
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

.fp_string_to_json <- function(string, extractor = "ecrData"){

  js <- V8::v8()
  js$eval(string)

  ecr_data <- js$get(extractor)

  return(ecr_data)
}
