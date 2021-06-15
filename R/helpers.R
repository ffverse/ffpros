
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

.fp_clean_ecr <- function(ecr_data){


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




