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
#' if(interactive()){
#'   fp_projections("qb")
#'   fp_projections("wr", year = 2016, week = 2, scoring = "PPR")
#'   fp_projections("flex", year = 2020, week = 7, scoring = "PPR", `min-yes`="true", `max-yes`="true")
#'
#'   fp_projections("hitters", `min-yes` = "true", `max-yes` = "true", sport = "mlb")
#'   fp_projections("pitchers", sport = "mlb")
#'
#'   fp_projections("overall", sport = "nba")
#' }
#'}
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

  parsed_projections <- fp_projections_parse(response)

  if(!include_metadata) return(parsed_projections$projections)

  return(parsed_projections)
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
      #The 'highlow' string may appear in the 'team' text when max and min requested
      team = stringr::str_remove(team, "highlow$"),
      team = purrr::map2(.data$player_name,
                         .data$team,
                         ~stringr::str_remove_all(.y,.x) %>%
                           stringr::str_extract_all("[A-Z]+(\\s|$)") %>%
                           stringr::str_squish()) %>% unlist()
    )


  projections <- rvest::html_table(table_html)

  # If simpler K and DST table get table names from the parsed table
  if( names(projections)[1]  == "Player" ){

    projections <- janitor::clean_names(projections)
    table_names <- names(projections)

  }else{
    #otherwise process the top two rows to get the table names
    table_names <- projections %>%
      dplyr::slice(1:2) %>%
      as.matrix() %>%
      apply(2,paste, collapse = "_") %>%
      janitor::make_clean_names()

  }



  #### Base Case ####

  if(stringr::str_detect(response$query, "max\\-yes", negate = TRUE) ||
     stringr::str_detect(response$query, "min\\-yes", negate =  TRUE)){

    # If simpler K and DST table keep all rows
    if( names(projections)[1]  == "player" ){

      table_out <- projections %>%
        dplyr::bind_cols(player_info) %>%
        dplyr::select(
          "fantasypros_id",
          "player_name",
          "team",
          dplyr::everything()
        )

    }else{
      # Otherwise drop the first two rows since they are the table names
      table_out <- projections %>%
        tail(-2) %>%
        stats::setNames(table_names) %>%
        dplyr::bind_cols(player_info) %>%
        dplyr::mutate(
          dplyr::across(dplyr::matches("passing_|rushing_|receiving_|misc_"),
                        ~ stringr::str_remove(.x,",") %>% as.numeric())
        ) %>%
        dplyr::select(
          "fantasypros_id",
          "player_name",
          dplyr::starts_with("pos"),
          "team",
          dplyr::starts_with("passing_"),
          dplyr::starts_with("rushing_"),
          dplyr::starts_with("receiving_"),
          dplyr::starts_with("misc_")
        )
    }


    return(list(projections = table_out, response = response$response))
  }

  ## If min and max projections are displayed, do this

  max_cells <- NULL

  if(stringr::str_detect(response$query, "max\\-yes\\=true")) {

    max_names <- paste(table_names,"max",sep="_")

    max_cells <- table_html %>%
      rvest::html_nodes(".max-cell") %>%
      rvest::html_text() %>%
      matrix(nrow = nrow(player_info),
             byrow = TRUE,
             dimnames = list(seq_len(nrow(player_info)),
                             max_names[max_names!="pos_max"])
      ) %>%
      tibble::as_tibble(.name_repair = "minimal") %>%
      dplyr::select(-"player_max")

  }

  min_cells <- NULL

  if(stringr::str_detect(response$query, "min\\-yes\\=true")) {

    min_names <- paste(table_names,"min",sep="_")

    min_cells <- table_html %>%
      rvest::html_nodes(".min-cell") %>%
      rvest::html_text() %>%
      matrix(nrow = nrow(player_info),
             byrow = TRUE,
             dimnames = list(1:nrow(player_info),
                             min_names[min_names!="pos_min"])
      ) %>%
      tibble::as_tibble(.name_repair = "minimal") %>%
      dplyr::select(-"player_min")

  }

  mean_names <- paste(table_names,"mean",sep="_")

  mean_cells <- table_html %>%
    rvest::html_nodes(".center") %>%
    xml2::as_list() %>%
    purrr::map_chr(purrr::pluck(1)) %>%
    matrix(nrow = nrow(player_info),
           byrow = TRUE,
           dimnames = list(1:nrow(player_info),
                           mean_names[!mean_names %in% c("player_mean", "pos_mean")])
    ) %>%
    tibble::as_tibble(.name_repair = "minimal")

  if(stringr::str_detect(response$query, "flex")) {

    player_info$pos <- table_html %>%
      rvest::html_nodes("tr td:nth-child(2)") %>%
      rvest::html_text() %>%
      tail(-1)
  }

  table_out <- dplyr::bind_cols(player_info,mean_cells,max_cells,min_cells) %>%
    dplyr::mutate(
      dplyr::across(dplyr::matches("passing_|rushing_|receiving_|misc_"),
                    ~ stringr::str_remove(.x,",") %>% as.numeric())
    )

  #handle offensive players
  if("misc" %in% names(table_out)){
    table_out <- dplyr::select(table_out,
                               "fantasypros_id",
                               "player_name",
                               dplyr::starts_with("pos"),
                               "team",
                               dplyr::starts_with("passing_"),
                               dplyr::starts_with("rushing_"),
                               dplyr::starts_with("receiving_"),
                               dplyr::starts_with("misc_")
                               )
  }else{
    table_out <- dplyr::select(table_out,
                               "fantasypros_id",
                               "player_name",
                               dplyr::starts_with("pos"),
                               "team",
                               dplyr::everything()
                               )
  }

  return(list(projections = table_out, response = response$response))
}

#' MLB method for fp_rankings
#' @describeIn fp_rankings_parse
#'
#' @keywords internal
fp_projections_parse.fp_mlb <- function(response){

  table_html <- response$content %>%
    rvest::html_node("#data")

  suppressWarnings(
    player_info <- tibble::tibble(
      fantasypros_id =
        table_html %>%
        rvest::html_nodes(".fp-player-link") %>%
        rvest::html_attr("class") %>%
        stringr::str_extract_all("[0-9]+$") %>%
        unlist(),
      player_name =
        table_html %>%
        rvest::html_nodes(".fp-player-link") %>%
        rvest::html_attr("fp-player-name"),
      node = table_html %>%
        rvest::html_nodes("td:nth-child(1)") %>%
        rvest::html_node("small") %>%
        rvest::html_text()
    ) %>%
      dplyr::mutate(
        team = ifelse(
          stringr::str_detect(.data$node,"\\-"),
          .data$node %>%
            stringr::str_extract_all( "\\(([A-Z]+) \\-") %>%
            stringr::str_remove_all("\\(|\\-") %>%
            stringr::str_squish(),
          NA_character_),
        pos = .data$node %>%
          stringr::str_extract_all(("([[:alnum:],]+)\\)")) %>%
          stringr::str_remove_all("\\)") %>%
          stringr::str_squish(),
        node = NULL
      )
  )

  #### Base Case ####

  if(stringr::str_detect(response$query, "max\\-yes", negate = TRUE) ||
     stringr::str_detect(response$query, "min\\-yes", negate =  TRUE)){

    projections <- rvest::html_table(table_html) %>%
      magrittr::extract(,names(rvest::html_table(table_html))!="") %>%
      janitor::clean_names() %>%
      dplyr::select(-"player")

    table_out <- projections %>%
      dplyr::bind_cols(player_info) %>%
      dplyr::mutate(
        dplyr::across(-names(player_info),
                      ~ stringr::str_remove(.x,",|%") %>%
                        dplyr::na_if("NULL") %>%
                        as.numeric())
      ) %>%
      dplyr::select(
        dplyr::all_of(names(player_info)),
        dplyr::everything()
      )

    return(list(projections = table_out, response = response$response))
  }

  ## If min and max projections are displayed, do this

  max_cells <- NULL

  if(stringr::str_detect(response$query, "max\\-yes\\=true")) {
    max_names <- names(rvest::html_table(table_html))
    max_names <- ifelse(max_names == "","", paste0(max_names,"_max") %>% janitor::make_clean_names())

    max_cells <- table_html %>%
      rvest::html_nodes(".max-cell") %>%
      rvest::html_text() %>%
      matrix(nrow = nrow(player_info),
             byrow = TRUE,
             dimnames = list(seq_len(nrow(player_info)),
                             max_names[max_names!="rost_percent_max" & max_names !=""])
      ) %>%
      tibble::as_tibble(.name_repair = "minimal") %>%
      dplyr::select(-"player_max")

  }

  min_cells <- NULL

  if(stringr::str_detect(response$query, "min\\-yes\\=true")) {

    min_names <- names(rvest::html_table(table_html))
    min_names <- ifelse(min_names == "","", paste0(min_names,"_min") %>% janitor::make_clean_names())

    min_cells <- table_html %>%
      rvest::html_nodes(".min-cell") %>%
      rvest::html_text() %>%
      matrix(nrow = nrow(player_info),
             byrow = TRUE,
             dimnames = list(seq_len(nrow(player_info)),
                             min_names[min_names!="rost_percent_min" & min_names !=""])
      ) %>%
      tibble::as_tibble(.name_repair = "minimal") %>%
      dplyr::select(-"player_min")
  }

  mean_names <- names(rvest::html_table(table_html))
  mean_names <- ifelse(
    mean_names == "",
    "",
    paste0(mean_names,"_mean") %>%
      janitor::make_clean_names()) %>%
    tail(-1)

  mean_cells <- table_html %>%
    rvest::html_nodes("td:not(:first-child)") %>%
    xml2::as_list() %>%
    purrr::map(purrr::pluck(1)) %>%
    as.character() %>%
    matrix(nrow = nrow(player_info),
           byrow = TRUE,
           dimnames = list(1:nrow(player_info),
                           mean_names)
    ) %>%
    tibble::as_tibble(.name_repair = "minimal") %>%
    magrittr::extract(,mean_names !="")

  table_out <- dplyr::bind_cols(player_info,mean_cells,max_cells,min_cells) %>%
    dplyr::mutate(
      dplyr::across(-names(player_info),
                    ~ stringr::str_remove(.x,",|%") %>%
                      dplyr::na_if("NULL") %>%
                      as.numeric()),
      dplyr::across(dplyr::matches("percent"), ~.x/100)
    ) %>%
    dplyr::select(
      dplyr::all_of(names(player_info)),
      dplyr::everything()
    )

  return(list(projections = table_out, response = response$response))


}
#' NHL method for fp_rankings
#' @describeIn fp_rankings_parse
#'
#' @keywords internal
fp_projections_parse.fp_nhl <- function(response){
  warning("No projections for NHL yet")
  return(response)
}

#' NBA method for fp_rankings
#' @describeIn fp_rankings_parse
#'
#' @keywords internal
fp_projections_parse.fp_nba <- function(response){

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
    tidyr::extract(
      .data$team,
      into = c("team","pos"),
      regex = "(\\([A-Z]+ )\\- ([A-Z,]+\\))"
    ) %>%
    dplyr::mutate(
      pos = stringr::str_remove_all(.data$pos,"\\)") %>% stringr::str_squish(),
      team = stringr::str_remove_all(.data$team,"\\(") %>% stringr::str_squish()
    )

  projections <- rvest::html_table(table_html) %>%
    janitor::clean_names() %>%
    dplyr::rename_with(
      ~stringr::str_replace(.x,"percent","pct") %>%
        stringr::str_replace("x3pm","threes_made")
    ) %>%
    dplyr::select(-"player") %>%
    dplyr::bind_cols(player_info) %>%
    dplyr::select(
      dplyr::all_of(names(player_info)),
      dplyr::everything()
    ) %>%
    dplyr::mutate(
      dplyr::across(-names(player_info),
                    ~ stringr::str_remove(.x,",") %>%
                      as.numeric()
      )
    )

  return(list(projections = projections, response = response$response))
}
