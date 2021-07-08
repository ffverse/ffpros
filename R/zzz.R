#### On Load ####

.ffpros_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {

  if(is.null(getOption("ffpros.sport"))) options(ffpros.sport = "nfl")
  if(is.null(getOption("ffpros.include_metadata"))) options(ffpros.include_metadata = FALSE)
  if(is.null(getOption("ffpros.user_agent"))) {
    user_agent <- glue::glue(
      "ffpros/{utils::packageVersion('ffpros')} ",
      "R client package ",
      "https://github.com/dynastyprocess/ffpros")

    options(ffpros.user_agent = user_agent)
  }

  # nocov start

  # Memoise specific functions
  # list of memoise options: "memory", "filesystem","off"
  memoise_option <- getOption("ffpros.cache")

  if (is.null(memoise_option) || !memoise_option %in% c("memory", "filesystem", "off")) {
    memoise_option <- "memory"
  }

  if (memoise_option == "filesystem") {
    cache <- cachem::cache_disk(dir = tools::R_user_dir("ffpros", which = "cache"))
  }

  if (memoise_option == "memory") cache <- cachem::cache_mem()

  if (memoise_option != "off") {
    .fp_get <<-memoise::memoise(.fp_get, ~memoise::timeout(3600), cache = cache)
  }

  # https://www.fantasypros.com/robots.txt
  # crawl-delay: 5
  get <-  ratelimitr::limit_rate(.retry_get, ratelimitr::rate(1, 5))
  assign("get", get, envir = .ffpros_env)

  # nocov end
}

.onAttach <- function(libname, pkgname){

  memoise_option <- getOption("ffpros.cache")

  if(!is.null(memoise_option) && memoise_option == "off")
    packageStartupMessage('Note: ffpros.cache is set to "off"')
}
