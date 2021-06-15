library(xfun)

paths <- c(
  "ffpros-tests-1.4.2",
  "ffpros-tests-main",
  "https://github.com/dynastyprocess/ffpros-tests/archive/main.zip",
  "https://github.com/dynastyprocess/ffpros-tests/archive/1.4.2.zip"
)

use_ffpros_tests_main <- function(){

  gsub_dir("archive/[0-9,\\.]+\\.zip", "archive/main.zip", dir = ".",recursive = TRUE, ext = c("R","Rmd"))
  #gsub_dir("archive/[0-9,\\.]+\\.zip", "archive/main.zip", dir = "vignettes",recursive = TRUE, ext = c("R","Rmd"))

  gsub_dir("ffpros\\-tests\\-[0-9\\.]+", "ffpros-tests-main", dir = ".", recursive = TRUE, ext = c("R","Rmd"))
  #gsub_dir("ffpros\\-tests\\-[0-9\\.]+", "ffpros-tests-main", dir = "vignettes", recursive = TRUE, ext = c("R","Rmd"))

  message("Check your version control now!")
  invisible()
}

# use_ffpros_tests_main()

use_ffpros_tests_version <- function(version){

  stopifnot(length(version)==1)

  gsub_dir("archive/main", paste0("archive/",version), dir = ".", ext = c("R","Rmd"), recursive = TRUE)

  gsub_dir("ffpros\\-tests\\-main", paste0("ffpros-tests-",version), dir = ".", ext = c("R","Rmd"), recursive = TRUE)

  message("Check your version control now!")
  invisible()
}

# use_ffpros_tests_version('1.4.3')
