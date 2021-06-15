with_mock_api({

  test_that("fp_rankings works for NFL",{

    skippy()

    nfl_dynasty_overall <- fp_rankings(page = "dynasty-overall")
    checkmate::expect_tibble(nfl_dynasty_overall, min.rows = 200, min.cols = 20)

    nfl_redraft_2018 <- fp_rankings(page = "consensus-cheatsheets", sport = "nfl", year = 2018)
    checkmate::expect_tibble(nfl_redraft_2018, min.rows = 200, min.cols = 20)

    nfl_flex_2020_3 <- fp_rankings(page = "flex", sport = "nfl", week = 3, year = 2020, include_metadata = TRUE)
    checkmate::expect_tibble(nfl_flex_2020_3$ecr, min.rows = 200, min.cols = 20)
    checkmate::expect_list(nfl_flex_2020_3, len = 3)

  })

  test_that("fp_rankings works for NHL", {

    skippy()
    nhl_ros_overall <- fp_rankings(page = "ros-overall", sport = "nhl")
    checkmate::expect_tibble(nhl_ros_overall, min.rows = 200, min.cols = 5)

  })

  test_that("fp_rankings works for MLB", {

    skippy()
    mlb_ros_overall <- fp_rankings(page = "overall", sport = "mlb")
    checkmate::expect_tibble(mlb_ros_overall, min.rows = 200, min.cols = 5)

  })

  test_that("fp_rankings works for NBA", {

    skippy()
    nba_ros_overall <- fp_rankings(page = "ros-overall", sport = "nba")
    checkmate::expect_tibble(nba_ros_overall, min.rows = 200, min.cols = 5)
  })

})
