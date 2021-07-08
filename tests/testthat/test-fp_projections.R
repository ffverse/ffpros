with_mock_api({

  test_that("fp_projections works for NFL",{

    skippy()

    qb <- fp_projections("qb")
    wr_draft_2016 <- fp_projections("wr", year = 2016, week = "draft", scoring = "PPR")
    flex_2020_7 <- fp_projections("flex", year = 2020, week = 7, scoring = "PPR", `min-yes`="true", `max-yes`="true")

    checkmate::expect_tibble(qb, min.rows = 50, min.cols = 10)
    checkmate::expect_tibble(wr_draft_2016, min.rows = 80, min.cols = 10)
    checkmate::expect_tibble(flex_2020_7, min.rows = 200, min.cols = 20)
  })
#
#   test_that("fp_rankings works for NHL", {
#
#     skippy()
#     nhl_ros_overall <- fp_rankings(page = "ros-overall", sport = "nhl")
#     checkmate::expect_tibble(nhl_ros_overall, min.rows = 200, min.cols = 5)
#
#   })
#
#   test_that("fp_rankings works for MLB", {
#
#     skippy()
#     mlb_ros_overall <- fp_rankings(page = "overall", sport = "mlb")
#     checkmate::expect_tibble(mlb_ros_overall, min.rows = 200, min.cols = 5)
#
#   })
#
#   test_that("fp_rankings works for NBA", {
#
#     skippy()
#     nba_ros_overall <- fp_rankings(page = "ros-overall", sport = "nba")
#     checkmate::expect_tibble(nba_ros_overall, min.rows = 200, min.cols = 5)
#   })

})
