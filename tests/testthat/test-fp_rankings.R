
test_that("fp_getrankings works for NFL",{

  nfl_dynasty_overall <- fp_rankings(page = "dynasty-overall", sport = "nfl")

  checkmate::expect_tibble(nfl_dynasty_overall, min.rows = 300, min.cols = 20)

  expect_error(
    fp_rankings(page = "dynasty-asdf", sport = "nfl"),
               regexp = "please recheck page name")

} )

test_that("fp_getrankings works for NHL", {

  nhl_ros_overall <- fp_rankings(page = "ros-overall", sport = "nhl")

  checkmate::expect_data_frame(nhl_ros_overall, min.rows = 300, min.cols = 5)

})
