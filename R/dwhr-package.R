#' dwhr: Interactive Shiny Dashboards Over Star-Schema Data Warehouse Data
#'
#' Provides a framework for building interactive Shiny dashboards over
#' star-schema data warehouse data. Users construct a `star` object from a
#' facts table, dimension views, measures, and presentation specifications;
#' the package handles the reactive plumbing between drillable hierarchical
#' dimensions and DataTable, Highcharts, and form control presentations.
#'
#' @keywords internal
#'
#' @import data.table
#' @import shiny
#' @importFrom checkmate assert_string assert_flag assert_number
#'   assert_character assert_numeric assert_list assert_data_frame
#'   assert_class assert_subset assert_true test_flag
#' @importFrom magrittr %>%
#' @importFrom highcharter datetime_to_timestamp
#' @importFrom utils read.csv head packageVersion
#' @importFrom stats coef lm
#' @importFrom grDevices col2rgb colorRampPalette
"_PACKAGE"

# Globals known at runtime but invisible to R CMD check's static analyzer:
# - `glob.env` is set by initGlob() via `.GlobalEnv$glob.env <- glob.env`
#   and accessed by bare name throughout the package.
# - `level0Label` is a data.table column name referenced inside DT
#   expressions (NSE).
# - `point` is a Highcharts JS context referenced inside JS() blocks.
utils::globalVariables(c("glob.env", "level0Label", "point"))
