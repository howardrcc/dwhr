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
#' @importFrom checkmate assert_string assert_flag assert_number
#'   assert_character assert_numeric assert_list assert_data_frame
#'   assert_class assert_subset assert_true test_flag
"_PACKAGE"
