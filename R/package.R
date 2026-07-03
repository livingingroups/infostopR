#' @useDynLib infostopR, .registration = TRUE
#' @importFrom Rcpp evalCpp
#' @importFrom checkmate check_list check_character check_logical
#' @importFrom checkmate assert assert_choice assert_numeric assert_character
#' @importFrom stats runif aggregate as.formula median time
#' @importFrom trackframe as.trackframe tf_options guess_all_cols
#' @importFrom trackframe northing easting id
#' @importFrom trackframe id_col easting_col northing_col time_col "time<-"
#' @importFrom sf st_coordinates st_crs
NULL
