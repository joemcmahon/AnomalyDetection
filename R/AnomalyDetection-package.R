#' Anomaly Detection Using Seasonal Hybrid Extreme Studentized Deviate Test
#'
#' A technique for detecting anomalies in seasonal univariate time series.
#' The methods uses are robust, from a statistical standpoint, in the presence of
#' seasonality and an underlying trend. These methods can be used in
#' wide variety of contexts. For example, detecting anomalies in system metrics after
#' a new software release, user engagement post an 'A/B' test, or for problems in
#' econometrics, financial engineering, political and social sciences.
#'
#' @name AnomalyDetection
#' @docType package
#' @author Owen S. Vallis, Jordan Hochenbaum, Arun Kejariwal; Modernization
#'         contributions by Bob Rudis
#' @importFrom stats aggregate mad median na.omit qt quantile stl ts
#' @importFrom stringr str_detect
#' @importFrom lubridate days floor_date hours
NULL

