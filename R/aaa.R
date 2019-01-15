
#  https://github.com/Rdatatable/data.table/issues/850
utils::globalVariables(c('isCluster', 'datetime', 'tenure'))


NULL

#DATASETS 


#' @name pesa56511
#' @title A dataset containing the track of a pectoral sandpiper male. 
#' @docType data
#' @format Track
#' \describe{
#'   \item{trajectories::Track}{with "connections","data","sp", "time", "endTime" slots}
#'   \item{quality}{in data slot, is the Argos location quality}
#' }
#' @keywords data
NULL