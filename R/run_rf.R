#' runs random forest model
#' @param Wdata data with variables Q, rT_Q, S, Y
#' @export
#' @return Wdata completed with column W (waiting time between two wood occurrences)
#' @examples

run_rf=function(Wdata){
  Wdata_rf <- Wdata %>%
    dplyr::select(Q,
                  rT_Q,
                  S,
                  Y) %>%
    na.omit()
  myrf=randomForest::randomForest(Y~.,data=Wdata_rf)
  return(myrf)
}
