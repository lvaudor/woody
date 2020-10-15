#' runs random forest model
#' @param Wdatc_wt data with wood data as waiting times and variables Q, rT_Q, S, Y
#' @export
#' @return random forest object
#' @examples

run_rf=function(Wdatc_wt){
  Wdata_rf <- Wdatc_wt %>%
    dplyr::select(Q,
                  rT_Q,
                  S,
                  Y) %>%
    na.omit()
  myrf=randomForest::randomForest(Y~.,data=Wdata_rf)
  return(myrf)
}
