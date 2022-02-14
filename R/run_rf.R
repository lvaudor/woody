#' runs random forest model
#' @param Wdatc_wt data with wood data as waiting times
#' @param pred_vars predictor variables by default c("Q","rT_Q","S")
#' @export
#' @return random forest object
run_rf=function(Wdatcwt,pred_vars=c("Q","S","rT_Q")){
  all_vars=c(pred_vars,"Y")
  Wdata_rf <- Wdatcwt %>%
    dplyr::select(all_vars) %>%
    na.omit()
  myrf=randomForest::randomForest(Y~.,data=Wdata_rf, ntree=1000)
  return(myrf)
}
