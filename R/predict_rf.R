#'Runs model random forest object on new data
#' @param obj_rf random forest object
#' @param data data with variables Q, rT_Q, S
#' @export
#' @return Wdata completed with column W (waiting time between two wood occurrences)
#' @examples

predict_rf=function(obj_rf, newdata){
  newdata=newdata %>%
    dplyr::mutate(Ypred=predict(obj_rf,newdata))
  return(newdata)
}
