#'Runs model random forest object on new data
#
#' @param data data with variables Q, rT_Q, S
#' @param obj_rf random forest object
#' @export
#' @return Wdata completed with column W (waiting time between two wood occurrences)
#' @examples

predict_rf=function(newdata,obj_rf){
  newdata=newdata %>%
    dplyr::mutate(Ypred=predict(obj_rf,newdata)) %>%
    dplyr::mutate(Npred=exp(Ypred))
  return(newdata)
}
