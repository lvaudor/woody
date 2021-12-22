#'Runs model random forest object on new data
#
#' @param data data with variables Q, rT_Q, S
#' @param obj_rf random forest object
#' @export
#' @return Wdata completed with column Ypred (log of hourly flux) and Npred (hourly flux)

predict_rf=function(newdata,obj_rf){
  newdata=newdata %>%
    dplyr::mutate(Ypred=predict(object=obj_rf,newdata=newdata))
  return(newdata)
}
