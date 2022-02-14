#'Runs model random forest object on new data
#
#' @param data data with variables Q, rT_Q, S
#' @param obj_rf random forest object
#' @export
#' @return Wdata completed with column Ypred (log of hourly flux) and Npred (hourly flux)

predict_rf=function(newdata,obj_rf){
  # make sure newdata has same factor levels as original data
  # if one of the predictors is indeed a factor
  nlevs=purrr::map_dbl(myrf$forest$xlevels,length)
  if(any(nlevs>1)){
    ifac=which(nlevs>1)
    namefac=names(nlevs)[ifac]
    levsfac=myrf$forest$xlevels[[ifac]]
    newdata[[namefac]]=factor(newdata[[namefac]],levels=levsfac)
  }

  newdata=newdata %>%
    dplyr::mutate(Y_pred=predict(object=obj_rf,newdata=newdata))
  return(newdata)
}
