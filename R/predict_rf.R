#'Runs model random forest object on new data
#
#' @param data data with variables Q, rT_Q, S
#' @param obj_rf random forest object
#' @param correction whether to correct covariate shift, in which case two coefficients (alpha,beta) should be provided. Defaults to (1,0) i.e. no correction.
#' @export
#' @return Wdata completed with column Ypred (log of hourly flux) and Npred (hourly flux)

predict_rf=function(newdata,obj_rf, correction=c(1,0)){
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
    dplyr::mutate(Y_pred=predict(object=obj_rf,newdata=newdata)) %>%
    dplyr::mutate(Y_pred=correction[1]*Y_pred+correction[2])

  return(newdata)
}
