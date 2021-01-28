#' calculates rf performance
#' @param data data with variables Y and Ypred
#' @export
#' @return tibble with meanError and bias
#' @examples
calc_rf_perf=function(data){
  result=tibble(
    meanError=mean(abs(data$Ypred-data$Y)),
    bias=mean(data$Ypred-data$Y)
  )
  return(result)
}
