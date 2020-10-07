#' calculates R2
#' @param data data with variables Y and Ypred
#' @export
#' @return R2
#' @examples
calc_rf_R2=function(data){
  R2=data %>%
    dplyr::mutate(CR=(Ypred-Y)^2,CT=(Y-mean(Y))^2) %>%
    dplyr::summarise(SCR=sum(CR),SCT=sum(CT)) %>%
    dplyr::mutate(R2=1-SCR/SCT)
  return(R2)
}
