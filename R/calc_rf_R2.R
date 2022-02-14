#' calculates R2
#' @param data data with variables Y and Y_pred if type="Y" or N_obse and N_pred if type="N"
#' @param type type of variables ("Y" or "N": defaults to "Y")
#' @export
#' @return R2
calc_rf_R2=function(data, type="Y"){
  if(type=="N"){
    data=data %>% mutate(Y=N_obse,Y_pred=N_pred)
  }
  R2=data %>%
      dplyr::select(Y,Y_pred) %>% na.omit() %>%
      dplyr::mutate(CR=(Y_pred-Y)^2,CT=(Y-mean(Y))^2) %>%
      dplyr::summarise(SCR=sum(CR),SCT=sum(CT)) %>%
      dplyr::mutate(R2=1-SCR/SCT)
  return(R2)
}
