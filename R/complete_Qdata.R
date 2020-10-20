#' Completes Qdata with variables T_Q and S.
#' @export
#' @param Qdata
#' @param Qdata_back
#' @return Qdata completed with variables T_Q, rT_Q and  S
#' @examples
complete_Qdata=function(Qdata){
  Qdata_back=get_Qdata_back(Qdata)
  print("Calculating T_Q.")
  Qdata=Qdata %>%
    dplyr::mutate(T_Q=purrr::map_dbl(Time,
                                     .f=calc_T_Q,
                                     Qdata=Qdata,
                                     Qdata_back=Qdata_back))
  Qdata=Qdata %>%
    dplyr::mutate(S=Q-approx(Qdata$Time,
                             Qdata$Q,
                             xout=Qdata$Time-60*5)$y) %>%
    dplyr::mutate(rT_Q=sqrt(T_Q))
  return(Qdata)
}
