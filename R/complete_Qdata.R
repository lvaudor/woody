#' Completes qtvar with variables T_Q and S. => Qdata
#' @export
#' @param qtvar
#' @return Qdata completed with variables T_Q, rT_Q and  S
#' @examples
complete_Qdata=function(qtvar,qnorm,site){
  Qdata=qtvar %>%
    dplyr::mutate(Q=Q/qnorm,
                  site=rep(site,n())) %>%
    dplyr::mutate(T_Q=purrr::map_dbl(Time,
                                     .f=calc_T_Q,
                                     Qdata=qtvar)) %>%
    dplyr::mutate(S=Q-approx(qtvar$Time,
                             qtvar$Q,
                             xout=qtvar$Time-60*5)$y) %>%
    dplyr::mutate(rT_Q=sqrt(T_Q)) %>%
    dplyr::select(site,Q,everything())
  return(Qdata)
}
