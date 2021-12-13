#' Transform Wdata (1 line= 1 occurrence of wood) into Wdata_wt (1 line= time between two occurrences of wood)
#' @param Wdata wood data
#' @param maxWait the maximum waiting time in seconds (defaults to 1200 seconds i.e. 20 minutes)
#' @export
#' @return Wdata completed with column W (waiting time between two wood occurrences)

Wdata_as_waiting_times=function(Wdata, maxWait=1200){
  Wdata_wt=Wdata %>%
    dplyr::arrange(Time) %>%
    dplyr::mutate(TimeLagged=dplyr::lag(Time,1)) %>%
    dplyr::mutate(W=difftime(Time,TimeLagged, units="secs") %>%
                    as.numeric()) %>%
    dplyr::group_by(Time) %>%
    dplyr::mutate(npieces=dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(W=dplyr::case_when(npieces<=1~W,
                                     npieces>1~1/npieces)) %>%
    dplyr::filter(W < maxWait) %>%
    dplyr::mutate(Y=log(3600/W)) %>%
    na.omit()
  return(Wdata_wt)
}
