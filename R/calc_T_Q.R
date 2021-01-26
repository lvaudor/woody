#' Based on Qdata and Qdata back in time, calculate T_Q
#'
#' @param time_obs a time of observation in Qdata
#' @param Qdata Qdata
#' @param Qdata_back Qdata before period covered by Qdata
#' @return the value of T_Q for the observation at time `time_obs` in `Qdata`
#' @examples
calc_T_Q=function(time_obs,Qdata){
  Qdatac=Qdata %>%
    unique() %>%
    dplyr::arrange(Time) %>%
    dplyr::mutate(index=1:dplyr::n())
  obs=Qdatac %>%
    dplyr::filter(Time==time_obs)
  Q_obs=obs %>%
    dplyr::select(Q) %>%
    dplyr::pull()
  index_obs=obs %>%
    dplyr::select(index) %>%
    dplyr::pull()
  index_last_time=Qdatac %>%
    dplyr::filter(Time<time_obs) %>%
    dplyr::mutate(s=sign(Q_obs-Q)) %>%
    dplyr::filter(s<0) %>%
    dplyr::select(index) %>%
    tail(n=1) %>%
    dplyr::pull()
  if(length(index_last_time)==0){T_Q=NA}else{
        last_time=Qdatac %>%
          dplyr::filter(index %in% c(index_last_time,index_last_time+1))
        numeric_approx_of_time=approx(x=last_time$Q,
                                      y=as.numeric(last_time$Time,
                                                   origin="1970-01-01 00:00:00"),
                                      xout=Q_obs)$y
        estim_time=as.POSIXct(numeric_approx_of_time,
                              origin="1970-01-01 00:00:00",
                              tz="UTC")
        T_Q=difftime(time_obs,estim_time,units="days")
  }
  T_Q=as.double(T_Q)
  return(T_Q)
}
