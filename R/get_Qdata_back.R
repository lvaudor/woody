#' Collects Qdata back in time (enough to get previous, high enough peak in discharge)
#' @param station gauging station
#' @param Qdata discharge data for which to look back in time
#' @return Qdata_back: discharge data back in time. Length of the period is enough to ensure calculation of T_Q for Qdata.
get_Qdata_back=function(Qdata){
  station=unique(Qdata$station)
  Qmax=Qdata%>%
    dplyr::summarise(Qmax=max(Q,na.rm=T)) %>%
    dplyr::pull(Qmax)
  Qmax_prior=Qmax-1
  Qdata_back=Qdata %>%
    dplyr::filter(Time==min(Time))
  while(Qmax_prior<Qmax){
      Sys.sleep(30)
      tmin=min(Qdata_back$Time,na.rm=TRUE)
      t1=tmin-lubridate::years(1) %>%
        format_time_for_qtvar()
      t2=tmin %>%
        format_time_for_qtvar()
      Qdata_back_oneyear=banqueHydro::bh_get_qtvar(station=station,
                                                   t1=t1,
                                                   t2=t2)
      Qdata_back=dplyr::bind_rows(Qdata_back,
                                  Qdata_back_oneyear)
      Qmax_prior=Qdata_back %>%
        dplyr::summarise(Qmax=max(Q,na.rm=T)) %>%
        dplyr::pull(Qmax)
  }
  Qdata_back=Qdata_back %>%
    unique()
  return(Qdata_back)
}
