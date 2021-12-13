#' Collects Qdata back in time (enough to get previous, high enough peak in discharge)
#' @export
#' @param Wdata wood data for which to collect Qdata
#' @param station gauging station
#' @param extratime time (in seconds) before and after wood occurrences for which also to collect Qdata
#' @param back_in_time whether to collect enough data back in time to be able to calculate T_Q
#' @return Qdata: discharge data over period covered by Qdata, + extratime seconds before and extratime seconds after
get_Qdata=function(Wdata, station, extratime=1200, back_in_time=TRUE){
  t1=min(Wdata$Time,na.rm=TRUE)-lubridate::seconds(extratime)
  t2=max(Wdata$Time,na.rm=TRUE)+lubridate::seconds(extratime)
  Qdata=banqueHydro::bh_get_qtvar(station=station,
                                  t1 %>% banqueHydro::format_time_for_qtvar(),
                                  t2 %>% banqueHydro::format_time_for_qtvar())
  if(back_in_time){
    Qdata=get_Qdata_back(Qdata)
  }
  return(Qdata)
}
