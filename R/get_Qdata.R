#' Collects Qdata back in time (enough to get previous, high enough peak in discharge)
#' @export
#' @param Wdata wood data for which to collect Qdata
#' @param station gauging station
#' @param extratime time (in seconds) before and after wood occurrences for which also to collect Qdata
#' @return Qdata: discharge data over period covered by Qdata, + extratime seconds before and extratime seconds after
#' @examples

get_Qdata=function(Wdata, station, extratime=1200){
  Qdata=banqueHydro::bh_get_qtvar(station=station,
                                  t1=min(Wdata$Time,na.rm=TRUE)-lubridate::seconds(extratime),
                                  t2=max(Wdata$Time,na.rm=TRUE)+lubridate::seconds(extratime))
  return(Qdata)
}
