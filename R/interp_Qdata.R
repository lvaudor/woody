#' interpolate Qdata to a period
#' @param Qdata data to interpolate
#' @param timescale time scale at which to interpolate data (defaults to minutes -"mins"-)
#' @export
#' @return summarised data

interp_Qdata=function(Qdata,timescale="mins"){
Qdata_interp=tibble::tibble(Time=seq(from=round(min(Qdata$Time),timescale),
                                     to=round(max(Qdata$Time),timescale),
                                     by=timescale)) %>%
  mutate(Q=approx(x=Qdata$Time,y=Qdata$Q,xout=.$Time)$y,
         T_Q=approx(x=Qdata$Time,y=Qdata$T_Q,xout=.$Time)$y,
         S=approx(x=Qdata$Time,y=Qdata$S,xout=.$Time)$y,
         rT_Q=approx(x=Qdata$Time,y=Qdata$rT_Q,xout=.$Time)$y) %>%
  mutate(site=unique(Qdata$site),
         station=unique(Qdata$station)) %>%
  select(site,station,everything())
return(Qdata_interp)
}
