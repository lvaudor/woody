#' Interpolates Qdata to add discharge variables to Wdata
#' @export
#' @param Wdata
#' @param Qdata
#' @return Wdata completed with variables Q, T_Q, and S and rT_Q
#' @examples
complete_Wdata_with_Qdata_onesite=function(Wdata,Qdata, newvars=c("Q","T_Q","S","rT_Q")){
  for(var in newvars){
    Wdata=Wdata %>%
      mutate(!!var:=approx(Qdata$Time,Qdata[[var]], xout=Wdata$Time)$y)
  }
  return(Wdata)
}
complete_Wdata_with_Qdata=function(Wdata,Qdata, newvars=c("Q","T_Q","S","rT_Q")){
  result <- Wdata %>%
    group_by(site) %>%
    nest(Wdata=-site)   %>%
    mutate(Qdata=purrr::map(site,~filter(Qdata,site==.x))) %>%
    mutate(Wdatac=purrr::map2(Wdata,
                              Qdata,
                              complete_Wdata_with_Qdata_onesite,
                              newvars=newvars)) %>%
    select(site,Wdatac) %>%
    unnest(cols=c(site,Wdatac)) %>%
    ungroup()
  return(result)
}
