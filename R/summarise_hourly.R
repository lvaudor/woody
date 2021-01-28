#' summarise data hourly
#' @param data
#' @param type of data either "Wdatc" or something else e.g. "Ddata"
#' @export
#' @return random forest object
#' @examples
summarise_hourly=function(data, type="Wdatc"){
  data=data %>%
    mutate(Time = floor_date(Time, "hour")) %>%
    group_by(site,event,Date,Time)
  if(type=="Wdatc"){
    data=data%>%
      group_by(site,event,sitevent,Date,Time)
    data=data %>%
      mutate(N=dplyr::n()) %>%
      mutate(Y=log(N))
  }
  data=mutate(data,
              Q=mean(Q),
              rT_Q=mean(rT_Q),
              S=mean(S),
              T_Q=mean(T_Q))
  if("Vmax" %in% colnames(data)){
    data=data %>%
      mutate(Vmax=max(Vmax)) %>%
      mutate(Vsum=mean(Vsum))
  }
  data=data %>%
    unique() %>%
    ungroup()
  return(Wdath)
}
