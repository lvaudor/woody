#' summarise data hourly
#' @param data data to summarise
#' @param type of data either "Wdatc" or something else e.g. "Ddata"
#' @export
#' @return random forest object
summarise_hourly=function(data, type="Wdatc", Adata="notprovided"){
  data=data %>%
    mutate(Time = floor_date(Time, "hour")) %>%
      group_by(site,event,sitevent,Date,Time) %>%
      mutate(N=dplyr::n()) %>%
      mutate(Y=log(N))
  if(is.data.frame(Adata)){
    data=data %>%
      dplyr::left_join(Adata,by=c("Time")) %>%
      dplyr::mutate(N=N/obs_duration) %>%
      dplyr::mutate(Y=log(N))
  }
  if(type!="Wdatc"){
    data=data %>%
      dplyr::select(-N,-Y)
  }
  if("Q" %in% colnames(data)){
  data=mutate(data,
              Q=mean(Q),
              rT_Q=mean(rT_Q),
              S=mean(S),
              T_Q=mean(T_Q))
  }
  if("Vmax" %in% colnames(data)){
    data=data %>%
      mutate(Vmax=max(Vmax)) %>%
      mutate(Vsum=mean(Vsum))
  }
  data=data %>%
    unique() %>%
    ungroup()
  return(data)
}
