#' summarise Wdata to a period
#' @param Wdata data to summarise
#' @param Adata the annotation times (if available)
#' @export
#' @return summarised data
#'
summarise_Wdata=function(Wdata,Adata=NULL){
    data_grouped=Wdata %>%
      mutate(Time = lubridate::floor_date(Time, "hour")) %>%
      group_by(site,event,sitevent,Date,Time)
    if("Y_obs" %in% names(data_grouped)){
      # if Wdata is of type Wwt
      data_grouped=data_grouped %>%
        dplyr::mutate(Y_obs=mean(Y),
                      N_obs=n()+1,
                      P_obs=sum(W)/3600) %>%
        # if Adata is not available, approximate P_obs by sum of waiting times
        dplyr::mutate(N_obse=N_obs/P_obs) %>%
        dplyr::select(-P_obs)
    }else{
      data_grouped=data_grouped %>%
        dplyr::mutate(N_obs=n())
    }
    if(!is.null(Adata)){
      result=data_grouped %>%
        dplyr::left_join(Adata,by=c("Time"))  %>%
        dplyr::mutate(N_obse=N_obs/P_obs)
    }
  result=result %>%
    unique() %>%
    ungroup()
  return(result)
}
