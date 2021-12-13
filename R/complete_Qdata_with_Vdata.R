#' Completes Qdata with variables Vmax,Vsum. => QVdata
#' @export
#' @param Qdata discharge data
#' @param Vdata wind data
#' @return QVdata completed with variables Vmax,Vsum
complete_Qdata_with_Vdata=function(Qdata,Vdata){
  result=Qdata %>%
    dplyr::mutate(Date=lubridate::date(Time)) %>%
    dplyr::mutate(Date_bef=Date-lubridate::days(round(T_Q))) %>%
    dplyr::mutate(id=1:dplyr::n()) %>%
    dplyr::group_by(id) %>%
    tidyr::nest(data=c(Date,Date_bef,T_Q))
  f_period=function(data){
    res=tibble::tibble(Vmax=NA,Vsum=NA)
    if(is.na(data$Date_bef)){
      return(res)
    }
    Vsub=Vdata %>%
      dplyr::filter(Date>=data$Date_bef,
                    Date<=data$Date)
    if(nrow(Vsub)==0){
      return(res)
    }
    res=Vsub %>%
      dplyr::summarise(Vmax=max(FFM),
                       Vsum=mean(FFM))%>%
      dplyr::mutate(Vsum=Vsum*data$T_Q) %>%
      dplyr::mutate(Vmax=case_when(data$T_Q>1~Vmax,
                                   data$T_Q<=1~0))
    return(res)
  }
  result=result %>%
    dplyr::mutate(QVdata=purrr::map(data,f_period))%>%
    tidyr::unnest(cols=c(data,QVdata)) %>%
    dplyr::select(-Date,-Date_bef) %>%
    ungroup() %>%
    dplyr::select(-id)
  return(result)
}



