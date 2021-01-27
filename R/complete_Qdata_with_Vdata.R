#' Completes Qdata with variables Vmax,Vsum. => QVdata
#' @export
#' @param Qdata
#' @param Vdata
#' @return QVdata completed with variables Vmax,Vsum
#' @examples
complete_Qdata_with_Vdata=function(Qdata,Vdata){
  result=Qdata %>%
    dplyr::mutate(Date=lubridate::date(Time)) %>%
    dplyr::mutate(Date_bef=Date-lubridate::days(round(T_Q))) %>%
    dplyr::group_by(Date) %>%
    tidyr::nest(data=c(Date,Date_bef,T_Q))
  f_period=function(data){
    if(is.na(data$Date_bef)){
      return(tibble::tibble(Vmax=NA,Vsum=NA))
    }
    if(data$T_Q==0){
      return(tibble::tibble(Vmax=Vdata %>%
                              dplyr::filter(Date==data$Date) %>%
                              dplyr::pull(FFM),
                            Vsum=0))
    }
    res=Vdata %>%
      dplyr::filter(Date>=data$Date_bef,
             Date<=data$Date) %>%
      dplyr::summarise(Vmax=max(FFM),
                Vsum=mean(FFM))%>%
      dplyr::mutate(Vsum=Vsum*data$T_Q)
    return(res)
  }
  result=result %>%
    dplyr::mutate(QVdata=purrr::map(data,f_period))%>%
    tidyr::unnest(cols=c(data,QVdata)) %>%
    dplyr::select(-Date,-Date_bef)
  return(result)
}

