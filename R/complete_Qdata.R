#' Completes qtvar with variables T_Q and S. => Qdata
#' @export
#' @param qtvar
#' @return Qdata completed with variables T_Q, rT_Q and  S
#' @examples
complete_Qdata=function(qtvar,qnorm,site){
      qtvarc=qtvar %>%
        unique() %>%
        dplyr::arrange(Time) %>%
        dplyr::mutate(index=1:dplyr::n(),
                      Q=Q/qnorm,
                      site=rep(site,n()))
      result= qtvarc%>%
        mutate(id=index) %>%
        group_by(id) %>%
        tidyr::nest(data=c(Time,Q,index))

      f_lasttime=function(data){
        dataslice=qtvarc %>%
          filter(Q>=data$Q,
                 index<data$index)
        if(nrow(dataslice)==0){return(NA_real_)}
        indexl=dataslice %>%
          summarise(index=max(index)) %>%
          pull(index)
        if(indexl==data$index-1){return(data$Time)}
        datatarget=qtvarc %>%
          filter(index %in% c(indexl,indexl+1)) %>%
          select(Time,Q)
        lasttime=approx(datatarget$Q,datatarget$Time,xout=data$Q)$y
        return(lasttime)
      }
      result=result %>%
        dplyr::mutate(lasttime=purrr::map_dbl(data,.f=f_lasttime))%>%
        dplyr::mutate(lasttime=as.POSIXct(lasttime,
                                          origin="1970-01-01 00:00:00",
                                          tz="UTC")) %>%
        tidyr::unnest(cols=c(data)) %>%
        dplyr::mutate(T_Q=as.numeric(difftime(Time,lasttime,units="days"))) %>%
        dplyr::mutate(T5minbef=Time-60*5) %>%
        dplyr::mutate(S=Q-approx(qtvarc$Time,
                                 qtvarc$Q,
                                 xout=T5minbef)$y) %>%
        dplyr::mutate(rT_Q=sqrt(T_Q)) %>%
        dplyr::select(site,station,Time,Q,T_Q,S,rT_Q) %>%
        dplyr::ungroup() %>%
        dplyr::select(-id)
  return(result)
}


