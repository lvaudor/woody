#' Completes qtvar with variables T_Q and S. => Qdata
#' @param qtvar the discharge dataset (with varying time resolution).
#' @param qnorm standard value of discharge used to normalise variable Q. Defaults to 1 (no normalisation).
#' @param site the site name (defaults to "site")
#' @param complete_T_Q whether to complete T_Q when the series does not go back enough in time to find such value of Q. Defaults to TRUE
#' @return Qdata completed with variables T_Q, rT_Q and  S
#' @export
complete_Qdata=function(qtvar,qnorm=1, site="site", complete_T_Q=TRUE){
      # qtvarc= qtvar %>%
      # remove duplicates
      # arrange by Time
      # number lines with variable index
      qtvarc=qtvar %>%
        unique() %>%
        dplyr::arrange(Time) %>%
        dplyr::mutate(index=1:dplyr::n(),
                      Q=Q/qnorm,
                      site=rep(site,))
      # Build individual datasets
      # corresponding to each Time
      result= qtvarc%>%
        dplyr::mutate(id=index) %>%
        dplyr::group_by(id) %>%
        tidyr::nest(data=c(Time,Q,index))

      # For such an individal dataset,
      # function f_lasttime calculates
      # last time such a discharge occurred
      f_lasttime=function(data){
        dataslice=qtvarc %>%
          dplyr::filter(Q>=data$Q,
                        index<data$index)
        if(nrow(dataslice)==0){return(NA_real_)}
        indexl=dataslice %>%
          dplyr::summarise(index=max(index)) %>%
          dplyr::pull(index)
        if(indexl==data$index-1){return(data$Time)}
        datatarget=qtvarc %>%
          dplyr::filter(index %in% c(indexl,indexl+1)) %>%
          dplyr::select(Time,Q)
        lasttime=approx(datatarget$Q,datatarget$Time,xout=data$Q)$y
        return(lasttime)
      }
      # Apply this function to all individual datasets
      # then unnest them
      # and calculate time difference T_Q (in days)
      # as well as Slope of discharge in the last 5 minutes, S
      result=result %>%
        dplyr::mutate(lasttime=purrr::map_dbl(data,.f=f_lasttime))%>%
        dplyr::mutate(lasttime=as.POSIXct(lasttime,
                                          origin="1970-01-01 00:00:00",
                                          tz="UTC")) %>%
        tidyr::unnest(cols=c(data)) %>%
        dplyr::mutate(T_Q=as.numeric(difftime(Time,lasttime,units="days")))
      # if complete_T_Q is TRUE,
      # missing values for T_Q(t) are replaced with time between t and beginning of Qdata
      # T_Q(t) then does not correspond to "time since such a discharge was observed"
      # but to "minimum time since such a discharge was observed"
      if(complete_T_Q){
        mintime=min(result$Time)
        result=result %>%
          dplyr::mutate(T_Q=case_when(is.na(T_Q)~as.numeric(difftime(Time,mintime,units="days")),
                                      !is.na(T_Q)~T_Q))
      }
      result=result%>%
        dplyr::mutate(T5minbef=Time-60*5) %>%
        dplyr::mutate(S=Q-approx(qtvarc$Time,
                                 qtvarc$Q,
                                 xout=T5minbef)$y) %>%
        dplyr::mutate(rT_Q=sqrt(T_Q)) %>%
        dplyr::select(site,station,Time,Q,T_Q,S,rT_Q,id) %>%
        dplyr::ungroup() %>%
        dplyr::select(-id)
  return(result)
}


