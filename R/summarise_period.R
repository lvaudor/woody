#' summarise data to a period
#' @param data data to summarise
#' @param period period (can be "hour","day",...)
#' @param type of data either "Wdata" or "Qdata"
#' @export
#' @return summarised data
#' data=tib_WpQc_h$Wdata[[1]];type="Wdata";period="hour";Adata=tib_WpQc_h$Adata[[1]]
#'
summarise_period=function(data, type="Wdata",period="hour", Adata=NULL){
  if(type=="Wdata"){
      data_grouped=data %>%
        mutate(Time = lubridate::floor_date(Time, "hour")) %>%
        group_by(site,event,sitevent,Date,Time) %>%
        dplyr::summarise(Nobs=n()+1,
                         obs_wt=sum(W)/3600,
                         mu=mean(Y),
                         sigma=sd(Y))%>%
        dplyr::mutate(Nobsc=Nobs/obs_wt)

      if(!is.null(Adata)){
        result=data_grouped %>%
          dplyr::left_join(Adata,by=c("Time"))  %>%
          dplyr::mutate(Nobsc=Nobs/obs_duration)
      }
  }
  if(type=="Qdata"){
      result=data %>%
        mutate(Time = lubridate::floor_date(Time, "hour")) %>%
        group_by(site,station,Time) %>%
        summarise(n=n(),
                  Q=mean(Q),
                  rT_Q=mean(rT_Q),
                  S=mean(S),
                  T_Q=mean(T_Q),
                  mu=mean(Ypred), # 3600/W follows a log-normal law of mean mean(Ypred)
                  sigma=sd(Ypred),
                  espW=exp(log(3600)-mu+(sigma^2)/2),
                  Npred=3600/espW,
                  Npredanc=exp(mu+(sigma^2)/2))
  }

  # mu=log(3600)-meanY
  # sigma=2
  # espN=3600/exp(mu+sigma^2/2)
#   if("Vmax" %in% colnames(data)){
#     data=data %>%
#       mutate(Vmax=max(Vmax)) %>%
#       mutate(Vsum=mean(Vsum))
#   }
  result=result %>%
    unique() %>%
    ungroup()
  return(result)
}
