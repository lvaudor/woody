#' summarise Qdata to a period
#' @param data data to summarise
#' @param Adata the annotation times (if available)
#' @param sigma the residual error in the estimates of Ypred
#' @export
#' @return summarised data
summarise_Qdata=function(Qdata,sigma){
  result=Qdata %>%
        mutate(Time = lubridate::floor_date(Time, "hour")) %>%
        group_by(site,station,Time) %>%
        summarise(n=n(),
                  Q=mean(Q),
                  rT_Q=mean(rT_Q),
                  S=mean(S),
                  T_Q=mean(T_Q),
                  Y_pred=mean(Y_pred), # 3600/W follows a log-normal law of mean mean(Ypred)
                  N_pred=exp(log(3600)-Y_pred+(sigma^2)/2),
                  N_pred=3600/N_pred) %>%
    unique() %>%
    ungroup()
  return(result)
}
