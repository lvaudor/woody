#' summarise Qdata to a period
#' @param data data to summarise
#' @param Adata the annotation times (if available)
#' @param sigma_coeffs the coefficients providing sigma (residual standard deviation) with sigma=coef[1]*mu +coef[2]
#' @export
#' @return summarised data
summarise_Qdata=function(Qdata,sigma_coeffs){
  result=Qdata %>%
        dplyr::mutate(Time = lubridate::floor_date(Time, "hour")) %>%
        dplyr::group_by(site,station,Time) %>%
        dplyr::summarise(n=n(),
                  Q=mean(Q),
                  rT_Q=mean(rT_Q),
                  S=mean(S),
                  T_Q=mean(T_Q),
                  mu=mean(Y_pred),
                  Y_pred=mu) %>% # 3600/W follows a log-normal law of mean mu=mean(Ypred),
    mutate(sigma=sigma_coeffs[1]*mu+sigma_coeffs[2],
           N_pred=exp(mu-(sigma^2)/2)) %>%
    unique() %>%
    dplyr::ungroup()
  return(result)
}
