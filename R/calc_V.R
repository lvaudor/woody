calc_V=function(Date_obs,T_Q_obs,Vdata_obs){
  if(is.na(T_Q_obs)){
    result=tibble(Vmax=NA_real_,
                  Vmean=NA_real_,
                  Vsum=NA_real_)
    return(result)
  }
  Date_Q=Date_obs-days(round(T_Q_obs))
  result=Vdata_obs %>%
    filter(Date>=Date_Q,
           Date<=Date_obs) %>%
    summarise(Vmax=max(FFM),
              Vmean=mean(FFM),
              Vsum=sum(FFM))
  return(result)
}
