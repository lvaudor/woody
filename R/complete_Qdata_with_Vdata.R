complete_Qdata_with_Vdata_onesite=function(Qdata,Vdata){
  result=Qdata %>%
    group_by(Date,T_Q) %>%
    nest(Qdata=c(-Date,-T_Q)) %>%
    mutate(Vdata=purrr::map2(.x=Date,.y=T_Q,.f=calc_V,Vdata)) %>%
    unnest(cols=c(Date,T_Q,Qdata,Vdata)) %>%
    ungroup()
  return(result)
}

complete_Qdata_with_Vdata=function(Qdata,Vdata){
  result=Qdata %>%
    group_by(site) %>%
    nest(Qdata=-site) %>%
    mutate(Vdata=purrr::map(site,~filter(Vdata,site==.x))) %>%
    mutate(Qdatac=purrr::map2(Qdata,
                              Vdata,
                              complete_Qdata_with_Vdata_onesite))  %>%
    unnest(cols=c(site,Qdatac)) %>%
    ungroup()
  return(result)
}
