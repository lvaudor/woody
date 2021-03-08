#' Imports data regarding occurrence of wood from files under path (path>1 directory=1 event>all logs of the event)
#' @param path the path towards the events directory
#' @param site the name of the site considered
#' @importFrom magrittr %>%
#' @export
#' @return Wdata
#' @example Wdata=import_Wdata("data/wood_data_Allier", site="Allier")
import_Wdata=function(path,site=path){
    if(!stringr::str_detect(path,"\\/$")){path=stringr::str_c(path,"/")}
    event_dir=paste0(path,list.files(path))
        Wdata=tibble::tibble(event= paste0("event_",1:length(event_dir)),
                            event_dir=event_dir,
                            wood_file=event_dir %>%
                              purrr::map(list.files, full.names=TRUE)) %>%
        tidyr::unnest(cols=c(wood_file)) %>%
          dplyr::mutate(data=wood_file %>%
                          purrr::map(woody:::read_1_wood_log_file)) %>%
          tidyr::unnest(cols=c(data)) %>%
          dplyr::select(-event_dir,-wood_file) %>%
          dplyr::mutate(site=site) %>%
          dplyr::mutate(maDate = lubridate::date(Time)) %>%
          dplyr::mutate(Date=maDate) %>%
          dplyr::select(-maDate) %>%
          tidyr::unite("sitevent",site,event,remove=FALSE) %>%
          dplyr::select(site,event,sitevent,everything())
  return(Wdata)
}
