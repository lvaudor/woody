#' Imports data regarding occurrence of wood from files under path (path>1 directory=1 event>all logs of the event)
#' @param path the path towards the events directory
#' @param site the name of the site considered
#' @param min_length the minimum length for a wood piece to be included in the dataset. Defaults to NA (no filtering).
#' @param sample_length whether to sample the wood pieces with no length provided, according to the proportion of pieces with Length>min_length. Defaults to FALSE.
#' @importFrom magrittr %>%
#' @export
#' @return Wdata
#' @example
#' Wdata=import_Wdata("data-raw/wood_data_Ain", site="Ain", min_length=NA, sample_length=FALSE)
#' Wdata=import_Wdata("data-raw/wood_data_Allier", site="Allier", min_length=NA, sample_length=FALSE)

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
          dplyr::mutate(maDate = lubridate::date(.$Time)) %>%
          dplyr::mutate(Date=maDate) %>%
          dplyr::select(-maDate) %>%
          tidyr::unite("sitevent",site,event,remove=FALSE) %>%
          dplyr::select(site,event,sitevent,everything())
    if(!is.na(min_length)){
      Wdata_lp=Wdata %>%
        dplyr::mutate(length_provided=!is.na(Length))
      Wdata_li=Wdata_lp %>%
        dplyr::filter(length_provided) %>%
        dplyr::filter(Length > min_length)
      if(sample_length){
      # do not remove all rows with missing length
          prop=nrow(Wdata_li)/nrow(Wdata_lp)
          Wdata_ls=Wdata_lp %>%
            dplyr::filter(!length_provided)
          nslice=floor(nrow(Wdata_ls)*prop)
          Wdata_ls=Wdata_ls %>%
            dplyr::slice(n=nslice) %>%
            dplyr::select(-length_provided)
          Wdata=dplyr::bind_rows(Wdata_li,Wdata_ls) %>%
            dplyr::arrange(Time)
      }
  }
  return(Wdata)
}
