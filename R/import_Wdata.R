#' Imports data regarding occurrence of wood from files under path (path>1 directory=1 event>all logs of the event)
#' @param path the path towards the events directory
#' @importFrom magrittr %>%
#' @export
#' @return Wdata
#' @examples
import_Wdata=function(path){
  event_dir=paste0(path,list.files(path))

  Wdata=tibble::tibble(event= paste0("event_",1:length(event_dir)),
                       event_dir=event_dir,
                       wood_file=event_dir %>%
                         purrr::map(list.files, full.names=TRUE)) %>%
    tidyr::unnest(cols=c(wood_file)) %>%
    dplyr::mutate(data=wood_file %>%
                    purrr::map(woody:::read_1_wood_log_file)) %>%
    tidyr::unnest(cols=c(data)) #%>%
    #dplyr::mutate(Date=lubridate::round_date(Time,"day"))
  return(Wdata)
}
