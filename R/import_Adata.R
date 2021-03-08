#' Imports data regarding annotation times
#' @param path the path towards the annotation times file
#' @param site the name of the site considered
#' @importFrom magrittr %>%
#' @export
#' @return Adata
#' @example Adata=import_Adata("data/annot_times_Ain.csv",site="Ain")
import_Adata=function(path, site){
  Adata=readr::read_csv(path) %>%
    dplyr::mutate(start=lubridate::dmy_hms(start),
                  end=lubridate::dmy_hms(end))

  cut_obs_times=function(start,end){
    Time=floor_date(start, "hour")
    Timend=floor_date(end,"hour")
    if(Time!=Timend){
      seqtimes=seq(from=Time+hours(1),to=Timend,by="hours")
      start=c(start,seqtimes)
      end=c(seqtimes,end)
    }
    result=tibble::tibble(start=start,
                          end=end) %>%
      dplyr::mutate(Time=floor_date(start,"hour"))
    return(result)
  }
  result=purrr::map2_df(Adata$start,
                        Adata$end,
                        cut_obs_times) %>%
    dplyr::mutate(obs_duration=as.numeric(difftime(end,start,units="hours"))) %>%
    dplyr::group_by(Time) %>%
    dplyr::summarise(obs_duration=sum(obs_duration),
                     .groups="drop")
return(result)
}
