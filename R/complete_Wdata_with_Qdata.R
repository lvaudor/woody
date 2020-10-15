#' Interpolates Qdata to add discharge variables to Wdata
#' @export
#' @param Wdata
#' @param Qdata
#' @return Wdata completed with variables Q, T_Q, and S
#' @examples
complete_Wdata_with_Qdata=function(Wdata,Qdata){
    Wdata <- Wdata %>%
      dplyr::mutate(Q=approx(Qdata$Time,Qdata$Q,xout=Wdata$Time)$y,
                    T_Q=approx(Qdata$Time,Qdata$T_Q,xout=Wdata$Time)$y,
                    S=Q-approx(Qdata$Time,Qdata$Q,xout=Wdata$Time-5*60)$y) %>%
      dplyr::filter(!is.na(Time))
    return(Wdata)
}
