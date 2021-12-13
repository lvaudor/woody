#' Interpolates Qdata to add descriptor variables to Wdata
#' @export
#' @param Wdata wood data
#' @param Qdata discharge data
#' @return Wdata completed with variables Q, T_Q, and S and rT_Q
complete_Wdata_with_Qdata=function(Wdata,Qdata, newvars=c("Q","T_Q","S","rT_Q")){
    result = Wdata
    for (var in newvars) {
        varfactor = class(Qdata[[var]]) != "numeric"
        if (varfactor) {
            levs = levels(as.factor(Qdata[[var]]))
            Qdata[[var]] = as.numeric(Qdata[[var]])
        }
        result[[var]] = approx(Qdata$Time,
                               Qdata[[var]],
                               xout = Wdata$Time)$y
        if (varfactor) {
            result[[var]] = factor(levs[result[[var]]],
                                   levels=levs)
        }
    }
    return(result)
}
