.cpp_helpfun_breslow <- function(etahat, numerator) {
    .Call('gamboostMSM_cpp_helpfun_breslow', PACKAGE = 'gamboostMSM', etahat, numerator)
}

.cpp_helpfun_breslow_risk_sets <- function(risksetsR, fromR, fromqR) {
    .Call('gamboostMSM_cpp_helpfun_breslow_risk_sets', PACKAGE = 'gamboostMSM', risksetsR, fromR, fromqR)
}

.cpp_helpfun_log_pl <- function(etahat, numerator) {
    .Call('gamboostMSM_cpp_helpfun_log_pl', PACKAGE = 'gamboostMSM', etahat, numerator)
}

.cpp_helpfun_risk_sets_wrt_times <- function(timeslR, timesrR) {
    .Call('gamboostMSM_cpp_helpfun_risk_sets_wrt_times', PACKAGE = 'gamboostMSM', timeslR, timesrR)
}

.cpp_helpfun_risk_sets_wrt_times_and_states <- function(risksetsR, fromR, toR, fromqR, toqR) {
    .Call('gamboostMSM_cpp_helpfun_risk_sets_wrt_times_and_states', PACKAGE = 'gamboostMSM', risksetsR, fromR, toR, fromqR, toqR)
}

.cpp_helpfun_z_i <- function(etahat, numerator, denominator) {
    .Call('gamboostMSM_cpp_helpfun_z_i', PACKAGE = 'gamboostMSM', etahat, numerator, denominator)
}
