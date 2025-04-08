# 
# Function to calculate actual vapor pressure (AVP)
# Returns an AVP value in kPa

# Arguments
# --------- #
# SVP: a saturation vapor pressure value (kPA) (see calc_svp)
# RH: a relative humidity value
# P_atm: an atmospheric pressure value

calc_avp <- function(SVP, RH, P_atm) {
  (RH / 100) * SVP * (P_atm / 101.3)
}
