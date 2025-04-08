# 
# Function to calculate saturation vapor pressure (SVP), as described in Gill 1982
# Returns an SVP value in kPa
# 

# Arguments
# --------- #
# T: an air temperature value, in degrees C


# Function to calculate SVP
calc_svp <- function(T) {
  0.6108 * exp((17.27 * T) / (T + 237.3))
}