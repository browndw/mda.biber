# NSE variables handled with .data$ syntax for most cases
# Some variables still need globalVariables for specific cases
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("x", "y"))  # Used in temporary data.frame creation
}
