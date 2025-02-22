# dynamically xports format_glimpse.tf if {pillar} is available
# (see ?s3_register)
# nocov start
.onLoad <- function(lib, pkg) {
  vctrs::s3_register("pillar::format_glimpse", "tf")

  invisible()
}
# nocov end
