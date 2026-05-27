# dynamically xports format_glimpse.tf if {pillar} is available
# (see ?s3_register)
# nocov start
.onLoad <- function(libname, pkgname) {
  vctrs::s3_register("pillar::format_glimpse", "tf")
  vctrs::s3_register("pillar::format_glimpse", "tf_mv")

  invisible()
}
# nocov end
