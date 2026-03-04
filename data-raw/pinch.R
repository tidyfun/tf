pinch <- tf::tfd(t(fda::pinchraw), arg = fda::pinchtime)

usethis::use_data(pinch, overwrite = TRUE)
