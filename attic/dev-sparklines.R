devtools::load_all("~/fda/tidyfun-pkgs/tf")
options(width = 80)

t <- seq(0, 1, l = 101)
cosine <- purrr::map(1:5, \(i) cos(2 * pi * i * t) * i) |> tfd(arg = t)
cosine

format(cosine, sparkline = FALSE)
format(cosine, bins = 5)
format(cosine, bins = 40)


tfb(cosine)
cosine |> tf_sparsify()


(rcst_b <- tfb(tidyfun::dti_df$rcst[1:10], verbose = FALSE))
(act <- tidyfun::chf_df$activity)

# --------

# also looks good in glimpse / pillar / print_tbl:
tibble::glimpse(tidyfun::chf_df)
tidyfun:::pillar_shaft.tf(tidyfun::chf_df$activity)
tidyfun::chf_df[, 5:8]

# ---------

#! 2-25 times slower, still just microseconds so who cares:

microbenchmark::microbenchmark(
  string = tf:::string_rep_tf(cosine),
  nobin = tf:::spark_rep_tf(cosine),
  bin30 = tf:::spark_rep_tf(cosine, bins = 30),
  bin100 = tf:::spark_rep_tf(cosine, bins = 100)
)


microbenchmark::microbenchmark(
  string = tf:::string_rep_tf(act),
  nobin = tf:::spark_rep_tf(act),
  bin30 = tf:::spark_rep_tf(act, bins = 30),
  bin100 = tf:::spark_rep_tf(act, bins = 100)
)

microbenchmark::microbenchmark(
  string = tf:::string_rep_tf(rcst_b),
  nobin = tf:::spark_rep_tf(rcst_b),
  bin30 = tf:::spark_rep_tf(rcst_b, bins = 30),
  bin100 = tf:::spark_rep_tf(rcst_b, bins = 100)
)

#-------------

#! very non-equidistant grids --> sparklines can mislead
cosine_2 <- tfd(cosine, arg = t^3)
format(cosine_2, bins = 40)
format(cosine, bins = 40)
