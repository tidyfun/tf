library("tf")
options(width = 60)

t <- seq(0, 1, l = 301)
sincos <- purrr::map(1:5, \(i) cos(2 * pi * i * t)) |> tfd()
sincos

tfb(sincos)

sincos |> tf_sparsify()

sincos[1] <- NA
sincos

options(width = 40, digits = 2)
sincos

tfb(tidyfun::dti_df$rcst[1:10])
tidyfun::chf_df$activity[1:10]

# --------

tf:::spark_rep_tf(sincos)
tf:::spark_rep_tf(sincos, bins = 30)

act <- tidyfun::chf_df$activity[1:20]
plot(act[1:5], type = "lasagna")
tf:::spark_rep_tf(act[1:5])
tf:::spark_rep_tf(act[1:5], bins = 50)

rcst_b <- tfb(
  tidyfun::dti_df$rcst[1:10],
  arg = seq(0, 1, l = 201),
  bs = "ps",
  m = c(2, 1)
)
plot(rcst_b[1:3], col = 1:5)
tf:::spark_rep_tf(rcst_b[1:3])
tf:::spark_rep_tf(rcst_b[1:3], bins = 30)

microbenchmark::microbenchmark(
  string = tf:::string_rep_tf(sincos),
  nobin = tf:::spark_rep_tf(sincos),
  bin30 = tf:::spark_rep_tf(sincos, bins = 30),
  bin100 = tf:::spark_rep_tf(sincos, bins = 100)
) # x1, x15


microbenchmark::microbenchmark(
  string = tf:::string_rep_tf(act),
  nobin = tf:::spark_rep_tf(act),
  bin30 = tf:::spark_rep_tf(act, bins = 30),
  bin100 = tf:::spark_rep_tf(act, bins = 100)
) # x6, x6


microbenchmark::microbenchmark(
  string = tf:::string_rep_tf(rcst_b),
  nobin = tf:::spark_rep_tf(rcst_b),
  bin30 = tf:::spark_rep_tf(rcst_b, bins = 30),
  bin100 = tf:::spark_rep_tf(rcst_b, bins = 100)
) # x10
