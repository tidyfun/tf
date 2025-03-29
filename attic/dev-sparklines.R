library("tf")
options(width = 80)

t <- seq(0, 1, l = 301)
sincos <- purrr::map(1:5, \(i) cos(2 * pi * i * t)) |> tfd(arg = t)
sincos

tfb(sincos)

sincos |> tf_sparsify()

sincos[1] <- NA
sincos

options(width = 40)
sincos

tfb(tidyfun::dti_df$rcst[1:10])
# if names are (too) long, rightmost edges get cut off :(
tidyfun::chf_df$activity[1:10]


# check: sparklines show values rel. to *global* min/max:
min_max <- tfd(rep(1, 20)) / 2^seq(-3, 3, l = 10)
print(min_max, n = 3)
print(min_max, n = 10)
print(rev(min_max), n = 3)

# --------

options(width = 60)

act <- tidyfun::chf_df$activity[1:20]
tf:::spark_rep_tf(act[1:2], bins = 2000)
tf:::spark_rep_tf(act[1:2], bins = 100)
tf:::spark_rep_tf(act[1:2], bins = 50)
tf:::spark_rep_tf(act[1:2], bins = 10)

rcst_b <- tfb(
  tidyfun::dti_df$rcst[1:10],
  arg = seq(0, 1, l = 201),
  bs = "ps",
  m = c(2, 1)
)
plot(rcst_b[1:3], col = 1:5)
tf:::spark_rep_tf(rcst_b[1:3], bins = 100)
tf:::spark_rep_tf(rcst_b[1:3], bins = 30)

# also looks good in glimpse / pillar:
tibble::glimpse(tidyfun::chf_df)
tidyfun:::pillar_shaft.tf(tidyfun::chf_df$activity)


# ---------

microbenchmark::microbenchmark(
  string = tf:::string_rep_tf(sincos),
  nobin = tf:::spark_rep_tf(sincos),
  bin30 = tf:::spark_rep_tf(sincos, bins = 30),
  bin100 = tf:::spark_rep_tf(sincos, bins = 100)
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

#------------------------------------------------------------------------------

# !! not working (well) in print.tbl_df:

# does work in principle:
tibble::tibble(a = 1:10, b = tf_rgp(10, arg = 5))
# but not for real data:
tidyfun::chf_df[, 7:8]
tidyfun::chf_df[, 8:7]

# reason (probably): since the default width of the format.tf output is
# options()$width, pillar:::ctl_colonnade always detects overflowing lines for
# the table body and omits these columns (and all columns after them):
options(width = 1500)
tidyfun::chf_df[, 7:8]

# !! need to figure out a way to use more aggressive binning when called from pillar --
# this seems to not be calling tidyfun:::pillar_shaft.tf but format.tf for some reason ...
