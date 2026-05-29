# tf_mv columns inside tibbles / data.frames, plus the standard tidyverse
# verbs (filter / mutate / summarize / arrange / slice / group_by / nest /
# left_join / bind_rows / pull / distinct). These all exercise vctrs (vec_proxy
# / vec_restore / vec_slice / vec_c / vec_ptype2 / vec_cast) one way or
# another; tf_mv was designed so the dplyr/tidyr surface "just works" via
# component-wise dispatch on the proxy.

skip_if_not_installed("tibble")
skip_if_not_installed("dplyr")
skip_if_not_installed("tidyr")

mk_data <- function(n = 4, seed = 1) {
  set.seed(seed)
  tibble::tibble(
    id = seq_len(n),
    g = rep(c("A", "B"), length.out = n),
    path = tfd_mv(list(x = tf_rgp(n), y = tf_rgp(n)))
  )
}

test_that("tf_mv works as a tibble column (constructor, ptype, length)", {
  tbl <- mk_data()
  expect_identical(nrow(tbl), 4L)
  expect_s3_class(tbl$path, "tfd_mv")
  expect_length(tbl$path, 4L)
  expect_match(vctrs::vec_ptype_abbr(tbl$path), "tfd_mv")
})

test_that("dplyr::filter on a tibble keeps the tf_mv column aligned", {
  tbl <- mk_data(6)
  sub <- dplyr::filter(tbl, g == "B")
  expect_identical(nrow(sub), 3L)
  expect_length(sub$path, 3L)
  expect_s3_class(sub$path, "tfd_mv")
  expect_identical(names(tf_components(sub$path)), c("x", "y"))
  expect_equal(sub$path, tbl$path[tbl$g == "B"])
})

test_that("dplyr::filter accepts a tf_mv-derived predicate", {
  tbl <- mk_data(6)
  threshold <- mean(tf_arclength(tbl$path))
  sub <- dplyr::filter(tbl, tf_arclength(path) > threshold)
  expect_true(all(tf_arclength(sub$path) > threshold))
})

test_that("dplyr::mutate can derive scalar and tfd columns from a tf_mv", {
  tbl <- mk_data()
  m <- dplyr::mutate(
    tbl,
    arclen = tf_arclength(path),
    speed = tf_speed(path),
    path_scaled = 2 * path
  )
  expect_type(m$arclen, "double")
  expect_length(m$arclen, nrow(tbl))
  expect_s3_class(m$speed, "tfd")
  expect_length(m$speed, nrow(tbl))
  expect_s3_class(m$path_scaled, "tfd_mv")
  expect_identical(tf_ncomp(m$path_scaled), 2L)
  expect_equal(m$arclen, tf_arclength(tbl$path))
  expect_equal(m$speed, tf_speed(tbl$path))
  expect_equal(m$path_scaled$x, 2 * tbl$path$x)
  expect_equal(m$path_scaled$y, 2 * tbl$path$y)
})

test_that("dplyr::summarize returns a length-1 tfd_mv via mean()", {
  tbl <- mk_data(6)
  s <- dplyr::summarize(
    tbl,
    mean_path = mean(path),
    total_len = sum(tf_arclength(path))
  )
  expect_s3_class(s$mean_path, "tfd_mv")
  expect_length(s$mean_path, 1L)
  expect_type(s$total_len, "double")
  expect_equal(s$mean_path, mean(tbl$path))
  expect_equal(s$total_len, sum(tf_arclength(tbl$path)))
})

test_that("group_by + summarize yields one tfd_mv entry per group", {
  tbl <- mk_data(6)
  gs <- tbl |>
    dplyr::group_by(g) |>
    dplyr::summarize(m = mean(path), n_curves = dplyr::n())
  expect_identical(nrow(gs), 2L)
  expect_s3_class(gs$m, "tfd_mv")
  expect_length(gs$m, 2L)
  expect_equal(gs$m[gs$g == "A"], mean(tbl$path[tbl$g == "A"]))
  expect_equal(gs$m[gs$g == "B"], mean(tbl$path[tbl$g == "B"]))
  expect_equal(gs$n_curves, c(3L, 3L))
})

test_that("arrange / slice keep the tf_mv column row-aligned", {
  tbl <- mk_data(6)
  out <- tbl |> dplyr::arrange(dplyr::desc(id)) |> dplyr::slice(1:3)
  expect_identical(out$id, c(6L, 5L, 4L))
  expect_length(out$path, 3L)
  # the path that ended up at row 1 of out is the same as the path at row 6
  # of the original tibble
  expect_equal(
    tf_evaluations(out$path[1])[[1]],
    tf_evaluations(tbl$path[6])[[1]]
  )
})

test_that("bind_rows concatenates a tf_mv column via vctrs c()", {
  tbl <- mk_data(4)
  br <- dplyr::bind_rows(tbl, tbl)
  expect_identical(nrow(br), 8L)
  expect_length(br$path, 8L)
  expect_s3_class(br$path, "tfd_mv")
  expect_equal(br$path[1:4], tbl$path)
  expect_equal(br$path[5:8], tbl$path)
})

test_that("left_join preserves tf_mv column and fills NA rows correctly", {
  tbl <- mk_data(4)
  extra <- tibble::tibble(id = c(1, 3), label = c("a", "c"))
  lj <- dplyr::left_join(tbl, extra, by = "id")
  expect_identical(nrow(lj), 4L)
  expect_length(lj$path, 4L)
  expect_equal(lj$label, c("a", NA, "c", NA))
  expect_equal(lj$path, tbl$path)
})

test_that("pull returns the tf_mv vector unchanged", {
  tbl <- mk_data()
  p <- dplyr::pull(tbl, path)
  expect_s3_class(p, "tfd_mv")
  expect_length(p, nrow(tbl))
  expect_equal(p, tbl$path)
})

test_that("distinct on a key column keeps tf_mv aligned", {
  tbl <- mk_data(4)
  d <- dplyr::distinct(tbl, g, .keep_all = TRUE)
  expect_identical(nrow(d), 2L)
  expect_length(d$path, 2L)
  expect_equal(d$path, tbl$path[c(1, 2)])
})

test_that("tidyr nest / unnest round-trip a tf_mv column", {
  tbl <- mk_data(4)
  nst <- tidyr::nest(tbl, data = c(id, path))
  expect_identical(nrow(nst), 2L)
  # each nested tibble carries the tfd_mv intact
  expect_s3_class(nst$data[[1]]$path, "tfd_mv")
  unn <- tidyr::unnest(nst, data)
  expect_identical(nrow(unn), nrow(tbl))
  expect_s3_class(unn$path, "tfd_mv")
  expect_equal(dplyr::arrange(unn, id)$path, tbl$path)
})

test_that("rowwise mutate with a tf_mv column works", {
  tbl <- mk_data()
  rw <- tbl |>
    dplyr::rowwise() |>
    dplyr::mutate(len = tf_arclength(path)) |>
    dplyr::ungroup()
  expect_equal(rw$len, tf_arclength(tbl$path))
})
