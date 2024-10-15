x <- tf_rgp(3, nugget = 1 / 10)

test <- list(
  reg = x,
  irr = tf_sparsify(x),
  tfb1 = tfb(x),
  tfb2 = tfb(x, k = 11),
  tfb3 = tfb(exp(x), family = "Gamma"),
  pc1 = tfb_fpc(x)
)
test_mat <- lapply(test, as.matrix)
ops <- c("+", "-", "*", "/", "^", "%%")

# fun-scalar
for (n in names(test)) {
  for (op in ops) {
    glue::glue("---- \n\n {n}{op}{2}:\n") |> cat()
    res <- NULL
    try(res <- vec_arith(op, test[[n]], 2))
    if (!is.null(res)) {
      res_mat <- NULL
      try(res_mat <- vec_arith(op, test_mat[[n]], 2))
      all.equal(res_mat, as.matrix(res), check.attributes = FALSE) |>
        print()
      plot(res, main = glue::glue("{n}{op}{2}"))
      lines(tfd(res_mat), col = 2)
    }
    browser()
  }
}


# scalar-fun
for (n in names(test)) {
  for (op in ops) {
    glue::glue("---- \n\n 2{op}{n}:\n") |> cat()
    res <- NULL
    try(res <- vec_arith(op, 2, test[[n]]))
    if (!is.null(res)) {
      res_mat <- NULL
      try(res_mat <- vec_arith(op, 2, test_mat[[n]]))
      all.equal(res_mat, as.matrix(res), check.attributes = FALSE) |>
        print()
      plot(res, main = glue::glue("2{op}{n}"))
      lines(tfd(res_mat), col = 2)
    }
    browser()
  }
}

# fun-fun (same type)
for (n1 in names(test)) {
    for (op in ops) {
      glue::glue("---- \n\n {n1}{op}{n1}:\n") |> cat()
      res <- NULL
      try(res <- vec_arith(op, test[[n1]], test[[n1]]))
      if (!is.null(res)) {
        res_mat <- vec_arith(op, test_mat[[n1]], test_mat[[n1]])
        all.equal(res_mat, as.matrix(res), check.attributes = FALSE) |>
          print()
        plot(res, main = glue::glue("{n1}{op}{n1}"))
        lines(tfd(res_mat), col = 2)
      }
      browser()
    }
}

# fun-fun (different types)
for (n1 in names(test)) {
  for (n2 in names(test)) {
    if(n1 == n2) next()
    for (op in ops) {
        glue::glue("---- \n\n {n1}{op}{n2}:\n") |> cat()
        res <- NULL
        try(res <- vec_arith(op, test[[n1]], test[[n2]]))
        if (!is.null(res)) {
          res_mat <- vec_arith(op, test_mat[[n1]], test_mat[[n2]])
          all.equal(res_mat, as.matrix(res), check.attributes = FALSE) |>
            print()
          plot(res, main = glue::glue("{n1}{op}{n2}"))
          lines(tfd(res_mat), col = 2)
        }
    }
  }
}
