growth <- fda::growth
growth_female <- tfd(t(growth$hgtf), arg = growth$age)
growth_male <- tfd(t(growth$hgtm), arg = growth$age)

growth_female <- tibble::tibble(gender = "female", growth = growth_female)
growth_male <- tibble::tibble(gender = "male", growth = growth_male)
growth <- vctrs::vec_rbind(growth_female, growth_male)
growth$gender <- as.factor(growth$gender)
growth <- as.data.frame(growth)

usethis::use_data(growth, overwrite = TRUE)
