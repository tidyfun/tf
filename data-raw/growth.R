growth <- fda::growth
growth_female <- tf::tfd(t(growth$hgtf), arg = growth$age)
growth_male <- tf::tfd(t(growth$hgtm), arg = growth$age)

growth_female <- vctrs::data_frame(gender = "female", growth = growth_female)
growth_male <- vctrs::data_frame(gender = "male", growth = growth_male)
growth <- vctrs::vec_rbind(growth_female, growth_male)
growth$gender <- as.factor(growth$gender)

usethis::use_data(growth, overwrite = TRUE)
