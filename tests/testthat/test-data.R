test_that("gait_mv bundles the gait hip and knee angles per subject (#300)", {
  expect_s3_class(gait_mv$joint_angle, "tfd_mv")
  expect_equal(nrow(gait_mv), nrow(gait))
  expect_identical(gait_mv$subject_id, gait$subject_id)
  expect_identical(attr(gait_mv$joint_angle, "comp_names"), c("hip", "knee"))
  expect_identical(gait_mv$joint_angle$hip, gait$hip_angle)
  expect_identical(gait_mv$joint_angle$knee, gait$knee_angle)
})
