gait <- datasets::gait
knee_angle <- gait[,, "Knee Angle"]
knee_angle <- tfd(t(knee_angle))
hip_angle <- gait[,, "Hip Angle"]
hip_angle <- tfd(t(hip_angle))
stopifnot(length(knee_angle) == length(hip_angle))
gait <- vctrs::data_frame(
  subject = seq_len(length(knee_angle)),
  knee_angle = knee_angle,
  hip_angle = hip_angle
)

usethis::use_data(gait, overwrite = TRUE)
