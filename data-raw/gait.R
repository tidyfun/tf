gait <- fda::gait
knee_angle <- gait[,, "Knee Angle"]
knee_angle <- tf::tfd(t(knee_angle))
hip_angle <- gait[,, "Hip Angle"]
hip_angle <- tf::tfd(t(hip_angle))
stopifnot(length(knee_angle) == length(hip_angle))
gait <- vctrs::data_frame(
  subject_id = seq_along(knee_angle),
  knee_angle = knee_angle,
  hip_angle = hip_angle
)

usethis::use_data(gait, overwrite = TRUE)

# the same trajectories as a single vector-valued (hip, knee) curve per subject
gait_mv <- vctrs::data_frame(
  subject_id = gait$subject_id,
  joint_angle = tf::tfd_mv(list(hip = gait$hip_angle, knee = gait$knee_angle))
)

usethis::use_data(gait_mv, overwrite = TRUE)
