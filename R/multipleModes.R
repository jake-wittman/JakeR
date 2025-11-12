multipleModes <- function(.x) {
  # If multiple modes exist, take the average of the modes
  modes <- DescTools::Mode(.x, na.rm = TRUE)
  if (length(modes) > 1) {
    mean(modes)
  } else {
    modes[1] # Strip the attribute of frequency so it just returns the value
  }
}
