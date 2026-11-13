trial_arm_colors <- function(arms) {
  palette <- c(
    "Screen Failure" = "#64748b",
    "Placebo" = "#3376a6",
    "Xanomeline High Dose" = "#785ba6",
    "Xanomeline Low Dose" = "#278279"
  )
  other_arms <- sort(setdiff(arms, names(palette)))
  if (length(other_arms)) {
    palette[other_arms] <- grDevices::hcl.colors(length(other_arms), "Dark 3")
  }
  palette[arms]
}

domain_sidebar <- function(...) {
  bslib::sidebar(width = 250, open = "always", class = "domain-filters", ...)
}
