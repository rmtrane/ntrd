#!/usr/bin/env Rscript
# Run test coverage locally and upload to Codecov
#
# Usage:
#   Rscript scripts/coverage.R           # run coverage and upload
#   Rscript scripts/coverage.R --no-upload  # run coverage only (view locally)
#
# Requirements:
#   install.packages(c("covr", "httr2"))
#   Set CODECOV_TOKEN env var (e.g. in .Renviron)

args <- commandArgs(trailingOnly = TRUE)
upload <- !("--no-upload" %in% args)

message("Running test coverage...")
cov <- covr::package_coverage()

message("\nCoverage summary:")
print(cov)

if (upload) {
  token <- Sys.getenv("CODECOV_TOKEN")
  if (nzchar(token)) {
    message("\nUploading to Codecov...")
    covr::codecov(coverage = cov, token = token)
    message("Done.")
  } else {
    warning(
      "CODECOV_TOKEN not set. Skipping upload.\n",
      "Set it in .Renviron or export it in your shell."
    )
  }
} else {
  message("\nSkipping upload (--no-upload flag set).")
  message("To view a detailed report, run in R:")
  message('  cov <- covr::package_coverage()')
  message('  covr::report(cov)')
}
