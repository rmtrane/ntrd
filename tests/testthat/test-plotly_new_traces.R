test_that("plotly_new_traces returns NULL when no std_ columns", {
  dt <- data.table::data.table(
    VISITDATE = as.Date("2024-06-15"),
    raw_MOCATOTS = 25
  )
  result <- plotly_new_traces(
    dt,
    visibility = visibility_defaults,
    legend_names = character(0),
    vars_colors = c()
  )
  expect_null(result)
})

test_that("plotly_new_traces returns NULL when all std_ columns are NA", {
  dt <- data.table::data.table(
    VISITDATE = as.Date("2024-06-15"),
    std_MOCATOTS = NA_real_,
    raw_MOCATOTS = 25
  )
  result <- plotly_new_traces(
    dt,
    visibility = visibility_defaults,
    legend_names = character(0),
    vars_colors = c()
  )
  expect_null(result)
})

test_that("plotly_new_traces returns list with expected structure", {
  prepared <- get_prepared_demo_data()

  # Get a single participant's data
  first_id <- prepared$NACCID[1]
  pt_data <- prepared[NACCID == first_id]

  # Select only VISITDATE and std_/raw_ columns
  std_raw_cols <- grep("^(std_|raw_)", colnames(pt_data), value = TRUE)
  sub_data <- pt_data[, c("VISITDATE", std_raw_cols), with = FALSE]

  # Build a vars_colors vector for all std_ variable names
  var_names <- unique(gsub(
    "^std_",
    "",
    grep("^std_", std_raw_cols, value = TRUE)
  ))
  # Use crosswalk translations where applicable
  display_names <- var_names
  display_names[display_names %in% names(crosswalk_translations)] <-
    crosswalk_translations[display_names[
      display_names %in% names(crosswalk_translations)
    ]]
  display_names <- unique(display_names)

  vars_colors <- setNames(
    rep("#000000", length(display_names)),
    display_names
  )

  result <- plotly_new_traces(
    sub_data,
    visibility = visibility_defaults,
    legend_names = character(0),
    vars_colors = vars_colors
  )

  expect_type(result, "list")
  expect_named(
    result,
    c("new_traces", "legend_names", "new_x_range", "new_y_range")
  )
  expect_type(result$new_traces, "list")
  expect_type(result$legend_names, "character")
  expect_s3_class(result$new_x_range, "Date")
  expect_length(result$new_x_range, 2)
  expect_type(result$new_y_range, "double")
  expect_length(result$new_y_range, 2)
})

test_that("plotly_new_traces traces carry legend names", {
  prepared <- get_prepared_demo_data()

  first_id <- prepared$NACCID[1]
  pt_data <- prepared[NACCID == first_id]

  std_raw_cols <- grep("^(std_|raw_)", colnames(pt_data), value = TRUE)
  sub_data <- pt_data[, c("VISITDATE", std_raw_cols), with = FALSE]

  var_names <- unique(gsub(
    "^std_",
    "",
    grep("^std_", std_raw_cols, value = TRUE)
  ))
  display_names <- var_names
  display_names[display_names %in% names(crosswalk_translations)] <-
    crosswalk_translations[display_names[
      display_names %in% names(crosswalk_translations)
    ]]
  display_names <- unique(display_names)

  vars_colors <- setNames(
    rep("#000000", length(display_names)),
    display_names
  )

  result <- plotly_new_traces(
    sub_data,
    visibility = visibility_defaults,
    legend_names = character(0),
    vars_colors = vars_colors
  )

  # Each trace should have a non-empty "name" field (the legend label)
  trace_names <- vapply(
    result$new_traces,
    \(tr) tr$name %||% "",
    character(1)
  )
  expect_true(all(nchar(trace_names) > 0))

  # Traces come in pairs (lines + markers), so names should repeat in pairs
  expect_true(length(trace_names) >= 2)
  expect_equal(length(trace_names) %% 2, 0)
})

test_that("plotly_new_traces combines all crosswalk pairs and uses diamond/cross markers", {
  # Build a synthetic 2-visit dataset from scratch. Visit 1 uses the older

  # instrument names (left side of crosswalk_translations), visit 2 uses
  # the newer names (right side).
  old_vars <- names(crosswalk_translations)
  new_vars <- unname(crosswalk_translations)

  # Columns: VISITDATE + std_/raw_ for every crosswalk variable
  std_old <- paste0("std_", old_vars)
  raw_old <- paste0("raw_", old_vars)
  std_new <- paste0("std_", new_vars)
  raw_new <- paste0("raw_", new_vars)

  all_cols <- c("VISITDATE", std_old, raw_old, std_new, raw_new)

  sub_data <- data.table::data.table(
    VISITDATE = as.Date(c("2010-06-15", "2020-06-15"))
  )

  # Initialise every score column to NA
  for (col in c(std_old, raw_old, std_new, raw_new)) {
    data.table::set(sub_data, j = col, value = NA_real_)
  }

  # Visit 1: old instruments have scores

  for (i in seq_along(old_vars)) {
    data.table::set(sub_data, i = 1L, j = std_old[i], value = -0.5 + i * 0.1)
    data.table::set(sub_data, i = 1L, j = raw_old[i], value = 20 + i)
  }

  # Visit 2: new instruments have scores
  for (i in seq_along(new_vars)) {
    data.table::set(sub_data, i = 2L, j = std_new[i], value = 0.5 - i * 0.1)
    data.table::set(sub_data, i = 2L, j = raw_new[i], value = 25 + i)
  }

  # vars_colors keyed by the *new* (target) variable names, which is what
  # plotly_new_traces uses after applying crosswalk_translations
  vars_colors <- setNames(
    rep("#000000", length(unique(new_vars))),
    unique(new_vars)
  )

  result <- plotly_new_traces(
    sub_data,
    visibility = visibility_defaults,
    legend_names = character(0),
    vars_colors = vars_colors
  )

  expect_type(result, "list")

  # Extract unique legend names from traces
  trace_names <- unique(vapply(
    result$new_traces,
    \(tr) tr$name %||% "",
    character(1)
  ))

  # Each crosswalk pair should produce a combined "old_label/new_label" legend.
  # The function deduplicates " - Total" and " - Span Length" suffixes,
  # e.g. "Digit Span Forward - Total/Number Span Forward - Total" becomes
  # "Digit Span Forward/Number Span Forward - Total".
  for (i in seq_along(old_vars)) {
    old_label <- nacc_var_labels[old_vars[i]]
    new_label <- nacc_var_labels[new_vars[i]]
    combined <- paste0(old_label, "/", new_label)

    # Replicate the cleanup logic from plotly_new_traces
    if (grepl(" - Total| - Span Length", combined)) {
      if (grepl(" - Total", combined)) {
        combined <- paste0(gsub(" - Total", "", combined), " - Total")
      }
      if (grepl(" - Span Length", combined)) {
        combined <- paste0(gsub(" - Span Length", "", combined), " - Span Length")
      }
    }

    expect_true(
      combined %in% trace_names,
      info = paste("Missing combined legend for", old_vars[i], "/", new_vars[i])
    )
  }

  # All marker traces for crosswalk pairs should use diamond/cross symbols
  crosswalk_marker_traces <- Filter(
    \(tr) grepl("/", tr$name %||% "") && tr$mode == "markers",
    result$new_traces
  )

  expect_true(length(crosswalk_marker_traces) > 0)

  for (tr in crosswalk_marker_traces) {
    symbols <- unlist(tr$marker$symbol)
    expect_true(
      all(symbols %in% c("diamond", "cross")),
      info = paste("Bad symbols in trace", tr$name)
    )
  }
})

test_that("plotly_new_traces converts T-scores to z-scores in traces", {
  # Build a synthetic dataset with a T-score column (method = "tscores")
  sub_data <- data.table::data.table(
    VISITDATE = as.Date(c("2020-06-15", "2021-06-15")),
    std_MOCATOTS = c(60, 70),
    raw_MOCATOTS = c(25, 28)
  )

  # Tag std_MOCATOTS as T-scores
  attr(sub_data$std_MOCATOTS, "method") <- "tscores"

  vars_colors <- c(MOCATOTS = "#000000")

  result <- plotly_new_traces(
    sub_data,
    visibility = visibility_defaults,
    legend_names = character(0),
    vars_colors = vars_colors
  )

  expect_type(result, "list")

  # T=60 → z=1.0, T=70 → z=2.0.  The y-range should reflect these z-values.
  expect_equal(result$new_y_range, c(1, 2))

  # The line trace y-values should be the converted z-scores, not T-scores
  line_trace <- Filter(
    \(tr) tr$mode == "lines",
    result$new_traces
  )[[1]]

  expect_equal(unlist(line_trace$y), c(1, 2))
})

test_that("plotly_new_traces returns NULL with only non-score columns", {
  # A dataset that has VISITDATE and other columns but nothing matching
  # the std_/raw_ pattern
  sub_data <- data.table::data.table(
    VISITDATE = as.Date(c("2020-06-15", "2021-06-15")),
    NACCID = c("001", "001"),
    NACCAGE = c(72, 73),
    SEX = c(1, 1)
  )

  result <- plotly_new_traces(
    sub_data,
    visibility = visibility_defaults,
    legend_names = character(0),
    vars_colors = c()
  )

  expect_null(result)
})

test_that("plotly_new_traces returns input legend_names unchanged", {
  # Note: legend_names is updated inside the lapply closure but the

  # assignment only modifies the local scope, so the returned legend_names
  # is the original input. The legend names ARE used as trace$name fields.
  prepared <- get_prepared_demo_data()

  first_id <- prepared$NACCID[1]
  pt_data <- prepared[NACCID == first_id]

  std_raw_cols <- grep("^(std_|raw_)", colnames(pt_data), value = TRUE)
  sub_data <- pt_data[, c("VISITDATE", std_raw_cols), with = FALSE]

  var_names <- unique(gsub(
    "^std_",
    "",
    grep("^std_", std_raw_cols, value = TRUE)
  ))
  display_names <- var_names
  display_names[display_names %in% names(crosswalk_translations)] <-
    crosswalk_translations[display_names[
      display_names %in% names(crosswalk_translations)
    ]]
  display_names <- unique(display_names)

  vars_colors <- setNames(
    rep("#000000", length(display_names)),
    display_names
  )

  pre_existing <- c("Already Here", "Another Legend")

  result <- plotly_new_traces(
    sub_data,
    visibility = visibility_defaults,
    legend_names = pre_existing,
    vars_colors = vars_colors
  )

  # The returned legend_names is the input (not augmented) due to lapply scoping
  expect_equal(result$legend_names, pre_existing)
})
