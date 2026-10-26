#' Helpers from gtExtras used in `my_gt_blt_bar_pct`
#'
#' @rdname from_gtExtras_for_my_gt_blt_bar_pct
#' @keywords internal

# ideal_fgnd_color <- function(bgnd_color, light = "#FFFFFF", dark = "#000000") {
#   bgnd_color <- rgba_to_hex(colors = bgnd_color)
#   bgnd_color <- html_color(colors = bgnd_color, alpha = 1)
#   yiq_contrasted_threshold <- 128
#   colors <- grDevices::col2rgb(bgnd_color)
#   score <- colSums(colors * c(299, 587, 144)) / 1000
#   ifelse(score >= yiq_contrasted_threshold, dark, light)
# }

rgba_to_hex <- function(
  colors
) {
  colors_vec <- rep(NA_character_, length(colors))
  colors_rgba <- is_rgba_col(colors = colors)
  colors_vec[!colors_rgba] <- colors[!colors_rgba]
  color_matrix <- colors[colors_rgba] |>
    gsub(
      pattern = "(rgba\\(|\\))",
      replacement = "",
      x = _
    ) |>
    strsplit(",") |>
    unlist() |>
    as.numeric() |>
    matrix(
      data = _,
      ncol = 4,
      dimnames = list(
        c(),
        c("r", "g", "b", "alpha")
      ),
      byrow = TRUE
    )
  alpha <- color_matrix[, "alpha"] |> unname()
  colors_to_hex <- grDevices::rgb(
    red = color_matrix[, "r"] / 255,
    green = color_matrix[, "g"] / 255,
    blue = color_matrix[, "b"] / 255,
    alpha = alpha
  )
  colors_vec[colors_rgba] <- colors_to_hex
  colors_vec
}

html_color <- function(colors, alpha = NULL) {
  if (any(is.na(colors))) {
    stop("No values supplied in `colors` should be `NA`.", call. = FALSE)
  }
  is_rgba <- is_rgba_col(colors = colors)
  is_short_hex <- is_short_hex(colors = colors)
  colors[is_short_hex] <- expand_short_hex(colors = colors[is_short_hex])
  is_hex <- is_hex_col(colors = colors)
  is_named <- !is_rgba & !is_hex
  colors[is_named] <- tolower(colors[is_named])
  named_colors <- colors[is_named]
  if (length(named_colors) > 0) {
    check_named_colors(named_colors)
    named_colors[named_colors == "transparent"] <- "#FFFFFF00"
    is_css_excl_named <- colors %in% names(css_exclusive_colors())
    if (any(is_css_excl_named)) {
      colors[is_css_excl_named] <- unname(css_exclusive_colors()[colors[
        is_css_excl_named
      ]])
    }
  }
  colors[!is_rgba] <- normalize_colors(colors = colors[!is_rgba], alpha = alpha)
  colors
}


is_rgba_col <- function(colors) {
  grepl("^rgba\\(\\s*(?:[0-9]+?\\s*,\\s*){3}[0-9\\.]+?\\s*\\)$", colors)
}
is_short_hex <- function(colors) {
  grepl("^#[0-9a-fA-F]{3}([0-9a-fA-F])?$", colors)
}
expand_short_hex <- function(colors) {
  gsub("^#(.)(.)(.)(.?)$", "#\\1\\1\\2\\2\\3\\3\\4\\4", toupper(colors))
}
is_hex_col <- function(colors) {
  grepl("^#[0-9a-fA-F]{6}([0-9a-fA-F]{2})?$", colors)
}

check_named_colors <- function(named_colors) {
  named_colors <- tolower(named_colors)
  if (!all(named_colors %in% valid_color_names())) {
    invalid_colors <- setdiff(unique(named_colors), valid_color_names())
    one_several_invalid <- ifelse(
      length(invalid_colors) > 1,
      "Several invalid color names were ",
      "An invalid color name was "
    )
    stop(
      "Only R/X11 color names and CSS 3.0 color names can be used.",
      call. = FALSE
    )
  }
}


valid_color_names <- function() {
  c(tolower(grDevices::colors()), names(css_exclusive_colors()), "transparent")
}
css_exclusive_colors <- function() {
  color_tbl_subset <- css_colors[!css_colors$is_x11_color, ]
  color_values <- color_tbl_subset[["hexadecimal"]]
  color_values <- stats::setNames(
    color_values,
    tolower(color_tbl_subset[["color_name"]])
  )
  color_values
}


normalize_colors <- function(colors, alpha) {
  color_matrix <- t(grDevices::col2rgb(col = colors, alpha = TRUE))
  color_matrix[, "alpha"] <- color_matrix[, "alpha"] / 255
  if (!is.null(alpha)) {
    color_matrix[, "alpha"] <- alpha
  }
  colors_html <- rep(NA_character_, nrow(color_matrix))
  colors_alpha_1 <- color_matrix[, "alpha"] == 1
  colors_html[colors_alpha_1] <- grDevices::rgb(
    red = color_matrix[colors_alpha_1, "red", drop = FALSE] / 255,
    green = color_matrix[colors_alpha_1, "green", drop = FALSE] / 255,
    blue = color_matrix[colors_alpha_1, "blue", drop = FALSE] / 255
  )
  colors_html[!colors_alpha_1] <- col_matrix_to_rgba(
    color_matrix[!colors_alpha_1, , drop = FALSE]
  )

  colors_html
}

col_matrix_to_rgba <- function(color_matrix) {
  paste0(
    "rgba(",
    color_matrix[, "red"],
    ",",
    color_matrix[, "green"],
    ",",
    color_matrix[, "blue"],
    ",",
    round(color_matrix[, "alpha"], 2),
    ")"
  )
}

gt_index <- function(gt_object, column, as_vector = TRUE) {
  stopifnot(
    `'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?` = "gt_tbl" %in%
      class(gt_object)
  )
  stopifnot(
    `'column' must be a single string` = is.character(column) &&
      length(column) == 1L
  )
  stopifnot(`'as_vector' must be a TRUE or FALSE` = is.logical(as_vector))
  if (length(gt_object[["_row_groups"]]) >= 1) {
    gt_row_grps <- gt_object[["_row_groups"]]

    grp_vec_ord <- gt_object[["_stub_df"]]
    grp_vec_ord$group_id <- factor(grp_vec_ord$group_id, levels = gt_row_grps)
    grp_vec_ord <- grp_vec_ord[order(grp_vec_ord$group_id), "rownum_i"]
    df_ordered <- gt_object[["_data"]][grp_vec_ord, ]
  } else {
    df_ordered <- gt_object[["_data"]]
  }
  if (isTRUE(as_vector)) {
    df_ordered[[column]]
  } else {
    df_ordered
  }
}
