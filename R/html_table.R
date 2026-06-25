# ============================================================================
# html_table: a dependency-light, shiny::tags-based table builder.
#
# Current scope:
#   * Row groups   (via `groupname_col`)
#   * Row stub     (via `rowname_col`)
#   * Stubhead     (via `ht_tab_stubhead()`)
#   * Column spanners (via `ht_tab_spanner()`)
#   * Hide/rename columns (`ht_cols_hide()`, `ht_cols_label()`)
#   * Table-level footnotes (`ht_tab_footnote()`)
#   * Per-cell styles (`ht_tab_style()` + `ht_cells_*()` + `ht_cell_*()`)
#
# Deferred for later iterations:
#   * Cell-anchored footnote markers (`locations =` in tab_footnote)
#   * Formatters (`fmt`, `fmt_markdown`, `sub_missing`), column alignment
#     helper, tab_options shorthand.
#
# Use inside Shiny:
#
#   # Server
#   output$tbl <- shiny::renderUI(ht_render(
#     ht_table(my_data, rowname_col = "name", groupname_col = "group") |>
#       ht_tab_spanner("Scores", c("raw", "std"))
#   ))
#
#   # UI
#   shiny::uiOutput("tbl")
#
# Only `{shiny}` is used (for `shiny::tags$*` and class-name checks). No
# `{htmltools}`, `{gt}`, or `{DT}` dependency is introduced.
# ============================================================================

#' Build an HTML table (first-cut: structure only)
#'
#' Constructs a `html_table` accumulator. Layered ops (column spanners,
#' hidden columns, etc.) are added via `ht_*` functions and the final
#' object is rendered with [ht_render()] for use inside `shiny::renderUI()`.
#'
#' @param data A data.frame / data.table.
#' @param id Optional string used as the `id=` attribute on the rendered
#'   `<table>`.
#' @param rowname_col Optional string naming the column to be rendered as
#'   the row stub (`<th scope="row">`).
#' @param groupname_col Optional string naming the column to be rendered
#'   as row group headers.
#'
#' @returns A `html_table` object.
#'
#' @export
ht_table <- function(
  data,
  id = NULL,
  rowname_col = NULL,
  groupname_col = NULL
) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data.frame / data.table.")
  }
  # Coerce away from data.table to keep indexing simple inside the builder.
  data <- as.data.frame(data, stringsAsFactors = FALSE)

  if (!is.null(rowname_col) && !rowname_col %in% names(data)) {
    cli::cli_abort(
      "{.arg rowname_col} = {.val {rowname_col}} not in {.arg data}."
    )
  }
  if (!is.null(groupname_col) && !groupname_col %in% names(data)) {
    cli::cli_abort(
      "{.arg groupname_col} = {.val {groupname_col}} not in {.arg data}."
    )
  }

  structure(
    list(
      data = data,
      id = id,
      rowname_col = rowname_col,
      groupname_col = groupname_col,
      col_hide = character(),
      col_labels = list(), # named: column name -> label (tag or string)
      col_label_merges = list(), # list of list(label =, columns = char, align =)
      spanners = list(), # list of list(label =, columns = char)
      stubhead_label = NULL,
      hide_header = FALSE,
      custom_css = character(),
      footnotes = list(), # list of footnote contents (string or tag)
      styles = list() # list of list(style = named-char-vec, locations = list)
    ),
    class = "html_table"
  )
}


# ---- Builder ops ----------------------------------------------------------

#' Hide one or more columns.
#'
#' @param x A `html_table`.
#' @param columns Character vector of column names.
#' @export
ht_cols_hide <- function(x, columns) {
  stopifnot(is.character(columns))
  x$col_hide <- unique(c(x$col_hide, intersect(columns, names(x$data))))
  x
}

#' Relabel one or more columns.
#'
#' @param x A `html_table`.
#' @param ... Named arguments mapping `column_name = label`. Labels may be
#'   strings or shiny tags (incl. `shiny::HTML()`).
#' @export
ht_cols_label <- function(x, ...) {
  labels <- list(...)
  if (length(labels) == 0L) {
    return(x)
  }
  if (is.null(names(labels)) || any(!nzchar(names(labels)))) {
    cli::cli_abort("All arguments to {.fn ht_cols_label} must be named.")
  }
  x$col_labels[names(labels)] <- labels
  x
}

#' Merge column labels into one cell.
#'
#' Replaces the individual `<th>`s for the named columns with a single
#' `<th colspan=N>` in the column-label row. Use this when several
#' columns share a unified label (e.g., a `"Raw"` header spanning a
#' value column and a units column).
#'
#' Differs from [ht_tab_spanner()] in that a spanner adds a *new row*
#' above the column labels, whereas a merge collapses cells *within*
#' the column-label row.
#'
#' The columns to merge must be contiguous in the rendered table after
#' hidden columns are removed; non-contiguous merges are ignored with a
#' warning, since they would produce invalid markup.
#'
#' Any individual labels set via [ht_cols_label()] for columns inside
#' the merge group are overridden by the merge's `label`. Styles set
#' via [ht_tab_style()] / [ht_cells_column_labels()] on any of the
#' covered columns combine onto the merged cell.
#'
#' @param x A `html_table`.
#' @param label String or tag for the merged cell.
#' @param columns Character vector of column names to merge (length >= 2).
#' @param align One of `"left"`, `"center"`, `"right"`. Default `"center"`.
#'
#' @examples
#' \dontrun{
#' ht_table(dat) |>
#'   ht_tab_spanner("Scores", c("raw", "raw_suffix", "units", "std")) |>
#'   ht_merge_cols_label("Raw", columns = c("raw", "raw_suffix"))
#' }
#'
#' @export
ht_merge_cols_label <- function(
  x,
  label,
  columns,
  align = c("center", "left", "right")
) {
  align <- match.arg(align)
  stopifnot(
    "`columns` must be a character vector" = is.character(columns),
    "`columns` must name at least 2 columns" = length(columns) >= 2L
  )
  cols <- intersect(columns, names(x$data))
  if (length(cols) < 2L) {
    cli::cli_warn(
      "Fewer than 2 of {.arg columns} found in the data; merge ignored."
    )
    return(x)
  }
  x$col_label_merges[[length(x$col_label_merges) + 1L]] <- list(
    label = label,
    columns = cols,
    align = align
  )
  x
}

#' Add a column spanner across one or more columns.
#'
#' @param x A `html_table`.
#' @param label String or tag rendered above the spanned columns.
#' @param columns Character vector of column names to span.
#' @export
ht_tab_spanner <- function(x, label, columns) {
  stopifnot(is.character(columns), length(columns) >= 1L)
  cols <- intersect(columns, names(x$data))
  if (length(cols) == 0L) {
    cli::cli_warn(
      "None of {.arg columns} found in {.arg data}; spanner ignored."
    )
    return(x)
  }
  x$spanners[[length(x$spanners) + 1L]] <- list(label = label, columns = cols)
  x
}

#' Set the stubhead label (top-left cell, only shown when `rowname_col` is set).
#'
#' @param x A `html_table`.
#' @param label String or tag.
#' @export
ht_tab_stubhead <- function(x, label) {
  x$stubhead_label <- label
  x
}

#' Append a table-level footnote.
#'
#' Rendered in a `<tfoot>` below the body. Call multiple times to add
#' multiple footnotes; each appears on its own row in input order.
#'
#' Markers tied to specific cells are not supported yet — `locations`
#' (à la `gt::tab_footnote(locations = cells_body(...))`) is on the
#' roadmap for the `assessment_summary_table()` migration.
#'
#' @param x A `html_table`.
#' @param footnote String or shiny tag.
#'
#' @export
ht_tab_footnote <- function(x, footnote) {
  x$footnotes <- c(x$footnotes, list(footnote))
  x
}

#' Attach styles to specific cell locations.
#'
#' Mirrors `gt::tab_style()`. `style` is a named character vector of CSS
#' properties (most easily built with [ht_cell_text()] / [ht_cell_fill()]),
#' a raw CSS string (`"padding-left: 0;"`), or a list of any of those.
#' `locations` is a single `ht_cells_*()` object or a list of them.
#'
#' `rows` expressions on body / stub locations are evaluated *here*
#' against `x$data`, not deferred to render time. Free variables in the
#' expression (e.g., a loop-local `desc`) are resolved against the
#' caller's frame at the moment `ht_tab_style()` runs, so loops like
#' `for (desc in ...) out <- ht_tab_style(out, ..., rows = X == desc)`
#' behave as written.
#'
#' @param x A `html_table`.
#' @param style A style spec. See description.
#' @param locations One [ht_cells_body()] / [ht_cells_stub()] / etc., or a
#'   list of them.
#'
#' @examples
#' \dontrun{
#' ht_table(dat) |>
#'   ht_tab_style(
#'     style = ht_cell_text(weight = "bold", color = "red"),
#'     locations = ht_cells_body(columns = "value", rows = is.na(value))
#'   )
#' }
#'
#' @export
ht_tab_style <- function(x, style, locations) {
  css <- normalize_style(style)
  if (inherits(locations, "ht_loc")) {
    locations <- list(locations)
  }
  if (
    !is.list(locations) ||
      !all(vapply(locations, inherits, logical(1), "ht_loc"))
  ) {
    cli::cli_abort(
      "{.arg locations} must be a {.cls ht_loc} (from {.fn ht_cells_body} etc.) or a list of them."
    )
  }
  # Eagerly resolve any deferred row-expressions against x$data, so
  # loop-local free variables are captured at their current value
  # rather than at render time (when only the loop's final value
  # would survive). Locations that don't reference data rows pass
  # through untouched.
  locations <- lapply(locations, resolve_loc_now, data = x$data)

  x$styles <- c(
    x$styles,
    list(list(style = css, locations = locations))
  )
  x
}

#' Suppress the entire `<thead>` (spanner row + column labels).
#'
#' Equivalent to gt's `tab_options(column_labels.hidden = TRUE)` when no
#' spanners are present. For demographics-style key/value tables.
#'
#' @param x A `html_table`.
#' @export
ht_hide_header <- function(x) {
  x$hide_header <- TRUE
  x
}

#' Attach scoped CSS to a single table.
#'
#' Every selector in `css` is prepended with `#<id>` (where `id` is the
#' value passed to [ht_table()]), so the styles only affect this table.
#' `@media`/`@supports`/`@container` blocks are recursed into; other
#' `@`-rules pass through untouched.
#'
#' To style the `<table>` element itself, use `&` (SCSS / native CSS
#' nesting convention) — it is replaced with `#<id>` directly, without a
#' descendant combinator. So `& { border: 1px solid; }` becomes
#' `#<id> { border: 1px solid; }`, whereas plain `tr { ... }` becomes
#' `#<id> tr { ... }`.
#'
#' @param x A `html_table`. Must have been constructed with a non-empty
#'   `id`.
#' @param css A single character string of CSS rules.
#'
#' @examples
#' \dontrun{
#' ht_table(dat, id = "demographics-table") |>
#'   ht_hide_header() |>
#'   ht_add_css("
#'     & {
#'       border: 1px solid #333;
#'     }
#'     tr:first-child { font-weight: bold; }
#'     td { padding: 2px 8px; }
#'   ")
#' }
#'
#' @export
ht_add_css <- function(x, css) {
  if (is.null(x$id) || !nzchar(x$id)) {
    cli::cli_abort(
      "{.fn ht_add_css} requires an {.arg id} on the table. Pass one to {.fn ht_table}."
    )
  }
  stopifnot(is.character(css), length(css) == 1L)
  scoped <- scope_css(css, x$id)
  x$custom_css <- c(x$custom_css, scoped)
  x
}


# ---- Cell style constructors ---------------------------------------------

#' Build a text-style spec for use in [ht_tab_style()].
#'
#' Returns a named character vector of CSS properties. Multiple style specs
#' may be combined by passing them as a list to [ht_tab_style()].
#'
#' @param weight `font-weight` (e.g., `"bold"`, `400`).
#' @param color `color`.
#' @param style `font-style` (e.g., `"italic"`).
#' @param align `text-align`.
#' @param v_align `vertical-align`.
#' @param whitespace `white-space` (e.g., `"nowrap"`, `"normal"`).
#' @param decorate `text-decoration-line` (e.g., `"underline"`).
#' @param size `font-size` (e.g., `"14px"`).
#' @param transform `text-transform`.
#'
#' @export
ht_cell_text <- function(
  weight = NULL,
  color = NULL,
  style = NULL,
  align = NULL,
  v_align = NULL,
  whitespace = NULL,
  decorate = NULL,
  size = NULL,
  transform = NULL
) {
  css <- character()
  if (!is.null(weight)) {
    css["font-weight"] <- weight
  }
  if (!is.null(color)) {
    css["color"] <- color
  }
  if (!is.null(style)) {
    css["font-style"] <- style
  }
  if (!is.null(align)) {
    css["text-align"] <- align
  }
  if (!is.null(v_align)) {
    css["vertical-align"] <- v_align
  }
  if (!is.null(whitespace)) {
    css["white-space"] <- whitespace
  }
  if (!is.null(decorate)) {
    css["text-decoration-line"] <- decorate
  }
  if (!is.null(size)) {
    css["font-size"] <- size
  }
  if (!is.null(transform)) {
    css["text-transform"] <- transform
  }
  css
}

#' Build a fill-style (background-color) spec for [ht_tab_style()].
#'
#' @param color Background color.
#' @export
ht_cell_fill <- function(color) {
  c("background-color" = color)
}


# ---- Cell location constructors ------------------------------------------

#' Locations for [ht_tab_style()]
#'
#' Each returns an `ht_loc` object naming a region of the table.
#' `rows` arguments use non-standard evaluation: write expressions
#' referencing the original data columns (e.g., `rows = is.na(value)`).
#'
#' Free variables in `rows` (e.g., a loop-local `desc`) are resolved
#' against the calling frame at the moment [ht_tab_style()] runs — not
#' deferred to render time — so writing loops like
#' `for (desc in ...) ht_tab_style(..., rows = Description == desc)`
#' behaves as written.
#'
#' @param columns Character vector of column names, or `NULL` for all.
#' @param rows Expression evaluated against the data. May yield a logical
#'   vector, integer indices, or `NULL` for all rows.
#' @param groups Character vector of group names, or `NULL` for all.
#'
#' @returns An object inheriting from `ht_loc`.
#'
#' @name ht_cells
#' @export
ht_cells_body <- function(columns = NULL, rows = NULL) {
  structure(
    list(
      columns = columns,
      rows_expr = substitute(rows),
      rows_env = parent.frame()
    ),
    class = c("ht_cells_body", "ht_loc")
  )
}

#' @rdname ht_cells
#' @export
ht_cells_stub <- function(rows = NULL) {
  structure(
    list(
      rows_expr = substitute(rows),
      rows_env = parent.frame()
    ),
    class = c("ht_cells_stub", "ht_loc")
  )
}

#' @rdname ht_cells
#' @export
ht_cells_column_labels <- function(columns = NULL) {
  structure(
    list(columns = columns),
    class = c("ht_cells_column_labels", "ht_loc")
  )
}

#' @rdname ht_cells
#' @export
ht_cells_stubhead <- function() {
  structure(list(), class = c("ht_cells_stubhead", "ht_loc"))
}

#' @rdname ht_cells
#' @export
ht_cells_row_groups <- function(groups = NULL) {
  structure(
    list(groups = groups),
    class = c("ht_cells_row_groups", "ht_loc")
  )
}

#' @rdname ht_cells
#' @export
ht_cells_footnotes <- function() {
  structure(list(), class = c("ht_cells_footnotes", "ht_loc"))
}


# ---- Rendering ------------------------------------------------------------

#' Render a `html_table` to `shiny::tags`
#'
#' Wrap the final builder result with `ht_render()` before passing it to
#' `shiny::renderUI()`:
#'
#' ```r
#' output$tbl <- shiny::renderUI(ht_render(my_table_fn(...)))
#' ```
#'
#' @param x A `html_table`.
#'
#' @returns A `shiny.tag` (the `<table>` element).
#'
#' @export
ht_render <- function(x) {
  if (!inherits(x, "html_table")) {
    cli::cli_abort("{.arg x} must be a {.cls html_table}.")
  }
  render_html_table(x)
}

#' @method print html_table
#' @export
print.html_table <- function(x, ...) {
  # Delegate to shiny.tag's print method, which (via htmltools, a transitive
  # dep of shiny) opens an RStudio Viewer preview when interactive() and
  # falls back to cat()ing the HTML otherwise.
  print(render_html_table(x), ...)
  invisible(x)
}


# Find which spanner (if any) a given column belongs to. Returns an index
# into x$spanners or NA_integer_.
find_spanner_for <- function(col, spanners) {
  for (i in seq_along(spanners)) {
    if (col %in% spanners[[i]]$columns) return(i)
  }
  NA_integer_
}

# ---- Style internals ------------------------------------------------------

# Normalize the `style` argument of ht_tab_style() into a named character
# vector of CSS properties (last-write-wins on key conflicts).
# Accepts:
#   * named char vec   ("font-weight" = "bold") — passed through
#   * raw CSS string   ("padding-left: 0;")     — parsed
#   * list of any of the above                  — concatenated, deduped
normalize_style <- function(style) {
  if (is.null(style)) {
    return(character())
  }
  if (is.character(style)) {
    if (!is.null(names(style)) && all(nzchar(names(style)))) {
      return(style)
    }
    return(parse_inline_css(style))
  }
  if (is.list(style)) {
    parts <- lapply(style, normalize_style)
    out <- do.call(c, parts)
    if (length(out) == 0L) {
      return(character())
    }
    out[!duplicated(names(out), fromLast = TRUE)]
  } else {
    cli::cli_abort("Unrecognized {.arg style} format.")
  }
}

# Parse an inline CSS declaration block ("color: red; padding-left: 0")
# into a named char vec. Splits on `;` and only on the first `:` per
# declaration (so URLs etc. survive).
parse_inline_css <- function(s) {
  parts <- strsplit(paste(s, collapse = ";"), ";", fixed = TRUE)[[1]]
  parts <- trimws(parts)
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0L) {
    return(character())
  }
  colon_pos <- regexpr(":", parts)
  good <- colon_pos > 0L
  if (any(!good)) {
    cli::cli_warn(
      "Skipping CSS declaration(s) without {.val :}: {.val {parts[!good]}}."
    )
    parts <- parts[good]
    colon_pos <- colon_pos[good]
  }
  props <- trimws(substr(parts, 1L, colon_pos - 1L))
  vals <- trimws(substr(parts, colon_pos + 1L, nchar(parts)))
  out <- vals
  names(out) <- props
  out
}

# Combine multiple CSS char vectors. Later args override earlier ones on
# property collisions. NULL and empty args drop out.
combine_css <- function(...) {
  parts <- list(...)
  parts <- parts[lengths(parts) > 0L]
  if (length(parts) == 0L) {
    return(character())
  }
  out <- do.call(c, parts)
  out[!duplicated(names(out), fromLast = TRUE)]
}

# Render a CSS char vec as an inline `style=` attribute value, or NULL when
# empty (so shiny omits the attribute).
css_to_inline <- function(css) {
  if (length(css) == 0L) {
    return(NULL)
  }
  paste(paste0(names(css), ": ", css), collapse = "; ")
}

# Translate a location's already-resolved original-data row indices to
# positions in the SORTED data. (Resolution against the data happens in
# `ht_tab_style()` via `resolve_loc_now()`, so by the time we get here
# `loc$rows_val` is guaranteed to be set on body/stub locations.)
resolve_loc_rows <- function(loc, sorted_data, orig_row_idx) {
  orig_idx <- loc$rows_val
  if (is.null(orig_idx)) {
    return(seq_len(nrow(sorted_data)))
  }
  sorted_idx <- match(orig_idx, orig_row_idx)
  sorted_idx[!is.na(sorted_idx)]
}

# Resolve a body/stub location's `rows` expression eagerly against the
# table's data. Sets `rows_val` to a vector of *original-data* row indices
# and clears the expression+env, so render time has nothing to look up.
#
# Locations without a `rows_expr` slot (column_labels, stubhead, etc.)
# pass through unchanged.
resolve_loc_now <- function(loc, data) {
  if (
    !inherits(loc, c("ht_cells_body", "ht_cells_stub"), which = FALSE) ||
      !"rows_expr" %in% names(loc)
  ) {
    return(loc)
  }
  expr <- loc$rows_expr
  if (is.null(expr) || identical(expr, quote(NULL))) {
    loc$rows_val <- seq_len(nrow(data))
    loc$rows_expr <- NULL
    loc$rows_env <- NULL
    return(loc)
  }
  val <- tryCatch(
    eval(expr, envir = data, enclos = loc$rows_env %||% parent.frame()),
    error = function(e) {
      cli::cli_abort(c(
        "Could not evaluate {.arg rows} expression against the table's data.",
        "i" = conditionMessage(e)
      ))
    }
  )
  orig_idx <- if (is.logical(val)) {
    if (length(val) != nrow(data)) {
      cli::cli_warn(
        "{.arg rows} returned logical of length {length(val)} but data has {nrow(data)} row(s)."
      )
    }
    which(val)
  } else if (is.numeric(val)) {
    as.integer(val)
  } else {
    cli::cli_abort(
      "{.arg rows} must evaluate to logical or numeric, not {.cls {class(val)[1]}}."
    )
  }
  loc$rows_val <- orig_idx
  loc$rows_expr <- NULL
  loc$rows_env <- NULL
  loc
}

# Resolve the `columns` slot of a location to a character vector,
# intersected with the visible columns.
resolve_loc_cols <- function(loc_cols, visible_cols) {
  if (is.null(loc_cols)) {
    return(visible_cols)
  }
  intersect(loc_cols, visible_cols)
}

# Walk x$styles and produce a map keyed by cell location. Each slot holds
# the accumulated CSS (named char vec) to apply at that location.
compute_style_maps <- function(x, sorted_data, orig_row_idx, visible_cols) {
  n <- nrow(sorted_data)
  body_styles <- setNames(
    lapply(visible_cols, function(.) vector("list", n)),
    visible_cols
  )
  maps <- list(
    body = body_styles,
    stub = vector("list", n),
    column_labels = setNames(
      vector("list", length(visible_cols)),
      visible_cols
    ),
    stubhead = character(),
    row_groups = list(),
    footnotes = character()
  )

  for (entry in x$styles) {
    css <- entry$style
    if (length(css) == 0L) {
      next
    }
    for (loc in entry$locations) {
      if (inherits(loc, "ht_cells_body")) {
        cols <- resolve_loc_cols(loc$columns, visible_cols)
        rows <- resolve_loc_rows(loc, sorted_data, orig_row_idx)
        for (cc in cols) {
          for (r in rows) {
            maps$body[[cc]][[r]] <- combine_css(maps$body[[cc]][[r]], css)
          }
        }
      } else if (inherits(loc, "ht_cells_stub")) {
        rows <- resolve_loc_rows(loc, sorted_data, orig_row_idx)
        for (r in rows) {
          maps$stub[[r]] <- combine_css(maps$stub[[r]], css)
        }
      } else if (inherits(loc, "ht_cells_column_labels")) {
        cols <- resolve_loc_cols(loc$columns, visible_cols)
        for (cc in cols) {
          maps$column_labels[[cc]] <- combine_css(maps$column_labels[[cc]], css)
        }
      } else if (inherits(loc, "ht_cells_stubhead")) {
        maps$stubhead <- combine_css(maps$stubhead, css)
      } else if (inherits(loc, "ht_cells_row_groups")) {
        groups <- loc$groups
        if (is.null(groups) && !is.null(x$groupname_col)) {
          groups <- unique(stats::na.omit(as.character(x$data[[
            x$groupname_col
          ]])))
        }
        for (gg in groups) {
          maps$row_groups[[gg]] <- combine_css(maps$row_groups[[gg]], css)
        }
      } else if (inherits(loc, "ht_cells_footnotes")) {
        maps$footnotes <- combine_css(maps$footnotes, css)
      }
    }
  }

  maps
}

# Walk `visible_cols` in order, emitting either a `<th colspan>` for each
# spanner run or a blank `<th>` for unspanned columns. Returns a list of tags.
build_spanner_cells <- function(visible_cols, spanners) {
  cells <- list()
  i <- 1L
  while (i <= length(visible_cols)) {
    sp_idx <- find_spanner_for(visible_cols[[i]], spanners)
    if (is.na(sp_idx)) {
      cells <- c(cells, list(shiny::tags$th(class = "ntrd-no-spanner")))
      i <- i + 1L
    } else {
      run <- i
      while (
        run <= length(visible_cols) &&
          identical(find_spanner_for(visible_cols[[run]], spanners), sp_idx)
      ) {
        run <- run + 1L
      }
      span <- run - i
      cells <- c(
        cells,
        list(
          shiny::tags$th(
            class = "ntrd-spanner",
            colspan = span,
            spanners[[sp_idx]]$label
          )
        )
      )
      i <- run
    }
  }
  cells
}


# Given the user's merge entries and the final ordered list of visible
# columns, build a per-position lookup `at[[i]]` that returns the merge
# entry covering position `i`, or NULL when no merge applies there.
# Validates that each merge's columns are contiguous in `visible_cols`;
# non-contiguous merges are dropped with a warning.
plan_col_label_merges <- function(merges, visible_cols) {
  at <- vector("list", length(visible_cols))
  if (length(merges) == 0L) {
    return(list(at = at))
  }

  for (m in merges) {
    cols <- intersect(m$columns, visible_cols)
    if (length(cols) < 2L) {
      next
    } # nothing left to merge after hiding
    positions <- match(cols, visible_cols)
    rng <- min(positions):max(positions)
    if (!setequal(positions, rng)) {
      cli::cli_warn(
        c(
          "Column-label merge for {.val {m$label}} spans non-contiguous columns; ignored.",
          "i" = "Merged columns: {.val {cols}}; positions: {positions}."
        )
      )
      next
    }
    # Refresh the column list to match render order, in case the user
    # passed them out of order.
    m$columns <- visible_cols[rng]
    occupied <- !vapply(at[rng], is.null, logical(1))
    if (any(occupied)) {
      cli::cli_warn(
        c(
          "Column-label merge for {.val {m$label}} overlaps an earlier merge; ignored.",
          "i" = "Conflicting columns: {.val {visible_cols[rng][occupied]}}."
        )
      )
      next
    }
    for (pos in rng) {
      at[[pos]] <- m
    }
  }
  list(at = at)
}


render_html_table <- function(x) {
  data <- x$data
  orig_row_idx <- seq_len(nrow(data))

  # 1. Sort by group (preserving original order within each group). Track
  #    the original row positions in `orig_row_idx` so that `rows = ...`
  #    expressions in locations can be translated back to sorted positions.
  if (!is.null(x$groupname_col)) {
    g <- as.character(data[[x$groupname_col]])
    grp_levels <- unique(g[!is.na(g)])
    ord <- order(match(g, grp_levels))
    data <- data[ord, , drop = FALSE]
    orig_row_idx <- ord
  }

  # 2. Visible columns: everything except rowname_col, groupname_col, hidden.
  visible_cols <- setdiff(
    names(data),
    c(x$col_hide, x$rowname_col, x$groupname_col)
  )

  # 3. Pre-compute the style map by walking x$styles. The map carries the
  #    accumulated CSS for each cell location and is consulted while
  #    building tags below.
  style_maps <- compute_style_maps(x, data, orig_row_idx, visible_cols)

  # 4. Build <thead>, unless suppressed.
  thead <- NULL
  if (!isTRUE(x$hide_header)) {
    thead_rows <- list()

    if (length(x$spanners) > 0L) {
      spanner_cells <- list()
      if (!is.null(x$rowname_col)) {
        # blank cell over the stub column
        spanner_cells <- c(spanner_cells, list(shiny::tags$th()))
      }
      spanner_cells <- c(
        spanner_cells,
        build_spanner_cells(visible_cols, x$spanners)
      )
      thead_rows <- c(
        thead_rows,
        list(
          shiny::tags$tr(class = "ntrd-spanner-row", spanner_cells)
        )
      )
    }

    label_cells <- list()
    if (!is.null(x$rowname_col)) {
      label_cells <- c(
        label_cells,
        list(
          shiny::tags$th(
            class = "ntrd-stubhead",
            style = css_to_inline(style_maps$stubhead),
            x$stubhead_label %||% ""
          )
        )
      )
    }
    # Validate merges against the *visible* column order and resolve them
    # into a per-position lookup: positions covered by a merge map to the
    # merge entry; the first position of each merge group is flagged so we
    # only emit the cell once.
    merge_plan <- plan_col_label_merges(x$col_label_merges, visible_cols)
    i <- 1L
    while (i <= length(visible_cols)) {
      cc <- visible_cols[[i]]
      m <- merge_plan$at[[i]]
      if (!is.null(m)) {
        # Combine styles from every column the merge covers (last write wins).
        merged_css <- character()
        for (mc in m$columns) {
          merged_css <- combine_css(merged_css, style_maps$column_labels[[mc]])
        }
        merged_css <- combine_css(merged_css, c("text-align" = m$align))
        label_cells <- c(
          label_cells,
          list(
            shiny::tags$th(
              class = "ntrd-col-label",
              colspan = length(m$columns),
              style = css_to_inline(merged_css),
              m$label
            )
          )
        )
        i <- i + length(m$columns)
      } else {
        lbl <- x$col_labels[[cc]] %||% cc
        label_cells <- c(
          label_cells,
          list(
            shiny::tags$th(
              class = "ntrd-col-label",
              style = css_to_inline(style_maps$column_labels[[cc]]),
              lbl
            )
          )
        )
        i <- i + 1L
      }
    }
    thead_rows <- c(
      thead_rows,
      list(
        shiny::tags$tr(class = "ntrd-col-label-row", label_cells)
      )
    )

    thead <- shiny::tags$thead(thead_rows)
  }

  # 5. Build <tbody> with group-header rows interleaved.
  group_for_row <- if (is.null(x$groupname_col)) {
    rep(NA_character_, nrow(data))
  } else {
    as.character(data[[x$groupname_col]])
  }
  ncol_total <- length(visible_cols) + (!is.null(x$rowname_col))

  body_rows <- list()
  prev_group <- NA_character_

  for (i in seq_len(nrow(data))) {
    grp <- group_for_row[[i]]

    # Emit a group header whenever the group label changes (and is not NA).
    if (!isTRUE(prev_group == grp) && !is.na(grp)) {
      body_rows <- c(
        body_rows,
        list(
          shiny::tags$tr(
            class = "ntrd-row-group",
            shiny::tags$th(
              class = "ntrd-row-group-label",
              colspan = ncol_total,
              scope = "colgroup",
              style = css_to_inline(style_maps$row_groups[[grp]]),
              grp
            )
          )
        )
      )
    }
    prev_group <- if (is.na(grp)) NA_character_ else grp

    cells <- list()
    if (!is.null(x$rowname_col)) {
      cells <- c(
        cells,
        list(
          shiny::tags$th(
            class = "ntrd-stub",
            scope = "row",
            style = css_to_inline(style_maps$stub[[i]]),
            as.character(data[[x$rowname_col]][[i]])
          )
        )
      )
    }
    for (cc in visible_cols) {
      val <- data[[cc]][[i]]
      # Pass-through for HTML / tag values; everything else becomes a string.
      inner <- if (
        inherits(val, "html") ||
          inherits(val, "shiny.tag") ||
          inherits(val, "shiny.tag.list")
      ) {
        val
      } else if (length(val) == 1L && is.na(val)) {
        ""
      } else {
        as.character(val)
      }
      cells <- c(
        cells,
        list(shiny::tags$td(
          class = "ntrd-body",
          style = css_to_inline(style_maps$body[[cc]][[i]]),
          inner
        ))
      )
    }

    body_rows <- c(
      body_rows,
      list(
        shiny::tags$tr(class = "ntrd-body-row", cells)
      )
    )
  }

  tbody <- shiny::tags$tbody(body_rows)

  # 6. Build <tfoot>, if any footnotes were added. Footnote-level styles
  #    apply uniformly to each footnote cell.
  tfoot <- NULL
  if (length(x$footnotes) > 0L) {
    fn_style <- css_to_inline(style_maps$footnotes)
    fn_rows <- lapply(x$footnotes, function(fn) {
      shiny::tags$tr(
        class = "ntrd-footnote-row",
        shiny::tags$td(
          class = "ntrd-footnote",
          colspan = ncol_total,
          style = fn_style,
          fn
        )
      )
    })
    tfoot <- shiny::tags$tfoot(fn_rows)
  }

  # 7. Wrap in <table>.
  table_tag <- shiny::tags$table(
    id = x$id,
    class = "ntrd-table",
    thead,
    tbody,
    tfoot
  )

  # 8. Bundle with the default stylesheet and any per-table scoped CSS.
  #    `shiny::singleton()` ensures the base <style> block is emitted only
  #    once per page even if multiple tables are rendered. Per-table CSS is
  #    not deduped (it's scoped to a unique id anyway) and is emitted
  #    immediately before the <table>, so it travels with the markup.
  custom_style <- if (length(x$custom_css)) {
    shiny::tags$style(
      type = "text/css",
      shiny::HTML(paste(x$custom_css, collapse = "\n\n"))
    )
  }

  shiny::tagList(
    ht_styles_tag(),
    custom_style,
    table_tag
  )
}


# Prepend `#<id> ` to every selector in `css`. Comma-separated selector
# lists are split and prefixed individually. `@media`, `@supports`, and
# `@container` rules recurse into their bodies; other `@`-rules pass
# through unchanged (so `@keyframes`, `@font-face`, etc. still work).
#
# The parser strips /* ... */ comments first and then walks the string
# tracking brace depth, so nested rules and selectors containing braces in
# strings are *not* fully supported — but they're rare in hand-written
# table CSS and well outside the scope of this helper.
scope_css <- function(css, id) {
  css <- gsub("/\\*.*?\\*/", "", css, perl = TRUE)
  rules <- parse_css_rules(css)
  if (length(rules) == 0L) {
    return("")
  }

  pieces <- vapply(
    rules,
    function(rule) {
      sel <- trimws(rule$selector)
      body <- rule$body
      if (grepl("^@(media|supports|container)\\b", sel)) {
        paste0(sel, " {\n", scope_css(body, id), "\n}")
      } else if (startsWith(sel, "@")) {
        # Pass-through (keyframes, font-face, import, ...).
        paste0(sel, " {", body, "}")
      } else {
        sels <- trimws(strsplit(sel, ",", fixed = TRUE)[[1]])
        sels <- sels[nzchar(sels)]
        # `&` (SCSS-style / native CSS nesting convention) refers to the
        # table itself: replace it with `#id` directly, no descendant
        # combinator. Everything else gets the standard `#id ` prefix.
        scoped <- vapply(
          sels,
          function(s) {
            if (grepl("&", s, fixed = TRUE)) {
              gsub("&", paste0("#", id), s, fixed = TRUE)
            } else {
              paste0("#", id, " ", s)
            }
          },
          character(1),
          USE.NAMES = FALSE
        )
        paste0(paste(scoped, collapse = ", "), " {", body, "}")
      }
    },
    character(1)
  )

  paste(pieces, collapse = "\n")
}

# Split a CSS string into (selector, body) pairs by walking the string and
# tracking brace depth. Returns a list of lists.
parse_css_rules <- function(css) {
  chars <- strsplit(css, "", fixed = TRUE)[[1]]
  n <- length(chars)
  rules <- list()
  i <- 1L
  while (i <= n) {
    # Skip leading whitespace.
    while (i <= n && chars[[i]] %in% c(" ", "\t", "\n", "\r")) {
      i <- i + 1L
    }
    if (i > n) {
      break
    }

    # Collect selector up to the next top-level '{'.
    sel_start <- i
    while (i <= n && chars[[i]] != "{") {
      i <- i + 1L
    }
    if (i > n) {
      break
    } # malformed: no opening brace

    selector <- paste(chars[sel_start:(i - 1L)], collapse = "")
    i <- i + 1L # step past '{'

    # Collect body to the matching '}'.
    depth <- 1L
    body_start <- i
    while (i <= n && depth > 0L) {
      ch <- chars[[i]]
      if (ch == "{") {
        depth <- depth + 1L
      } else if (ch == "}") {
        depth <- depth - 1L
      }
      i <- i + 1L
    }
    body <- if (i - 2L >= body_start) {
      paste(chars[body_start:(i - 2L)], collapse = "")
    } else {
      ""
    }
    rules[[length(rules) + 1L]] <- list(selector = selector, body = body)
  }
  rules
}


# Inline default stylesheet, wrapped in singleton + head so it is emitted
# at most once per page. Defined as a function (rather than a top-level
# tagList) so it lazy-builds at render time.
ht_styles_tag <- function() {
  shiny::singleton(
    shiny::tags$head(
      shiny::tags$style(type = "text/css", shiny::HTML(ntrd_table_css))
    )
  )
}


# Default stylesheet. Aims to approximate {gt}'s default look so that
# tables built with `ht_table()` slot in next to existing gt output
# without visual surprise. Override per-table via `id` selectors, or
# replace wholesale by editing this string.
ntrd_table_css <- r"(
.ntrd-table {
  font-family: Arial, "Helvetica Neue", Helvetica, sans-serif;
  font-size: 14px;
  line-height: 1.4;
  color: #333;
  background-color: #fff;
  border-collapse: collapse;
  border-spacing: 0;
  margin: 0;
  border-top: 2px solid #5f5f5f;
  border-bottom: 2px solid #5f5f5f;
}

.ntrd-table th,
.ntrd-table td {
  padding: 4px 10px;
  vertical-align: middle;
}

/* --- Spanner row ------------------------------------------------------- */
.ntrd-table .ntrd-spanner {
  font-weight: bold;
  text-align: center;
  border-bottom: 1px solid #d3d3d3;
  padding-top: 6px;
  padding-bottom: 4px;
}

/* Blank cells in the spanner row (over the stub or non-spanned columns)
   should not pick up the spanner's underline. */
.ntrd-table .ntrd-spanner-row .ntrd-no-spanner,
.ntrd-table .ntrd-spanner-row > th:empty {
  border-bottom: 0;
  padding-top: 0;
  padding-bottom: 0;
}

/* --- Column labels ----------------------------------------------------- */
.ntrd-table .ntrd-col-label {
  font-weight: bold;
  text-align: center;
  vertical-align: bottom;
  border-bottom: 2px solid #5f5f5f;
  padding-top: 8px;
  padding-bottom: 6px;
}

.ntrd-table .ntrd-stubhead {
  font-weight: bold;
  text-align: left;
  vertical-align: bottom;
  border-bottom: 2px solid #5f5f5f;
  padding-top: 8px;
  padding-bottom: 6px;
}

/* --- Row group --------------------------------------------------------- */
.ntrd-table .ntrd-row-group-label {
  font-weight: bold;
  font-style: italic;
  text-align: left;
  background-color: #f4f4f4;
  border-top: 1px solid #d3d3d3;
  border-bottom: 1px solid #d3d3d3;
  padding-top: 6px;
  padding-bottom: 6px;
}

/* --- Stub (row-label) cells ------------------------------------------- */
.ntrd-table .ntrd-stub {
  font-weight: normal;
  text-align: left;
  white-space: nowrap;
  padding-right: 16px;
}

/* --- Body cells -------------------------------------------------------- */
.ntrd-table .ntrd-body {
  text-align: left;
}

/* --- Footnotes --------------------------------------------------------- */
.ntrd-table .ntrd-footnote {
  font-size: 0.85em;
  text-align: left;
  vertical-align: top;
  white-space: normal;
  border-top: 1px solid #d3d3d3;
  padding-top: 4px;
  padding-bottom: 4px;
}
)"
