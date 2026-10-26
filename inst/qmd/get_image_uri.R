get_image_uri <- function(file) {
  image_raw <- lapply(file, FUN = function(x) {
    readBin(con = x, what = "raw", n = file.info(x)$size)
  })

  vapply(
    seq_along(image_raw),
    FUN.VALUE = character(1L),
    USE.NAMES = FALSE,
    FUN = function(x) {
      file <- file[x]
      pos <- regexpr("\\.([[:alnum:]]+)$", file)
      extension <- tolower(ifelse(pos > -1L, substring(file, pos + 1L), ""))

      file <- switch(
        extension,
        svg = "image/svg+xml",
        jpg = "image/jpeg",
        paste("image", extension, sep = "/")
      )
      paste0(
        "data:",
        file,
        ";base64,",
        base64enc::base64encode(image_raw[[x]])
      )
    }
  )
}
