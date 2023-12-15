dir_create_umask <- function(path, umask) {
  if (is.integer(umask)) {
    old_umask <- Sys.umask(umask)
    fs::dir_create(path)
    Sys.umask(old_mask)
  } else {
    fs::dir_create(path)
  }
}

file_create_umask <- function(path, umask) {
  if (is.null(umask)) {
    fs::dir_create(dirname(path))
    fs::file_create(path)
  } else {
    old_umask <- Sys.umask(umask)
    fs::dir_create(dirname(path))
    fs::file_create(path)
    Sys.umask(old_mask)
  }
}

file_write_umask <- function(path, umask, data, format) {
  file_create_umask(path, umask)
  if (format == "json") {
    jsonlite::write_json(data, path)
  } else {
    cat(data, file = path)
  }
  invisible()
}

file_read <- function(path, format) {
  if (format == "json") {
    data <- jsonlite::read_json(path)
  } else {
    data <- readLines(path)
  }
  data
}
