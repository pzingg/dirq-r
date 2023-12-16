# utilities

#' @noRd
dir_create_umask <- function(path, umask) {
  if (is.integer(umask)) {
    old_umask <- Sys.umask(umask)
    fs::dir_create(path)
    Sys.umask(old_umask)
  } else {
    fs::dir_create(path)
  }
}

#' @noRd
file_create_umask <- function(path, umask) {
  if (is.null(umask)) {
    fs::dir_create(dirname(path))
    fs::file_create(path)
  } else {
    old_umask <- Sys.umask(umask)
    fs::dir_create(dirname(path))
    fs::file_create(path)
    Sys.umask(old_umask)
  }
}

#' @noRd
lock_create <- function(path, permissive) {
  lock_path <- paste0(path, ".lck")
  if (permissive) {
    tryCatch({
      fs::link_create(path, lock_path, symbolic = FALSE)
      TRUE
    }, error = FALSE)
  } else {
    fs::link_create(path, lock_path, symbolic = FALSE)
    TRUE
  }
}

#' @noRd
file_touch <- function(path, permissive) {
  if (permissive) {
    tryCatch({
      fs::file_touch(path)
      TRUE
    }, error = FALSE)
  } else {
      fs::file_touch(path)
      TRUE
  }
}

#' @noRd
file_write_umask <- function(path, umask, data, format) {
  file_create_umask(path, umask)
  if (format == "json") {
    jsonlite::write_json(data, path)
  } else if (format == "utf8") {
    cat(enc2utf8(data), file = path, sep = "\n", append = FALSE)
  } else {
    cat(data, file = path, sep = "\n", append = FALSE)
  }
  invisible()
}

#' @noRd
file_read <- function(path, format) {
  if (format == "json") {
    data <- jsonlite::read_json(path)
  } else if (format == "utf8") {
    data <- readLines(path, encoding = "UTF-8")
  } else {
    data <- readLines(path)
  }
  data
}

#' @noRd
get_intermediate_dirs = function(qpath, seen = NULL, greater_than = FALSE) {
  dirs <- list.files(qpath,
    pattern = "^[0-9a-f]{8}$", ignore.case = TRUE,
    full.names = FALSE, include.dirs = TRUE)
  if (is.null(seen)) {
    dirs
  } else if (greater_than) {
    dirs[dirs > seen]
  } else {
    dirs[dirs >= seen]
  }
}

#' @noRd
get_next_element_file <- function(qpath, dirs, dir_seen, file_seen) {
  if (length(dirs) == 0) {
    return(NULL)
  }
  h <- dirs[[1]]
  files <- list.files(file.path(qpath, h), pattern = "^[0-9a-f]{14}$",
    ignore.case = TRUE, full.names = FALSE, include.dirs = FALSE)
  for (name in sort(files)) {
    if (h > dir_seen) {
      return(paste0(h, "/", name))
    }
    if (name > file_seen) {
      return(paste0(h, "/", name))
    }
  }

  # Try next directory
  dirs <- get_intermediate_dirs(qpath, dir_seen, greater_than = TRUE)
  get_next_element_file(qpath, dirs, dir_seen, file_seen)
}
