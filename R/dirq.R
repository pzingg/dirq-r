#' @title Create a message queue.
#' @description See the README at
#'   [https://github.com/pzingg/dirq-r](https://github.com/pzingg/dirq-r)
#'   and the examples in this help file for instructions.
#' @export
#' @param path Character string giving the top-level directory
#'   path of the queue.
#' @param umask Integer umask to use when creating files and
#'   directories (default: use the running process' umask)
#' @param rndhex Integer hexadecimal digit (an integer in the
#'   range 0..15) to use in names (default: randomly chosen)
#' @param granularity Integer time granularity for intermediate
#'   directories (default: 60)
#' @examples
#'   path <- tempfile() # Define a path to your queue.
#'   q <- dirq(path) # Create a new queue or recover an existing one.
#'   # Let's say two parallel processes (A and B) are sharing this queue.
#'   # Process A sends Process B some messages.
#'   # You can send character vectors or anything that can be serialized
#'   # using jsonlite.
#'   q$add("process B.")
#'   q$add(list(title = "Calculate", message = "sqrt(4)"), format = "json")
#'   # See your queued messages.
#'   q$count() # Number of messages in the queue.
#'   # Now, let's assume process B comes online. It can consume
#'   # some messages.
#'   # Create a cursor for iterating the queue.
#'   first <- q$iter_first()
#'   # Peek at the first message
#'   if (q$lock(first)) {
#'     data <- q$get(first)
#'     q$unlock(first)
#'     cat(data)
#'     # Advance to the next message
#'     cursor <- q$iter_next(first)
#'   }
#'   # Refresh an element to prevent it from being purged
#'   if (q$lock(first)) {
#'     q$touch(first)
#'     q$unlock(first)
#'   }
#'   # Remove an element
#'   if (q$lock(first)) {
#'     q$remove(first)
#'   }
#'   # Clean up lock and temp file elements older than 1 second
#'   q$purge(maxlock = 1, maxtemp = 1)
dirq <- function(path, umask = NULL,
    rndhex = NULL, granularity = 60) {
  R6_dirq$new(path, umask = umask,
    rndhex = rndhex, granularity = granularity)
}

#' @title R6 class for `dirq` objects
#' @description See the [`dirq()`] function for full documentation and usage.
#' @seealso dirq
#' @export
R6_dirq <- R6::R6Class(
  classname = "R6_dirq",
  private = list(
    id_ = character(0),
    path_ = character(0),
    umask_ = NULL,
    rndhex_ = NULL,
    granularity_ = 60,
    dirq_establish = function(path, umask, rndhex, granularity) {
      dir_create_umask(path, umask)
      if (is.integer(umask)) {
        private$umask_ <- umask
      }
      private$path_ <- path

      if (is.integer(rndhex)) {
        private$rndhex_ <- rndhex %% 16
      } else {
        private$rndhex_ <- sample(0:15, 1, replace = FALSE)
      }
      if (is.integer(granularity)) {
        private$granularity_ <- granularity
      }
      info <- fs::file_info(path, fail = FALSE)
      private$id_ <- paste0(as.character(info$device_id),
        ":", as.character(info$inode))
    },
    dirq_count = function() {
      acc <- 0
      for (dir in get_intermediate_dirs(private$path_)) {
        files <- private$get_element_files(file.path(private$path_, dir))
        acc <- acc + length(files)
      }
      acc
    },
    dirq_add = function(data, format) {
      elem <- private$add_data(data, format)
      private$link_path(elem$tmp, elem$dir)
    },
    dirq_get_path = function(name) {
      file.path(private$path_, paste0(name, ".lck"))
    },
    dirq_lock = function(name, permissive) {
      elem_path <- file.path(private$path_, name)
      if (lock_create(elem_path, permissive)) {
        file_touch(elem_path, permissive)
      } else {
        FALSE
      }
    },
    dirq_unlock = function(name, permissive) {
      lock_path <- private$dirq_get_path(name)
      if (permissive) {
        tryCatch({
          fs::file_delete(lock_path)
          TRUE
        }, error = FALSE)
      } else {
        fs::file_delete(lock_path)
        TRUE
      }
    },
    dirq_get = function(name, format) {
      file_read(private$dirq_get_path(name), format)
    },
    dirq_remove = function(name) {
      elem_path = file.path(private$path_, name)
      suppressWarnings(file.remove(elem_path))
      suppressWarnings(file.remove(paste0(elem_path, ".lck")))
    },
    dirq_touch = function(name) {
      elem_path = file.path(private$path_, name)
      file_touch(elem_path, permissive = TRUE)
    },
    dirq_first = function() {
      private$dirq_next("00000000/00000000000000")
    },
    dirq_next = function(cursor) {
      splt <- strsplit(cursor, "/", fixed = TRUE)[[1]]
      dir_seen <- splt[[1]]
      file_seen <- splt[[2]]
      dirs <- sort(get_intermediate_dirs(private$path_, dir_seen))
      get_next_element_file(private$path_, dirs, dir_seen, file_seen)
    },
    dirq_purge = function(maxtemp, maxlock) {
      now <- trunc(as.numeric(Sys.time()))
      oldtemp <- ifelse(maxtemp > 0, now - maxtemp, 0)
      oldlock <- ifelse(maxlock > 0, now - maxlock, 0)
      dirs <- get_intermediate_dirs(private$path_)
      private$remove_stale_elements(dirs, oldtemp, oldlock)
      private$purge_empty_directories(dirs)
    },
    add_data = function(data, format) {
      dir <- private$new_dir_name()
      name <- private$new_elem_name()
      path <- file.path(private$path_, dir, paste0(name, ".tmp"))
      file_write_umask(path, private$umask_, data, format)
      list(tmp = path, dir = dir)
    },
    add_path = function(path) {
      dir <- private$new_dir_name()
      private$dir_create(file.path(private$path_, dir))
      link_path(queue, path, dir)
    },
    link_path = function(tmp, dir) {
      name <- private$new_elem_name()
      path <- file.path(private$path_, dir, name)
      fs::link_create(tmp, path, symbolic = FALSE)
      fs::file_delete(tmp)
      file.path(dir, name)
    },
    new_dir_name = function() {
      now <- trunc(as.numeric(Sys.time()))
      if (private$granularity_ > 1) {
        now <- now - (now %% private$granularity_)
      }
      format(as.hexmode(now), 8, upper.case = TRUE)
    },
    new_elem_name = function() {
      now <- trunc(as.numeric(Sys.time()) * 1000000.0)
      secs <- format(as.hexmode(now %/% 1000000), 8, upper.case = TRUE)
      msecs <- format(as.hexmode(now %% 1000000), 5, upper.case = TRUE)
      rnd <- format(as.hexmode(private$rndhex_), 1, upper.case = TRUE)
      paste0(secs, msecs, rnd)
    },
    get_element_files = function(int_dir) {
      list.files(int_dir,
        pattern = "^[0-9a-f]{14}", ignore.case = TRUE,
        full.names = FALSE, include.dirs = FALSE)
    },
    remove_stale_elements = function(dirs, oldtemp, oldlock) {
      # Remove stale temporary or locked elements
      for (dir in dirs) {
        dir_path <- file.path(private$path_, dir)
        temp_lock_elems <- list.files(dir_path,
          pattern = "(\\.tmp|\\.lck)$", ignore.case = TRUE,
          full.names = FALSE, include.dirs = FALSE)
        for (old in temp_lock_elems) {
          old_path <- file.path(dir_path, old)
          info <- fs::file_info(old_path)
          mtime <- trunc(as.numeric(info$modification_time))
          if ((grepl("\\.tmp$", old, ignore.case = TRUE) && mtime < oldtemp) ||
            (grepl("\\.lck$", old, ignore.case = TRUE) && mtime < oldlock)) {
            fs::file_delete(old_path)
          }
        }
      }
      invisible()
    },
    purge_empty_directories = function(dirs) {
      # Try to remove all but the last intermediate directory
      dirs <- sort(dirs, decreasing = TRUE)
      dirs <- dirs[-1]
      for (dir in dirs) {
        path <- file.path(private$path_, dir)
        suppressMessages(file.delete(path))
      }
      invisible()
    }
  ),
  public = list(
    #' @description Initialize a `dirq`.
    #' @param path Character string giving the top-level directory
    #'   path of the queue.
    #' @param umask Integer umask to use when creating files and
    #'   directories (default: use the running process' umask)
    #' @param rndhex Integer hexadecimal digit (an integer in the
    #'   range 0..15) to use in names (default: randomly chosen)
    #' @param granularity Integer time granularity for intermediate
    #'   directories (default: 60)
    initialize = function(path, umask = NULL, rndhex = NULL, granularity = 60) {
      private$dirq_establish(path, umask, rndhex, granularity)
    },
    #' @description Get the top-level directory for the queue.
    path = function() {
      private$path_
    },
    #' @description Get the number of messages in the queue.
    count = function() {
      private$dirq_count()
    },
    #' @description Add an element to the queue.
    #' @param data string or object than can be JSON serialized
    #' @param format Character
    add = function(data, format = c("utf8", "json", "binary")) {
        format <- match.arg(format)
        private$dirq_add(data, format)
    },
    #' @description Lock an element.
    #' @param name Character name of the element
    #' @param permissive Logical if `TRUE` it is not an
    #'   error if the element cannot be locked
    lock = function(name, permissive = TRUE) {
      private$dirq_lock(name, permissive)
    },
    #' @description Attempts to unlock an element.
    #' @param name Character name of the element
    #' @param permissive Logical if `TRUE` it is not an
    #'   error if the element cannot be unlocked
    unlock = function(name, permissive = FALSE) {
      private$dirq_unlock(name, permissive)
    },
    #' @description Get the content of a locked element.
    #' @param name Character name of the element
    #' @param format Character
    get = function(name, format = c("utf8", "json", "binary")) {
      format <- match.arg(format)
      private$dirq_get(name, format)
    },
    #' @description Remove a locked element.
    #' @param name Character name of the element
    remove = function(name) {
      private$dirq_remove(name)
    },
    #' @description Touch a locked element.
    #' @param name Character name of the element
    touch = function(name) {
      private$dirq_touch(name)
    },
    #' @description Find the first element in the queue.
    iter_first = function() {
      private$dirq_first()
    },
    #' @description Find the next element in the queue.
    #' @param cursor Character name of element last seen
    iter_next = function(cursor) {
      private$dirq_next(cursor)
    },
    #' @description Purge stale elements and directories.
    #' @param maxtemp Integer maximum time for a temporary element
    #'   (in seconds, default: 300); if set to 0, temporary elements
    #'   will not be removed
    #' @param maxlock Integer maximum time for a locked element
    #'   (in seconds, default: 600); if set to 0, locked elements
    #'   will not be unlocked
    purge = function(maxtemp = 300, maxlock = 600) {
      private$dirq_purge(maxtemp, maxlock)
    }
  )
)
