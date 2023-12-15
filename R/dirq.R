#' @title Create a message queue.
#' @description See the README at
#'   [https://github.com/pzingg/dirq-r](https://github.com/pzingg/dirq-r)
#'   and the examples in this help file for instructions.
#' @export
#' @param path Character string giving the file path of the queue.
#'   The `dirq()` function creates a folder at this path to store
#'   the messages.
#' @param umask Integer
#' @param rndhex Integer
#' @param granularity Integer
#' @examples
#'   path <- tempfile() # Define a path to your queue.
#'   q <- dirq(path) # Create a new queue or recover an existing one.
#'   # Let's say two parallel processes (A and B) are sharing this queue.
#'   # Process A sends Process B some messages.
#'   # You can send character vectors or anything that can be serialized
#'   # using jsonlite.
#'   q$add("process B.")
#'   q$add(list(title = "Calculate", message = "sqrt(4)")), mode="json")
#'   # See your queued messages.
#'   q$count() # Number of messages in the queue.
#'   q$purge()
#'   # Now, let's assume process B comes online. It can consume
#'   # some messages, locking the queue so process A does not
#'   # mess up the data.
#'   cursor <- q$next() # Creates a cursor for iterating the queue.
#'   # Peek at the first message
#'   if (q$unlock(cursor)) {
#'     data <- q$get(cursor)
#'     q$lock(cursor)
#'     cat(data)
#'   }
dirq <- function(path, umask = NULL,
    rndhex = NULL, granularity = 60) {
  R6_dirq$new(path, umask = umask,
    rndhex = rndhex, granularity = granularity)
}

#' @title R6 class for `dirq` objects
#' @description See the [dirq()] function for full documentation and usage.
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
      purrr::accumulate(private$get_intermediate_dirs(), .init = 0,
        .f = function(acc, dir) {
          files <- private$get_element_files(file.path(private$path_, dir))
          acc + len(files)
      })
    },
    dirq_add = function(data, format) {
      elem <- private$add_data(data, format)
      private$link_path(elem$tmp, elem$dir)
    },
    dirq_lock = function(name, permissive) {
      elem_path <- file.path(private$path_, name)
      if (private$create_lock(elem_path, permissive)) {
        private$touch(elem_path, permissive)
      } else {
        FALSE
      }
    },
    dirq_unlock = function(name, permissive) {
      lock_path <- private$dirq_get_path(name)
      fs::link_delete(lock_path)
      invisible()
    },
    dirq_get_path = function(name) {
      file.path(private$path_, paste0(name, ".lck"))
    },
    dirq_get = function(name, format) {
      file_read(private$dirq_get_path(name), format)
    },
    dirq_purge = function(maxtemp, maxlock) {
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
    lock_create = function(path, permissive) {
      fs::link_create(path, paste0(path, ".lck"), symbolic = FALSE)
      TRUE
    },
    update_utime = function(path, permissive) {
      fs::file_touch(path)
      TRUE
    },
    get_intermediate_dirs = function() {
      list.files(private$path_, pattern = "^[0-9a-f]{8}$",
        full.names = FALSE, include.dirs = TRUE)
    },
    get_element_files = function(int_dir) {
      list.files(int_dir, pattern = "^[0-9a-f]{14}",
        full.names = FALSE, include_dirs = FALSE)
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
    #' @param data
    #' @param mode Character
    add = function(data, mode = "utf8") {
        private$dirq_add(data, mode)
    },
    #' @description Purge stale elements and directories.
    #' @param maxtemp Integer maximum time for a temporary element
    #'   (in seconds, default: 300); if set to 0, temporary elements
    #'   will not be removed
    #' @param maxloc Integer maximum time for a locked element
    #'   (in seconds, default: 600); if set to 0, locked elements
    #'   will not be unlocked
    purge = function(maxtemp = 300, maxlock = 600) {
      private$dirq_purge(maxtemp, maxlock)
    }
  )
)
