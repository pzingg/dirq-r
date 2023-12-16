
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dirq - A Filesystem Directory Based Queue

A port of Perl module \[Directory::Queue::Simple\]
(<http://search.cpan.org/dist/Directory-Queue/>) The goal of this
package is to offer a simple queue system using the underlying
filesystem for storage, security and to prevent race conditions via
atomic operations.

A simple producer:

``` r
# Define a path to your queue
# In production, you wouldn't use a temporary directory!
path <- tempfile()
# Create a new queue or recover an existing one
q <- dirq(path)
cat(paste("top level directory is", q$path(), "\n"))
#> top level directory is /tmp/RtmpJDLM7b/file29f13413268dd
# Add some messages
for (i in seq.int(1, 6)) {
  name <- q$add(list(data = paste("element", i)), format = "json")
  cat(paste("added element", name, "\n"))
}
#> added element 657CEE98/657CEE9A75DCE4 
#> added element 657CEE98/657CEE9A76AFC4 
#> added element 657CEE98/657CEE9A775C24 
#> added element 657CEE98/657CEE9A784004 
#> added element 657CEE98/657CEE9A790224 
#> added element 657CEE98/657CEE9A79B3A4
```

A simple consumer:

``` r
# Recover the previous queue
q <- dirq(path)
# Get the first element in the queue
cursor <- q$iter_first()
while (!is.null(cursor)) {
  if (q$lock(cursor)) {
    cat(paste("reading element", cursor, "\n"))
    data <- q$get(cursor, format = "json")
    q$remove(cursor)
    # Or use q$unlock(cursor) to just peek at data
  }
  # Get the next element in the queue
  cursor <- q$iter_next(cursor)
}
#> reading element 657CEE98/657CEE9A75DCE4 
#> reading element 657CEE98/657CEE9A76AFC4 
#> reading element 657CEE98/657CEE9A775C24 
#> reading element 657CEE98/657CEE9A784004 
#> reading element 657CEE98/657CEE9A790224 
#> reading element 657CEE98/657CEE9A79B3A4
```

# Similar work

## txtq

[Will Landau](https://github.com/wlandau)’s
[`txtq`](https://github.com/wlandau/txtq) is also a pure
filesystem-based queue.

## liteq

[Gábor Csárdi](https://github.com/gaborcsardi)’s
[`liteq`](https://github.com/r-lib/liteq) package offers essentially the
same functionality implemented with SQLite. It has a some additional
features, such as the ability to detect crashed workers and re-queue
failed messages, but it was in an early stage of development at the time
`dirq` was released.

## Other message queues

There is a plethora of message queues beyond R, most notably
[ZeroMQ](https://zeromq.org) and [RabbitMQ](https://www.rabbitmq.com/).
In fact, [Jeroen Ooms](https://github.com/jeroen) and [Whit
Armstrong](https://github.com/armstrtw) maintain
[`rzmq`](https://github.com/ropensci/rzmq), a package to work with
[ZeroMQ](https://zeromq.org) from R. Even in this landscape, `dirq` has
advantages.

1.  The `dirq` user interface is friendly, and its internals are simple.
    No prior knowledge of sockets or message-passing is required.
2.  `dirq` is lightweight, R-focused, and easy to install. It only
    depends on R and a few packages on
    [CRAN](https://cran.r-project.org).
3.  Because `dirq` it is file-based, the queue persists even if your
    work crashes.
