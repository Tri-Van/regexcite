# Test Next in traceback

x<- 1
#use red dot instead of browser(), replace x <-10
a <- x+1
b <- x+2
c <- x+3

trace(print, quote(if (is.numeric(x) && x >= 3) cat("hi\n")), print = FALSE)

# from https://adv-r.hadley.nz/debugging.html
f <- function(a) g(a)
g <- function(b) h(b)
h <- function(c) i(c)
i <- function(d) {
  if (!is.numeric(d)) {
    stop("`d` must be numeric", call. = FALSE)
  }
  d + 10
}
