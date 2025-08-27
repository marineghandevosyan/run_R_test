#' Arithmetic operations with safe recycling and checks
#'
#' These helpers wrap base arithmetic with:
#' - type checks (numeric only),
#' - controlled recycling (allow length 1 or equal lengths),
#' - informative errors (e.g., divide-by-zero).
#'
#' All functions are vectorized and return numeric vectors. MMM
#'
#' @param a,b Numeric vector(s).
#' @return Numeric vector.
#' @examples
#' add(1, 2)           # 3
#' subtract(5, 3)      # 2
#' multiply(2, 4)      # 8
#' divide(10, 2)       # 5
#' power(2, 3)         # 8
#' modulo(10, 3)       # 1

.check_numeric <- function(x, nm) {
  if (!is.numeric(x)) stop(sprintf("`%s` must be numeric.", nm), call. = FALSE)
  invisible(TRUE)
}

# allow equal length or length-1 recycling (safe & explicit)
.reconcile_lengths <- function(a, b) {
  la <- length(a); lb <- length(b)
  if (la == lb) return(list(a = a, b = b))
  if (la == 1)  return(list(a = rep(a, lb), b = b))
  if (lb == 1)  return(list(a = a, b = rep(b, la)))
  stop(sprintf(
    "Lengths must match or one side must be length 1 (got %d and %d).", la, lb
  ), call. = FALSE)
}

#' Add two numbers (vectorized)
#' @rdname arithmetic
#' @export
add <- function(a, b) {
  .check_numeric(a, "a"); .check_numeric(b, "b")
  args <- .reconcile_lengths(a, b)
  args$a + args$b
}

#' Subtract b from a (vectorized)
#' @rdname arithmetic
#' @export
subtract <- function(a, b) {
  .check_numeric(a, "a"); .check_numeric(b, "b")
  args <- .reconcile_lengths(a, b)
  args$a - args$b
}

#' Multiply two numbers (vectorized)
#' @rdname arithmetic
#' @export
multiply <- function(a, b) {
  .check_numeric(a, "a"); .check_numeric(b, "b")
  args <- .reconcile_lengths(a, b)
  args$a * args$b
}

#' Divide a by b (vectorized) with divide-by-zero check
#' @rdname arithmetic
#' @export
divide <- function(a, b) {
  .check_numeric(a, "a"); .check_numeric(b, "b")
  if (any(b == 0, na.rm = TRUE)) {
    stop("Division by zero detected in `b`.", call. = FALSE)
  }
  args <- .reconcile_lengths(a, b)
  args$a / args$b
}

#' Exponentiation a^b (vectorized)
#' @rdname arithmetic
#' @export
power <- function(a, b) {
  .check_numeric(a, "a"); .check_numeric(b, "b")
  args <- .reconcile_lengths(a, b)
  args$a ^ args$b
}

#' Modulo a %% b (vectorized) with zero-denominator check
#' @rdname arithmetic
#' @export
modulo <- function(a, b) {
  .check_numeric(a, "a"); .check_numeric(b, "b")
  if (any(b == 0, na.rm = TRUE)) {
    stop("Modulo by zero detected in `b`.", call. = FALSE)
  }
  args <- .reconcile_lengths(a, b)
  args$a %% args$b
}

