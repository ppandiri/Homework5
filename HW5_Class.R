setClass(
  Class = "sparse_numeric",
  slots = c(
    value = "numeric",
    pos = "integer",
    length = "integer"
  )
)

setValidity("sparse_numeric", function(object) {
  if (length(object@length) != 1L)
    return("The length slot must be a single integer.")
  if (object@length < 0L)
    return("The length must be non-negative.")
  if (length(object@value) != length(object@pos))
    return("The value and pos slots must have the same length.")
  if (length(object@pos) > 0L) {
    if (any(object@pos < 1L) || any(object@pos > object@length))
      return("Positions must fall between 1 and the overall length.")
    if (any(diff(object@pos) <= 0L))
      return("Positions must be strictly increasing with no duplicates.")
  }
  TRUE
})

setAs("numeric", "sparse_numeric", function(from) {
  nz_idx <- which(from != 0)
  new("sparse_numeric",
      value = from[nz_idx],
      pos = as.integer(nz_idx),
      length = as.integer(length(from)))
})

setAs("sparse_numeric", "numeric", function(from) {
  out <- numeric(from@length)
  if (length(from@pos) > 0L) out[from@pos] <- from@value
  out
})

if (!isGeneric("sparse_add")) setGeneric("sparse_add", function(x, y) standardGeneric("sparse_add"))
if (!isGeneric("sparse_mult")) setGeneric("sparse_mult", function(x, y) standardGeneric("sparse_mult"))
if (!isGeneric("sparse_sub")) setGeneric("sparse_sub", function(x, y) standardGeneric("sparse_sub"))
if (!isGeneric("sparse_crossprod")) setGeneric("sparse_crossprod", function(x, y) standardGeneric("sparse_crossprod"))

setMethod("sparse_add",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y) {
            if (x@length != y@length) stop("Sparse vectors must be the same length.")
            all_pos <- sort(unique(c(x@pos, y@pos)))
            xv <- numeric(length(all_pos))
            yv <- numeric(length(all_pos))
            xv[match(x@pos, all_pos)] <- x@value
            yv[match(y@pos, all_pos)] <- y@value
            val <- xv + yv
            keep <- val != 0
            new("sparse_numeric",
                value = val[keep],
                pos = as.integer(all_pos[keep]),
                length = x@length)
          }
)

setMethod("sparse_sub",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y) {
            if (x@length != y@length) stop("Sparse vectors must be the same length.")
            all_pos <- sort(unique(c(x@pos, y@pos)))
            xv <- numeric(length(all_pos))
            yv <- numeric(length(all_pos))
            xv[match(x@pos, all_pos)] <- x@value
            yv[match(y@pos, all_pos)] <- y@value
            val <- xv - yv
            keep <- val != 0
            new("sparse_numeric",
                value = val[keep],
                pos = as.integer(all_pos[keep]),
                length = x@length)
          }
)

setMethod("sparse_mult",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y) {
            if (x@length != y@length) stop("Sparse vectors must be the same length.")
            common_pos <- intersect(x@pos, y@pos)
            if (length(common_pos) == 0L)
              return(new("sparse_numeric", value = numeric(0), pos = integer(0), length = x@length))
            xv <- x@value[match(common_pos, x@pos)]
            yv <- y@value[match(common_pos, y@pos)]
            val <- xv * yv
            keep <- val != 0
            new("sparse_numeric",
                value = val[keep],
                pos = as.integer(common_pos[keep]),
                length = x@length)
          }
)

setMethod("sparse_crossprod",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y) {
            if (x@length != y@length) stop("Sparse vectors must be the same length.")
            common_pos <- intersect(x@pos, y@pos)
            if (length(common_pos) == 0L) return(0)
            xv <- x@value[match(common_pos, x@pos)]
            yv <- y@value[match(common_pos, y@pos)]
            sum(xv * yv)
          }
)

setMethod("+", signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_add(e1, e2))
setMethod("-", signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_sub(e1, e2))
setMethod("*", signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_mult(e1, e2))

setMethod("show", "sparse_numeric", function(object) {
  cat("An object of class sparse_numeric\n")
  cat("Length:", object@length, "\n")
  cat("Non-zero entries:", length(object@value), "\n")
  if (length(object@value) > 0L) {
    df <- data.frame(pos = object@pos, value = object@value)
    print(df, row.names = FALSE)
  }
})

setMethod("plot",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) {
            if (x@length != y@length) stop("Sparse vectors must be the same length.")
            common_pos <- intersect(x@pos, y@pos)
            if (length(common_pos) == 0L) {
              plot(NA, xlim = c(0, 1), ylim = c(0, 1),
                   xlab = "x values", ylab = "y values",
                   main = "No overlapping non-zero elements")
              return(invisible(NULL))
            }
            xv <- x@value[match(common_pos, x@pos)]
            yv <- y@value[match(common_pos, y@pos)]
            plot(xv, yv,
                 xlab = "x values (overlap)",
                 ylab = "y values (overlap)",
                 main = "Overlapping non-zero elements", ...)
            invisible(NULL)
          }
)

setMethod("length", "sparse_numeric", function(x) x@length)