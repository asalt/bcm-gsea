myzscore <- function(value, minval = NA, remask = TRUE) {
    mask <- is.na(value)
    if (is.na(minval)) minval <- min(value, na.rm = TRUE)

    if (minval == Inf) {
        minval <- 0
    }

    value[is.na(value)] <- minval
    # todo make smaller than min val
    out <- scale(value)

    # if all NA:
    if (sum(!is.finite(out)) == length(out)) {
        out[, 1] <- 0
    }

    if (remask == TRUE) {
        out[mask] <- NA
    }
    return(out)
}

dist_no_na <- function(mat) {
    mat[is.na(mat)] <- min(mat, na.rm = TRUE)
    edist <- dist(mat)
    return(edist)
}
