#' Taxonomic spell check
#'
#' A function to check for and count potential spelling variations of the same
#' taxon. Spelling variations are checked within alphabetical groups. To check
#' within higher taxonomic groups (e.g. "family", "order") instead, call
#' `tax_check()` via \code{\link{group_apply}} with `verbose = FALSE` (see
#' examples).
#'
#' @param taxdf \code{data.frame}. A dataframe with named columns containing
#' taxon names (e.g. "species", "genus").
#' NA values or empty strings in the name column (i.e. "" and " ")
#' are ignored.
#' @param name \code{character}. The column name of the taxon names you wish
#' to check (e.g. "genus").
#' @param dis \code{numeric}. The dissimilarity threshold: a value greater than
#' 0 (completely dissimilar), and less than 1 (completely similar).
#' Potential synonyms above this threshold are not returned.
#' This value is set to 0.05 by default, but the user might wish to experiment
#' with this value for their specific data.
#' @param start \code{numeric}. The number of shared characters at the
#' beginning of potential synonyms that should match. Potential synonyms below
#' this value will not be returned. By default this value is set to 1 (i.e.
#' the first letter of synonyms must match).
#' @param verbose \code{logical}. Should the results of the non-letter
#' character check be reported to the user? If `TRUE`, the result will only be
#' reported if such characters are detected in the taxon names.
#'
#' @return If verbose = `TRUE` (default), a \code{list} with two elements. The
#' first element in the list (synonyms) is a \code{data.frame} with each row
#' reporting a pair of potential synonyms. The first column "group" contains the
#' alphabetical grouping in which they occur. The second column "greater"
#' contains the most common synonym
#' in each pair. The third column "lesser" contains the least common synonym in
#' each pair. The third and fourth column (`count_greater`, `count_lesser`)
#' contain the respective counts of each synonym in a pair. If no matches were
#' found for the filtering arguments, this element is `NULL` instead. The second
#' element (`non_letter_name`) is a vector of taxon names which contain
#' non-letter characters, or `NULL` if none were detected. If verbose = `FALSE`,
#' a \code{data.frame} as described above is returned, or `NULL` if no matches
#' were found.
#'
#' @details The function also performs a check for
#' non-letter characters which are not expected to be present in
#' correctly-formatted taxon names. This detection may be made available to the
#' user via the `verbose` argument. Comparisons are performed using the
#' Jaro dissimilarity metric via
#' \code{\link[stringdist:stringdistmatrix]{stringdist::stringdistmatrix()}}.
#'
#' As all string distance metrics rely on approximate string matching,
#' different metrics can produce different results. This function uses Jaro
#' distance as it was designed with short, typed strings in mind, but good
#' practice should include comparisons using multiple metrics, and ultimately
#' specific taxonomic vetting where possible. A more complete implementation
#' and workflow for cleaning taxonomic occurrence data is available in the
#' `fossilbrush` R package on CRAN.
#'
#' @section Reference:
#' van der Loo, M. P. J. (2014). The stringdist package for approximate string
#' matching. The R Journal 6, 111-122.
#'
#' @section Developer(s):
#' Joseph T. Flannery-Sutherland & Lewis A. Jones
#' @section Reviewer(s):
#' Lewis A. Jones, Kilian Eichenseer & Christopher D. Dean
#' @importFrom stats na.omit
#' @importFrom stringdist stringdistmatrix
#' @examples
#' \dontrun{
#' # load occurrence data
#' data("tetrapods")
#' # Check taxon names alphabetically
#' ex1 <- tax_check(taxdf = tetrapods, name = "genus", dis = 0.1)
#' # Check taxon names within higher taxonomic groups
#' # (rows without a name or a group must be removed first)
#' occdf <- subset(tetrapods, !is.na(genus) & !is.na(family))
#' ex2 <- group_apply(occdf = occdf, group = "family", fun = tax_check,
#'                    name = "genus", dis = 0.1, verbose = FALSE)
#' }
#' @export
tax_check <- function(
  taxdf,
  name = "genus",
  dis = 0.05,
  start = 1,
  verbose = TRUE
) {
  ensure_args_are_named(exceptions = "taxdf")

  # ARGUMENT CHECKS --------------------------------------------------------- #

  check_data_frame(taxdf)
  check_column_presence(taxdf, name)

  # Replace missing values with NA
  taxdf[grep("^$|^\\s+$", taxdf[, name, drop = TRUE]), name] <- NA

  check_class(taxdf, name, "character")
  if (all(is.na(taxdf[, name, drop = TRUE]))) {
    cli::cli_abort(
      "Column {.val {name}} in {.arg taxdf} must have at least one entry that is not NA or empty."
    )
  }

  # Names are compared within alphabetical groups
  group <- substring(taxdf[, name, drop = TRUE], 1, 1)

  # dis: a 1L numeric > 0 and < 1
  rlang::check_number_decimal(dis)
  if (dis <= 0 || dis >= 1) {
    cli::cli_abort("{.arg dis} must be greater than 0 and less than 1.")
  }

  rlang::check_number_whole(start, min = 0)
  rlang::check_bool(verbose)

  # check for non-letter characters, returning NULL if none
  nm <- unique(grep("[^[:alpha:] ]", taxdf[, name, drop = TRUE], value = TRUE))
  if (length(nm) != 0) {
    cli::cli_warn("Non-letter characters present in the taxon names.")
  } else {
    nm <- NULL
  }

  # FORMAT INPUT DATA ------------------------------------------------------- #

  # names data.frame, drop missing names
  taxdf <- taxdf2 <- data.frame(
    group = group,
    name = taxdf[, name, drop = TRUE]
  )
  taxdf <- taxdf[!duplicated(taxdf), , drop = FALSE]
  taxdf <- taxdf[!is.na(taxdf[, "name", drop = TRUE]), , drop = FALSE]

  # RUN GROUPWISE COMPARISONS ----------------------------------------------- #

  # apply the comparison procedure group wise
  sp <- lapply(unique(taxdf[, "group", drop = TRUE]), function(y) {
    # all taxon names which belong to group y
    ob <- taxdf[taxdf[, "group", drop = TRUE] == y, "name"]

    # if there is are not multiple names in the group, skip
    if (length(ob) < 2) {
      flag <- NULL

      # otherwise perform group wise comparisons
    } else {
      # else get the Jaro distance matrix for the elements in the group
      test <- stringdist::stringdistmatrix(a = ob, b = ob, method = "jw")
      colnames(test) <- rownames(test) <- ob

      # set self matches to max dissimilarity for removal in the next step
      diag(test) <- 1

      # subset to those which fall below the dissimilarity threshold
      flag <- which(test < dis, arr.ind = TRUE)

      # if there are no remaining flagged names, return NULL
      if (length(flag) == 0) {
        flag <- NULL

        # otherwise additionally filter using shared starting/ending letters
      } else {
        # retrieve names
        flag <- cbind(ob[flag[, 1]], ob[flag[, 2]], y)
        # drop equivalent rows (xy, yx pairs)
        eq <- duplicated(t(apply(flag, 1, function(z) {
          paste0(sort(z))
        })))
        flag <- flag[!eq, , drop = FALSE]

        # cull by first y letter non-matches
        if (!is.null(start)) {
          c1 <- substr(flag[, 1], start = 1, stop = start)
          c2 <- substr(flag[, 2], start = 1, stop = start)
          flag <- flag[which(c1 == c2), , drop = FALSE]
        }

        # if there are no remaining flagged names, return NULL
        if (length(flag) == 0) {
          flag <- NULL
        }
      }
    }
    flag
  })

  # FORMAT OUTPUT ----------------------------------------------------------- #

  # format initial results data.frame from list
  err <- sp[!unlist(lapply(sp, is.null))]
  err <- as.data.frame(do.call(rbind, err))
  err$f1 <- as.vector(table(taxdf2[, "name"])[match(
    err$V1,
    names(table(
      taxdf2[, "name"]
    ))
  )])
  err$f2 <- as.vector(table(taxdf2[, "name"])[match(
    err$V2,
    names(table(
      taxdf2[, "name"]
    ))
  )])

  # NULL if no matches present
  if (nrow(err) == 0) {
    err <- NULL

    # else reorder rows so the more frequent synonym is in the first column
  } else {
    mins <- apply(err[, 4:5], 1, which.min) - 1
    maxs <- abs(mins - 1)
    fq1 <- unlist(err[, 4:5])[seq_along(maxs) + (maxs * length(maxs))]
    fq2 <- unlist(err[, 4:5])[seq_along(mins) + (mins * length(mins))]
    mins <- unlist(err[, 1:2])[seq_along(mins) + (mins * length(mins))]
    maxs <- unlist(err[, 1:2])[seq_along(maxs) + (maxs * length(maxs))]
    err <- data.frame(
      group = err$y,
      greater = as.vector(maxs),
      lesser = as.vector(mins),
      count_greater = fq1,
      count_lesser = fq2
    )
    err <- err[order(err[, "group"], err[, "greater"], method = "radix"), ]
    row.names(err) <- NULL
  }

  # return
  if (verbose) {
    return(list(synonyms = err, non_letter_name = nm))
  } else {
    return(err)
  }
}
