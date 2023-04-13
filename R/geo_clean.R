#' Interval names spell check
#'
#' A function to check for and count potential spelling variations of the same
#' interval. ###Spelling variations are checked within alphabetical groups
#' (default), or within higher taxonomic groups if provided.###
#'
#' @param occdf \code{dataframe}. A dataframe containing
#' interval names (e.g. "species", "genus"). An optional column
#' containing the groups (e.g. "family", "order") which taxon names
#' belong to may also be provided (see `group` for details).
#' NA values or empty strings in the name and group columns (i.e. "" and " ")
#' are ignored.
#'
#'
#' @param column_name \code{character}. The name of the column with the interval
#' names (e.g. "time_intervals"). ##Can be 1 or 2 columns".##
#'
#'
#' @param reference \code{character} or \code{FALSE}. Allows interval names to
#' be compared to intervals in the `GTS2020` (default), the `GTS2012` table or
#' `palaeoverse::interval_key`.
#'
#'
#' @param out \code{character}. Determine whether to return either a
#' \code{dataframe} describing which taxa are included or not included in the
#' tree ("full_table", the default), the same table but with taxa included in
#' both the tree and the list removed ("diff_table"), the counts of taxa
#' included and not included in the tree ("counts"), or the phylogeny trimmed to
#' only include taxa in the provided list ("tree").
#'
#'
#' @param clean \code{logical}. Return interval names which could
#' not be assigned, instead of the dataframe with assignments.
#' Defaults to \code{FALSE}.
#'
#'
#'
#'
#'
#' @return If verbose = `TRUE` (default), a \code{list} with three elements. The
#' first element in the list (synonyms) is a \code{data.frame} with each row
#' reporting a pair of potential synonyms. The first column "group" contains the
#' higher group in which they occur (alphabetical groupings if `group` is
#' not provided). The second column "greater" contains the most common synonym
#' in each pair. The third column "lesser" contains the least common synonym in
#' each pair. The third and fourth column (`count_greater`, `count_lesser`)
#' contain the respective counts of each synonym in a pair. If no matches were
#' found for the filtering arguments, this element is `NULL` instead. The second
#' element (`non_letter_name`) is a vector of taxon names which contain
#' non-letter characters, or `NULL` if none were detected. The third element
#' (non_letter_group) is a vector of taxon groups which contain non-letter
#' characters, or `NULL` if none were detected. If verbose = `FALSE`, a
#' \code{data.frame} as described above is returned, or `NULL` if no matches
#' were found.
#'
#' @details When higher taxonomy is provided, but some entries are missing,
#' comparisons will still be made within alphabetical groups of taxa which lack
#' higher taxonomic affiliations. The function also performs a check for
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
#' Pedro L. Godoy & Lewis A. Jones
#' @section Reviewer(s):
#' Lewis A. Jones
#' @importFrom stats na.omit
#' @importFrom stringdist stringdistmatrix
#' @examples
#' # load occurrence data
#' data("tetrapods")
#' # Check taxon names alphabetically
#' ex1 <- tax_check(taxdf = tetrapods, name = "genus", dis = 0.1)
#' # Check taxon names by group
#' ex2 <- tax_check(taxdf = tetrapods, name = "genus",
#'                  group = "family", dis = 0.1)
#'
#' @export
geo_clean <- function(occdf,
                      column_name = "time_intervals",
                      reference = "GTS2020",
                      out = ""
                      clean = "TRUE") {

  # ARGUMENT CHECKS --------------------------------------------------------- #

  # taxdf: a data.frame with column names and at least one row
  if (!exists("taxdf")) {
    taxdf <- NULL
    }

  if (any(c(!is.data.frame(taxdf),
            nrow(taxdf) == 0,
            is.null(colnames(taxdf))))) {
    stop("Please supply `taxdf` as a data.frame with named columns, containing
         taxon names, and optionally their higher classification")
  }

  # names: a 1L character vector denoting a character column in taxdf
  if (any(c(!is.atomic(name),
            length(name) != 1,
            !name %in% colnames(taxdf)))) {
    stop("Please specify `name` as a single column name in `taxdf`")
  }

  # Replace missing values with NA
  taxdf[grep("^$|^\\s+$", taxdf[, name]), name] <- NA

  if (!is.character(taxdf[, name]) || all(is.na(taxdf[, name]))) {
    stop("The `name` column in `taxdf` must contain data of class character and
         at least one entry that is not NA or empty")
  }

  # groups: If not NULL, a 1L character vector denoting a character column
  # in taxdf
  if (!is.null(group)) {
    if (any(c(!is.atomic(group), length(group) != 1,
             !group %in% colnames(taxdf)))) {
      stop("Please specify `group` as a single column name in `taxdf`")
    }
    if (!is.character(taxdf[, group])) {
      stop("The `group` column in `taxdf` must contain data of class character")
    }
    group <- gsub("^$|^\\s+$", NA, taxdf[, group])
  } else {
    group <- substring(taxdf[, name], 1, 1)
  }

  # dis: a 1L numeric > 0 and < 1
  if (any(c(!is.numeric(dis), length(dis) != 1, !is.atomic(dis)))) {
    stop("`dis` must be a single numeric, greater than 0 and less than 1")
  }
  if (dis >= 1 || dis <= 0) {
    stop("`dis` must be a single numeric, greater than 0 and less than 1")
  }

  # start: a 1L integer >= 0
  if (!is.null(start)) {
    if (any(c(!is.numeric(start),
              length(start) != 1,
              !is.atomic(start)))) {
      stop("`start` must be a single positive integer, or zero")
    }
    if (any(c(start < 0, start %% 1 != 0,
              is.nan(start),
              is.infinite(start)))) {
      stop("`start` must be a single positive integer, or zero")
    }
  }

  # verbose: a 1L logical vector
  if (any(c(!is.atomic(c(verbose)),
           !is.logical(verbose),
           length(verbose) != 1))) {
    stop("`verbose` must be a single logical value")
  }

  # check for non-letter characters, returning NULL if none
  gp <- unique(grep("[^[:alpha:] ]", group, value = TRUE))
  if (length(gp) != 0) {
    warning("Non-letter characters present in the group names")
  } else {
    gp <- NULL
  }

  nm <- unique(grep("[^[:alpha:] ]", taxdf[, name], value = TRUE))
  if (length(nm) != 0) {
    warning("Non-letter characters present in the taxon names")
  } else {
    nm <- NULL
  }

  # FORMAT INPUT DATA ------------------------------------------------------- #

  # names data.frame, drop missing names, fill missing groups alphabetically
  taxdf <- taxdf2 <- data.frame(group = group, name = taxdf[, name])
  taxdf <- taxdf[!duplicated(taxdf), , drop = FALSE]
  taxdf <- taxdf[!is.na(taxdf[, "name"]), , drop = FALSE]
  no_group <- which(is.na(taxdf[, "group"]))
  taxdf[no_group, "group"] <- substring(taxdf[no_group, "name"], 1, 1)

  # RUN GROUPWISE COMPARISONS ----------------------------------------------- #

  # apply the comparison procedure group wise
  sp <- lapply(unique(taxdf[, "group"]), function(y) {

    # all taxon names which belong to group y
    ob <- taxdf[taxdf[, "group"] == y, "name"]

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
          paste0(z[order(z)])
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
  err$f1 <- as.vector(table(taxdf2[, "name"])[match(err$V1,
                                                names(table(
                                                  taxdf2[, "name"])))])
  err$f2 <- as.vector(table(taxdf2[, "name"])[match(err$V2,
                                                names(table(
                                                  taxdf2[, "name"])))])

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
    err <- data.frame(group = err$y, greater = as.vector(maxs),
                      lesser = as.vector(mins), count_greater = fq1,
                      count_lesser = fq2)
    err <- err[order(err[, "group"], err[, "greater"], method = "radix"), ]
    row.names(err) <- NULL
  }

  # return
  if (verbose) {
    return(list(synonyms = err, non_letter_name = nm, non_letter_group = gp))
  } else {
    return(err)
  }
}
