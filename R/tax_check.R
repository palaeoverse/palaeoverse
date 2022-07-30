#' tax_check
#'
#' A function for checking for potential spelling variations of the same
#' taxon name. These variations are checked for within higher taxonomic
#' groups or  within alphabetical groups if higher taxonomy is not provided
#' (the default assumption).
#'
#' @param x \code{dataframe}. A data.frame with named columns (e.g., 'species',
#' 'genus', ect.). This must contain taxon names and optionally a further
#' column denoting the groups within which taxon names will be checked against
#' one another (e.g., 'family', 'order', ect.).
#' @param names \code{character}. The column name of the taxonomic names you
#' wish to check (e.g., 'genus')
#' @param groups \code{character}. The column name of the higher taxonomic
#' assignments in `x` you wish to group by. If `NULL` (default), name
#' comparison will be conducted within alphabetical groups.
#' @param sim \code{numeric}. The percentage similarity, above which potential
#' synonyms be reported.
#' @param start \code{numeric}. The number of matching characters at the
#' beginnings of two potential synonyms, below which the match will be
#' discarded. By default this value is set to 1 (i.e., the first letters must
#' match).
#' @param end \code{numeric}. The number of matching characters at the ending
#' of two potential synonyms, below which the match will be discarded. By
#' default this value is set to 0 (i.e., no matching required).
#' @param pref \code{character}. A vector of prefixes which may result in
#' spuriously high similarities. Synonyms will be filtered out out if only one
#' or the other bears a given prefix. The default is `NULL`.
#' @param suff \code{character}. A vector of suffixes which may result in
#' spuriously high similarities. Synonyms will be filtered out out if only one
#' or the other bears a given suffixes, The default is `NULL`.
#' @param verbose \code{logical}. Should the results of the non-letter
#' character check be reported to the user. If `TRUE`, the result will only be
#' reported if such characters are detected in the taxon names.
#'
#' @return \code{dataframe}. A data.frame with each row reporting a pair of
#' synonyms. The first column 'greater' contains the most common synonym in
#' each pair, the second column 'lesser' the least common synonym in each pair,
#' and the third column 'group' the higher group in which they occur (this may
#' be the alphabetical groupings default). If no matches were found for the
#' filtering parameters, `NULL` is returned instead.
#'
#' @details Where higher taxonomy is provided, but some entries are missing,
#' comparisons will be made within alphabetical groups. The function also
#' silently performs a check for non-letter characters which are not expected
#' to be present in correctly-formatted taxon names. This detection may be made
#' available to the user via the verbose argument.
#'
#' Comparison is first performed using the Jaro string similarity metric, using
#' the `stringdist` function from the `stringdist` package. The Jaro metric can
#' be easily interpreted as percentage similarity, allowing low similarity
#' pairs to be filtered before the result is returned. Matches can then been
#' filtered out if they do not share a given number of starting and/or ending
#' letters. Penalisation by starting letters is analogous to Jaro-Winkler
#' distance, but purely Jaro distance is used instead as common Latin or Greek
#' prefixes and suffixes which result in high string similarities can also be
#' supplied, e.g. 'Pro', 'Proto'. High similarity matches resulting from high
#' similarity between such prefixes or suffixes will only be retained if two
#' potential synonyms share the same prefix or suffix (see example). Note that
#' this creates the assumption that spelling variations are not present among
#' the prefixes and suffixes, which is not always the case.
#'
#' As all string distance metrics rely on approximate string matching,
#' different metrics can produce different results. This function uses Jaro
#' similarity as it was designed with short, typed strings in mind, but good
#' practice should include comparisons using multiple metrics, and ultimately
#' specific taxonomic vetting where possible. A more complete implementation
#' and workflow for cleaning taxonomic occurrence data is available in the
#' `fossilbrush` R package on CRAN.
#'
#' #' @section Reference:
#' M. P. J. van der Loo (2014). The stringdist package for approximate string
#' matching. The R Journal 6, 111-122.
#'
#' @section Developer(s):
#' Joseph T. Flannery-Sutherland
#' @section Reviewer(s):
#' Lewis A. Jones & XXX
#' @importFrom stats na.omit
#' @importFrom stringdist stringdistmatrix
#' @examples
#' \dontrun{
#' # load occurrence data
#' data("tetrapods")
#'
#' # define prefixes and suffixes
#' b_pref <- c("Neo", "Proto")
#' b_suff <- c("saurus", "suchus")
#'
#' # run function
#' synon <- tax_check(tetrapods, names = "genus", groups = "family",
#' sim = 90, pref = b_pref, suff = b_suff)
#'
#' }
#' @export

tax_check <- function(x, names, groups = NULL, sim = 80, start = 1, end = 0,
                      pref = NULL, suff = NULL, verbose = FALSE) {

  # ARGUMENT CHECKS --------------------------------------------------------- #

  # x: a data.frame with at least one row
  if(!exists("x")) {
    stop("Please supply 'x' as a data.frame containing a column of taxon names,
         and optionally a column denoting their higher classification")
  }
  if(!is.data.frame(x)) {
    stop("Please supply 'x' as a data.frame containing a column of taxon names,
         and optionally a column denoting their higher classification")
  }
  if(nrow(x) == 0) {
    stop("'x' does not contain any data")
  }
  if(is.null(colnames(x))) {
    stop("'x' does not contain any column names")
  }

  # names: a 1L character vector denoting a character column in x
  if(!exists("names")) {
    stop("Please specify 'names' as a single column name in 'x'")
  }
  if(length(names) != 1 | !is.character(names)) {
    stop("Please specify 'names' as a single column name in 'x'")
  }
  if(!names %in% colnames(x)) {
    stop("'names' is not a column name in 'x'")
  }
  if(!is.character(x[,names])) {
    stop("The 'names' column in 'x' must contain character data")
  }
  if(all(is.na(x[,names]))) {
    stop("All taxon name entries in the specified column are NA")
  }

  # groups: If not NULL, a 1L character vector denoting a character column in x
  if(!is.null(groups)) {
    if(length(groups) != 1 | !is.character(groups)) {
      stop("Please specify 'groups' as a single column name in 'x")
    }
    if(!groups %in% colnames(x)) {
      stop("'groups' is not a column name in 'x'")
    }
    if(!is.character(x[,groups])) {
      stop("The 'groups' column in 'x' must contain character data")
    }
    groups <- x[,groups]
  } else {
    groups <- substring(x[,names], 1, 1)
  }

  # sim: a 1L numeric > 0 and < 1
  if(!is.numeric(sim) | length(sim) != 1) {
    stop("'sim' must be a single numeric, greater than 0, less than 1")
  }
  if(sim >= 100 | sim <= 0) {
    stop("'sim' must be a single numeric, greater than 0, less than 1")
  }

  # start: a 1L integer >= 0
  if(!is.null(start)) {
    if(!is.numeric(start) | length(start) != 1) {
      stop("'start' must be a single positive integer, or zero")
    }
    if(start < 0 | start %% 1 != 0) {
      stop("'start' must be a single positive integer, or zero")
    }
  }

  # end: a 1L integer >= 0
  if(!is.null(end)) {
    if(!is.numeric(end) | length(end) != 1) {
      stop("'end' must be a single positive integer, or zero")
    }
    if(end < 0 | end %% 1 != 0) {
      stop("'end' must be a single positive integer, or zero")
    }
  }

  # pref: If not NULL, a character vector
  if(!is.null(pref)) {
    if(!is.vector(pref)) {
      stop("'pref' must be a character vector or NULL")
    }
    if(!is.character(pref)) {
      stop("'pref' must be a character vector or NULL")
    }
    pref <- na.omit(pref)
  }

  # suff: If not NULL, a character vector
  if(!is.null(suff)) {
    if(!is.vector(suff)) {
      stop("'suff' must be a character vector or NULL")
    }
    if(!is.character(suff)) {
      stop("'suff' must be a character vector or NULL")
    }
    suff <- na.omit(suff)
  }

  # verbose: a 1L logical vector
  if(!is.logical(verbose) | length(verbose) != 1) {
    stop("'verbose' must be a single logical")
  }
  if(length(grep("[^[[:alpha:]]", x[,names])) != 0) {
    if(verbose) {warning("Non-letter characters present in the taxon names")}
  }


  # FORMAT INPUT DATA ------------------------------------------------------- #

  # names data.frame, drop missing names, fill missing groups alphabetically
  x <- x2 <- data.frame(groups = groups, names = x[,names])
  x <- x[!duplicated(x),,drop = FALSE]
  x <- x[!is.na(x[,"names"]),,drop = FALSE]
  no_group <- which(is.na(x[,"groups"]))
  x[no_group, "groups"] <- substring(x[no_group, "names"], 1, 1)
  x <- x[order(x[,"groups"]),]
  # convert from similarity to dissimilarity
  sim <- 1 - (sim / 100)


  # RUN GROUPWISE COMPARISONS ----------------------------------------------- #

  # apply the comparison procedure group wise
  sp <- lapply(unique(x[,"groups"]), function(y) {

    # all taxon names which belong to group y
    ob <- x[x[,"groups"] == y,"names"]

    # if there is are not multiple names in the group, skip
    if(length(ob) < 2) {
      flag <- NULL

    # otherwise perform group wise comparisons
    } else {

      # else get the Jaro distance matrix for the elements in the group
      test <- stringdistmatrix(a = ob, b = ob, method = "jw")
      colnames(test) <- rownames(test) <- ob

      # set self matches to max dissimilarity for removal in the next step
      diag(test) <- 1

      # subset to those which fall below the dissimilarity threshold
      flag <- which(test < sim, arr.ind = TRUE)

      # if there are no remaining flagged names, return NULL
      if(length(flag) == 0) {
        flag <- NULL

      # otherwise additionally filter using shared starting/ending letters
      } else {

        # retrieve names
        flag <- cbind(ob[flag[,1]], ob[flag[,2]], y)

        # cull equivalent matches
        tx <- unique(c(flag[,1], flag[,2]))
        tx1 <- match(flag[,1], tx)
        tx2 <- match(flag[,2], tx)
        txs <- tx1 + tx2
        flag <- flag[!duplicated(txs), , drop = FALSE]

        # cull by first y letter non-matches
        if(!is.null(start)) {
          c1 <- substr(flag[,1], start = 1, stop = start)
          c2 <- substr(flag[,2], start = 1, stop = start)
          flag <- flag[which(c1 == c2), , drop = FALSE]
        }

        # cull by last y letter non-matches
        if(!is.null(end)) {
          c1 <- substr(flag[,1], start = (nchar(flag[,1]) - (end - 1)),
                       stop = nchar(flag[,1]))
          c2 <- substr(flag[,2], start = (nchar(flag[,2]) - (end - 1)),
                       stop = nchar(flag[,2]))
          flag <- flag[which(c1 == c2), , drop = FALSE]
        }

        # if there are no remaining flagged names, return NULL
        if(length(flag) == 0) {
          flag <- NULL

        # otherwise cull by prefixes and suffixes
        } else {

          # set the regex for prefixes and suffixes within a single vector
          common <- c(paste0("^", pref), paste0(suff, "$"))
          common <- common[nchar(common) > 1]

          to_drop <- unlist(lapply(common, function(z) {
            # grep the common suffix in the first column
            g1 <- grepl(z, flag[,1])
            # grep that same suffix for the second column
            g2 <- grepl(z, flag[,2])
            # designate the non matching elements in the dataframe
            which(g1 != g2)
          }))

          # drop entries without matching prefixes/suffixes
          if(length(to_drop) != 0) {
            flag <- flag[-to_drop,,drop = FALSE]

            # if there are no remaining flagged names, return NULL
            if(length(flag) == 0) {
              flag <- NULL
            }
          }
        }
      }
    }
    out <- flag
  })

  # FORMAT OUTPUT ----------------------------------------------------------- #

  # format initial results data.frame from list
  err <- sp[!unlist(lapply(sp, is.null))]
  err <- as.data.frame(do.call(rbind, err))
  err$f1 <- as.vector(table(x2[,"names"])[match(err$V1,
                                                names(table(x2[,"names"])))])
  err$f2 <- as.vector(table(x2[,"names"])[match(err$V2,
                                                names(table(x2[,"names"])))])

  # return NULL if no matches present
  if(nrow(err) == 0) {
    return(NULL)

  # else reorder rows so the more frequent synonym is in the first column
  } else {

        mins <- apply(err[,4:5], 1, which.min) - 1
    maxs <- abs(mins - 1)
    mins <- unlist(err[,1:2])[1:length(mins) + (mins * length(mins))]
    maxs <- unlist(err[,1:2])[1:length(maxs) + (maxs * length(maxs))]
    err <- data.frame(greater = as.vector(maxs), lesser = as.vector(mins),
                      group = err$y)
    err <- err[order(err[,"group"], err[,"greater"], method = "radix"),]
    row.names(err) <- NULL
    return(err)
  }
}
