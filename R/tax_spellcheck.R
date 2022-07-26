#' tax_spellcheck
#'
#' A function for checking for spelling variations of the same
#' taxon name. These are checked for within higher taxonomic
#' groups or if higher taxonomy is not provided (the default
#' assumption), within alphabetical groups.
#'
#' @param x \code{dataframe}. This must have column names and contain
#' column with taxon names, and optionally a further column denoting the
#' groups within which taxon names will be checked against one another.
#' @param names \code{character}. The column header of the taxon names field
#' in `x`.
#' @param groups \code{character}. The column header of the higher taxonomic
#' assignments field in `x`. If `NULL` (default), name comparison will be conducted
#' within alphabetical groups.
#' @param sim \code{numeric}. The percentage dissimilarity, above which potential
#' synonyms be filtered out. Percentage is expressed between 0 and 1.
#' @param srt \code{numeric}. The number of matching characters at the
#' beginnings of two potential synonyms, below which the match will be
#' discarded. By default 1, i.e. the first letters must match.
#' @param end \code{numeric}. The number of matching characters at the endings
#' of two potential synonyms, below which the match will be discarded. By
#' default 0, i.e. no penalisation.
#' @param pref \code{character}. If not `NULL`, a vector of prefixes
#' which may result in spuriously high similarities. Synonyms will be filtered
#' out if only one or the other bears a given prefix.
#' @param suff \code{character}. If not `NULL`, a vector of suffixes
#' which may result in spuriously high similarities. Synonyms will be filtered
#' out if only one or the other bears a given suffix.
#' @param verbose \code{logical}. Should the results of the non-alpha character
#' check should be reported to the user. If `TRUE`, the result will only be
#' reported if such characters are detected in the taxon names.
#'
#' @return \code{dataframe}. The output contains the more common flagged synonym
#' in the first column, the less common synonym in the middle column and the
#' group in which they occur in in the final column (this may be the
#' alphabetical) default. If no matches were found which exceeded the filtering
#' parameters, `NULL` is returned instead.
#'
#' @details Where higher taxonomy is provided, but some entries are
#' missing, comparisons will be made within alphabetical groups. The
#' function also silently performs a check for non-letter characters
#' which are not expected to be present in correctly-formatted
#' taxon names. This detection may be made verbose by the user.
#'
#' Comparison is first performed using the Jaro string similarity
#' metric which can be easily interpreted as percentage dissimilarity,
#' allowing low similarity pairs to be filtered before the result is
#' returned. Matches can then been filtered out if they do not share a
#' given number of starting and/or ending letters. Penalisation by
#' starting letters is analogous to Jaro-Winkler distance, but purely
#' Jaro distance is used instead as common Latin or Greek prefixes and
#' suffixes which can result in spuriously high string similarities
#' can also be supplied. User-supplied prefixes and suffixes will cause
#' higher similarity matches which do not share the same prefix or
#' suffix to be filtered from the results (see example). Note that this
#' creates the assumption that spelling variations are not present among
#' the prefixe and suffixes, which is not always the case.
#'
#' As all string distance metrics rely on approximate string matching,
#' different metrics can produce different results. This function uses
#' Jaro similarity as it was designed with short, typed strings in mind,
#' but good practice should include comparisons using multiple metrics.
#' A more sophisticated version of this spell check function is available
#' in the `fossilbrush` R package on CRAN, as part of a multistage cleaning
#' routine designed for taxonomic occurrence data.
#'
#' @section Developer(s):
#' Joseph T. Flannery-Sutherland
#' @section Reviewer(s):
#' Name(s)
#' @importFrom stats na.omit
#' @importFrom stringdist stringdistmatrix
#' @examples
#' \dontrun{
#' # load occurrence data
#' brachios <- read.csv(paste0("https://paleobiodb.org/data1.2/occs/list.csv",
#'                      "?base_name=Brachiopoda",
#'                      "&interval=Cambrian,Silurian",
#'                      "&show=class"))
#'
#' # define prefixes and suffixes
#' b_pref <- c("Neo", "Micro", "Schizo", "Stropho", "Ortho")
#' b_suff <- c("spirifer", "rhynchus", "strophia", "treta", "thyris", "orthis",
#'             "ina", "ella", "trypa")
#'
#' # run function
#' synon <- tax_spellcheck(brachios, names = "genus", groups = "family",
#'                         pref = b_pref, suff = b_suff)
#'
#' }
#' @export

tax_spellcheck <- function(x, names, groups = NULL,
                           sim = 0.2, srt = 1, end = 0,
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
  if(sim >= 1 | sim <= 0) {
    stop("'sim' must be a single numeric, greater than 0, less than 1")
  }

  # srt: a 1L integer >= 0
  if(!is.null(srt)) {
    if(!is.numeric(srt) | length(srt) != 1) {
      stop("'srt' must be a single positive integer, or zero")
    }
    if(srt < 0 | srt %% 1 != 0) {
      stop("'srt' must be a single positive integer, or zero")
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
        if(!is.null(srt)) {
          c1 <- substr(flag[,1], start = 1, stop = srt)
          c2 <- substr(flag[,2], start = 1, stop = srt)
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

  # reorder rows so the more frequent potential synonym is in the first column
  mins <- apply(err[,4:5], 1, which.min) - 1
  maxs <- abs(mins - 1)
  mins <- unlist(err[,1:2])[1:length(mins) + (mins * length(mins))]
  maxs <- unlist(err[,1:2])[1:length(maxs) + (maxs * length(maxs))]
  err <- data.frame(greater = as.vector(maxs), lesser = as.vector(mins),
                    group = err$y)

  # return NULL if no matches present, otherwise the data.frame of matches
  if(nrow(err) == 0) {
    return(NULL)
  } else {
    err <- err[order(err[,"greater"], err[,"group"], method = "radix"),]
    return(err)
  }
}
