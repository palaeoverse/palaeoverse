#' tax_check
#'
#' A function for checking for potential spelling variations of the same
#' taxon. Spelling variations are checked within alphabetical groups (default),
#' or within higher taxonomic groups, if provided.
#'
#' @param x \code{dataframe}. A data.frame with named columns (e.g., 'species',
#' 'genus', etc.). This must contain taxon names and optionally a further
#' column denoting the groups within which taxon names will be checked against
#' one another (e.g., 'family', 'order', etc.). NA values or empty strings in
#' the name and group columns (i.e., '' and ' ') will be ignored.
#' @param names \code{character}. The column name of the taxon names you wish
#' to check (e.g., 'genus').
#' @param groups \code{character}. The column name of the higher taxonomic
#' assignments in `x` you wish to group by. If `NULL` (default), name
#' comparison will be conducted within alphabetical groups.
#' @param sim \code{numeric}. The percentage similarity threshold. Potential
#' synonyms above this threshold are reported.
#' @param start \code{numeric}. The number of matching characters at the
#' beginnings of two potential synonyms, below which the match will be
#' discarded. By default this value is set to 1 (i.e., the first letters must
#' match).
#' @param pref \code{character}. A vector of prefixes which may result in
#' spuriously high similarities. Synonyms will be filtered out if only one or
#' the other bears a given prefix. The default is `NULL`.
#' @param suff \code{character}. A vector of suffixes which help distinguish
#' otherwise similar taxon names. Synonyms will be filtered out if only one or
#' the other bears a given suffix, The default is `NULL`.
#' @param verbose \code{logical}. Should the results of the non-letter
#' character check be reported to the user? If `TRUE`, the result will only be
#' reported if such characters are detected in the taxon names.
#'
#' @return If verbose = `TRUE' (default), a \code{list} with two elements. The
#' first element in the list (synonyms) is a data.frame with each row reporting
#' a pair of synonyms. The first column 'group' contains the higher group in
#' which they occur (this may be the alphabetical groupings default). The
#' second column 'greater' contains the most common synonym in each pair. The
#' third column 'lesser' contains the least common synonym in each pair. The
#' third and fourth columns (freq_1, freq_2) contain the respective frequencies
#' of each synonym in a pair. If no matches were found for the filtering
#' parameters, this element is `NULL` instead. The second element (non_letter)
#' is a vector of taxon names which contain non-letter characters, or `NULL` if
#' none were detected. If verbose = `FALSE`, a \code{data.frame} as described
#' above, or `NULL` if no matches were found.
#'
#' @details When higher taxonomy is provided, but some entries are missing,
#' comparisons will still be made within alphabetical groups of taxa which lack
#' higher taxonomic affiliations. The function also performs a check for non-
#' letter characters which are not expected to be present in correctly-
#' formatted taxon names. This detection may be made available to the user via
#' the `verbose` argument. Comparison is first performed using the Jaro string
#' dissimilarity with the `stringdistmatrix` function from the `stringdist`
#' package. The Jaro metric can be easily interpreted as a percentage
#' similarity, converted as (1 - Jaro dissimilarity). Low similarity pairs are
#' discarded, and matches can also be filtered out if they do not share a given
#' number of starting letters. The Jaro threshold is set to 90% similarity by
#' default, but synonyms may still slip through at lower similarity thresholds.
#'
#' Frequent re-use of Latin or Greek prefixes can induce high similarity
#' between taxon names, e.g., 'Proto' and Protoro' between 'Protosuchus' and
#' 'Protorosaurus'. Conversely, suffixes are often used to differentiate taxon
#' names which share a common stem, e.g., 'is' and 'ina' separate
#' 'Gelidorthis' and 'Gelidorthina'. As such, vectors of prefixes and suffixes
#' can also be supplied and matches will be discarded if two potential synonyms
#' do not share the same prefix or suffix, even if their similarity exceeds the
#' Jaro filtering threshold. Prefixes and suffixes may have no effect if this
#' threshold is very high and their use is entirely optional, but can be useful
#' for filtering out large numbers of false matches from for large data sets
#' where prefixes and name stems are prevalent, for example brachiopods.
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
#' M. P. J. van der Loo (2014). The stringdist package for approximate string
#' matching. The R Journal 6, 111-122.
#'
#' @section Developer(s):
#' Joseph T. Flannery-Sutherland
#' @section Reviewer(s):
#' Lewis A. Jones & Kilian Eichenseer
#' @importFrom stats na.omit
#' @importFrom stringdist stringdistmatrix
#' @examples
#' # load occurrence data
#' data("tetrapods")
#'
#' # synonym check with default params. Data is well curated so
#' # few matches are returned and no non-letter checks triggered
#'
#' synon <- tax_check(tetrapods, names = "genus", groups = "family")
#'
#' # define prefixes and suffixes
#' b_pref <- c("Neo", "Proto")
#' b_suff <- c("saurus", "suchus")
#'
#' # synonym check using a couple of example prefixes and suffixes
#' synon <- tax_check(tetrapods, names = "genus", groups = "family",
#'                    pref = b_pref, suff = b_suff)
#'
#' @export

tax_check <- function(x, names, groups = NULL, sim = 90, start = 1,
                      pref = NULL, suff = NULL, verbose = TRUE) {

  # ARGUMENT CHECKS --------------------------------------------------------- #

  # x: a data.frame with column names and at least one row
  if(!exists("x")) {x <- NULL}
  if(any(c(!is.data.frame(x), nrow(x) == 0, is.null(colnames(x))))) {
    stop("Please supply 'x' as a data.frame with named columns, containing
         taxon names, and optionally their higher classification")
  }

  # names: a 1L character vector denoting a character column in x
  if(!exists("names")) {names <- NULL}
  if(any(c(!is.atomic(names), length(names) != 1, !names %in% colnames(x)))) {
    stop("Please specify 'names' as a single column name in 'x'")
  }
  x[grep("^$|^\\s+$", x[,names]),names] <- NA
  if(!is.character(x[,names]) | all(is.na(x[,names]))) {
    stop("The 'names' column in 'x' must contain data of class character and
         at least one entry that is not NA or empty")
  }

  # groups: If not NULL, a 1L character vector denoting a character column in x
  if(!is.null(groups)) {
    if(any(c(!is.atomic(groups), length(groups) != 1,
             !groups %in% colnames(x)))) {
      stop("Please specify 'groups' as a single column name in 'x")
    }
    if(!is.character(x[,groups])) {
      stop("The 'groups' column in 'x' must contain data of class character")
    }
    groups <- gsub("^$|^\\s+$", NA, x[,groups])
  } else {
    groups <- substring(x[,names], 1, 1)
  }

  # sim: a 1L numeric > 0 and < 1
  if(any(c(!is.numeric(sim), length(sim) != 1, !is.atomic(sim)))) {
    stop("'sim' must be a single numeric, greater than 0, less than 100")
  }
  if(sim >= 100 | sim <= 0) {
    stop("'sim' must be a single numeric, greater than 0, less than 100")
  }

  # start: a 1L integer >= 0
  if(!is.null(start)) {
    if(any(c(!is.numeric(start), length(start) != 1, !is.atomic(start)))) {
      stop("'start' must be a single positive integer, or zero")
    }
    if(any(c(start < 0, start %% 1 != 0, is.nan(start), is.infinite(start)))) {
      stop("'start' must be a single positive integer, or zero")
    }
  }

  # pref, suff: If not NULL, character vectors
  if(is.null(pref)) {pref <- ""}
  if(is.null(suff)) {suff <- ""}
  if(any(c(!is.atomic(c(pref, suff)), !is.character(pref),
           !is.character(suff)))) {
    stop("'pref' and 'suff' must be character vectors or NULL")
  }

  # verbose: a 1L logical vector
  if(any(c(!is.atomic(c(verbose)), !is.logical(verbose),
           length(verbose) != 1))) {
    stop("'verbose' must be a single logical value")
  }

  # check for non-letter characters, returning NULL if none
  gp <- unique(grep("[^[:alpha:] ]", groups, value = TRUE))
  if(verbose) {warning("Non-letter characters present in the group names")}

  nm <- unique(grep("[^[:alpha:] ]", x[,names], value = TRUE))
  if(length(nm) != 0) {
    if(verbose) {warning("Non-letter characters present in the taxon names")}
  } else {
    nm <- NULL
  }


  # FORMAT INPUT DATA ------------------------------------------------------- #

  # names data.frame, drop missing names, fill missing groups alphabetically
  x <- x2 <- data.frame(groups = groups, names = x[,names])
  x <- x[!duplicated(x),,drop = FALSE]
  x <- x[!is.na(x[,"names"]),,drop = FALSE]
  no_group <- which(is.na(x[,"groups"]))
  x[no_group, "groups"] <- substring(x[no_group, "names"], 1, 1)
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
      test <- stringdist::stringdistmatrix(a = ob, b = ob, method = "jw")
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
        # drop equivalent rows (xy, yx pairs)
        eq <- duplicated(t(apply(flag, 1, function(z) {paste0(z[order(z)])})))
        flag <- flag[!eq,,drop = FALSE]

        # cull by first y letter non-matches
        if(!is.null(start)) {
          c1 <- substr(flag[,1], start = 1, stop = start)
          c2 <- substr(flag[,2], start = 1, stop = start)
          flag <- flag[which(c1 == c2), , drop = FALSE]
        }

        # if there are no remaining flagged names, return NULL
        if(length(flag) == 0) {
          flag <- NULL

        # otherwise cull by prefixes and suffixes if supplied
        } else {

          if(as.logical(sum(nchar(c(pref, suff))))) {

            # set the regex for prefixes and suffixes within a single vector
            common <- c(paste0("^", na.omit(pref)), paste0(na.omit(suff), "$"))
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

  # NULL if no matches present
  if(nrow(err) == 0) {
    err = NULL

  # else reorder rows so the more frequent synonym is in the first column
  } else {

    mins <- apply(err[,4:5], 1, which.min) - 1
    maxs <- abs(mins - 1)
    fq1 <- unlist(err[,4:5])[1:length(maxs) + (maxs * length(maxs))]
    fq2 <- unlist(err[,4:5])[1:length(mins) + (mins * length(mins))]
    mins <- unlist(err[,1:2])[1:length(mins) + (mins * length(mins))]
    maxs <- unlist(err[,1:2])[1:length(maxs) + (maxs * length(maxs))]
    err <- data.frame(group = err$y, greater = as.vector(maxs),
                      lesser = as.vector(mins), freq_1 = fq1,
                      freq_2 = fq2)
    err <- err[order(err[,"group"], err[,"greater"], method = "radix"),]
    row.names(err) <- NULL
  }

  # return
  if(verbose) {
    return(list(synonyms = err, non_letter = nm))
  } else {
    return(err)
  }
}
