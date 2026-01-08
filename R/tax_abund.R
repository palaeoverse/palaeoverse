#' Summarise abundance of fossil occurrences
#'
#' A function to calculate the absolute and relative abundance of fossil
#' occurrences in a dataset and optionally plot the rank abundance distribution
#' of taxa
#'
#' @param occdf \code{data.frame}. A dataframe of fossil occurrences.
#' @param name \code{character}. A column name with the taxonomic rank at which
#'   you want to calculate abundance (e.g. "genus").
#' @param abund_vals \code{character}. An optional column name of abundance
#'   values for a given taxon or collection. If this column is provided it
#'   will be used to calculate the abundance values. Otherwise, row wise
#'   occurrence counts will be used.
#' @param plot \code{logical}. Should a rank abundance distribution plot be
#'   generated in addition to the dataframe of results?
#' @param plot_args \code{list}. A list of optional arguments relevant to
#'   plotting. See Details for options.
#'
#' @return A \code{data.frame} containing the following columns: Taxon ID
#'   (`Taxon`), absolute abundance of each fossil taxon (`Abundance`), Relative
#'   abundance of each fossil taxon (`Rel_abundance`), natural logarithm of
#'   relative abundance of each fossil taxon (`Log_rel_abundance`), abundance
#'   rank within dataset (`Abundance_rank`).
#'
#' @details The (absolute and relative) abundance of each taxon is calculated by
#'   tabulating all rowwise counts of unique taxon occurrences (`Taxon` column)
#'   from the input `occdf`. Absolute abundance is calculated by counting the
#'   number of occurrences, while relative abundance is calculated relative to
#'   the community in the dataset. A plot of the rank abundance distribution
#'   is also returned if `plot = TRUE`. Customisable argument
#'   options to pass to `plot_args` as a list (and
#'   their defaults) for plotting include:
#'   - main = "Rank abundance"
#'   - xlab = "Abundance rank"
#'   - ylab = "Relative abundance"
#'   - col = "black"
#'   - brks = 50
#'   - log = FALSE
#'   - abs = FALSE
#'
#'   The `brks` argument will set the x axis label breaks. Use `log = TRUE` to
#'   plot the logged rank abundance, and use `abs = TRUE` to plot the
#'   absolute abundance data.
#'
#'   Caution: taxonomic synonyms will cause abundances for the accepted taxon
#'   to be distributed across the synonymised taxa, causing the accepted taxon
#'   to be under-counted. Use [tax_check()] to check for and clean synonyms.
#'
#' @examples
#'  # Grab internal data and remove NAs
#'  occdf <- subset(tetrapods, !is.na(family))
#'  # Summarise family abundance
#'  ex <- tax_rel_abun(occdf = occdf, name = "family", plot = TRUE)
#'  # Customise appearance
#'  ex <- tax_rel_abun(occdf = occdf, name = "family", plot = TRUE,
#'                      plot_args = list(log = TRUE, col = "pink", brks = 100))
#'
#'
#' @export
tax_abund <- function(occdf,
                      name = "genus",
                      abund_vals = NULL,
                      plot = FALSE,
                      plot_args = NULL) {

  #=== Handling errors ===
  if (is.data.frame(occdf) == FALSE) {
    stop("`occdf` should be a data.frame")
  }

  if (is.logical(plot) == FALSE) {
    stop("`plot` should be logical (TRUE/FALSE)")
  }

  if (!is.null(plot_args) && !is.list(plot_args)) {
    stop("`plot_args` must be either NULL, or a list")
  }

  if (name %in% colnames(occdf) == FALSE) {
    stop(paste0("`occdf` must contain `", name, "` column"))
  }

  if (!is.null(abund_vals) && abund_vals %in% colnames(occdf) == FALSE) {
    stop(paste0("`abund_vals` must either be NULL or a column in `occdf`"))
  }

  if (length(which(is.na(occdf[[name]]))) > 0) {
    stop(paste0(name, " column contains NAs"))
  }

  # Tabulate absolute abundance of (row wise) occurrences for each taxon
  if (is.null(abund_vals)) {
    occ_table <- table(occdf[[name]])
    occ_table <- data.frame(occ_table)
    colnames(occ_table) <- c("taxon", "abundance")
  } else {
    # Calculate abundances from abundance column if provided
    taxon_names <- unique(occdf[[name]])
    occ_table <- matrix(nrow = length(taxon_names), ncol = 2)
    occdf[[abund_vals]][is.na(occdf[[abund_vals]])] <- 1

    for (i in 1:length(taxon_names)) {
      occ_table[i, 1] <- taxon_names[i]
      occ_table[i, 2] <- sum(occdf[[abund_vals]][which(occdf[[name]] == taxon_names[i])])
    }

    occ_table <- data.frame(occ_table)
    colnames(occ_table) <- c("taxon", "abundance")
    occ_table$abundance <- as.numeric(occ_table$abundance)
  }

  # Calculate relative abundance for raw and log data
  occ_table$rel_abundance <- occ_table$abundance / sum(occ_table$abundance)
  occ_table$log_rel_abundance <- log(occ_table$abundance) / sum(log(occ_table$abundance))

  # Order by rank abundance
  occ_table <- occ_table[order(-occ_table$rel_abundance), ]
  rownames(occ_table) <- seq_len(nrow(occ_table))
  occ_table$abundance_rank <- seq_len(nrow(occ_table))


  # Plot rank abundance distribution
  if (plot == TRUE) {
    # Default plot args
    args <- list(y = occ_table$rel_abundance,
                 main = "Rank abundance",
                 xlab = "Abundance rank",
                 ylab = "Relative abundance",
                 col = "black",
                 brks = 50,
                 log = FALSE,
                 abs = FALSE)

    # Update any provided
    rpl <- match(names(plot_args), names(args))
    if (length(rpl) != 0) {
      args[rpl] <- plot_args
    }

    if (args$log == TRUE) {
      args$y <- occ_table$log_rel_abundance
      args$ylab <- "Log relative abundance"
    }

    if (args$abs == TRUE) {
      args$y <- occ_table$abundance
      args$ylab <- "Absolute abundance"
    }

    barplot(height = args$y, names.arg = occ_table$abundance_rank,
            xlab = args$xlab, ylab = args$ylab, main = args$main,
            col = args$col, border = NA, xaxt = "n")
    axis_breaks <- c(1, seq(args$brks, max(occ_table$abundance_rank),
                            by = args$brks))
    axis(side = 1, at = axis_breaks)
  }

  return(occ_table)

}
