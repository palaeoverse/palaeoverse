#' Summarise abundance of fossil occurrences
#'
#' A function to calculate the absolute and relative abundance of fossil
#' occurrences in a dataset and optionally plot the rank abundance distribution
#' of taxa
#'
#' @param occdf \code{data.frame}. A dataframe of fossil occurrences.
#' @param name \code{character}. The taxonomic rank at which you want to
#'   calculate abundance.
#' @param plot \code{logical}. Should a rank abundance distribution plot be
#'   generated?
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
#'   tabulating all occurrences of unique taxa (`Taxon` column) from the input
#'   `occdf`. Absolute abundance is calculated by counting the number of
#'   occurrences, while relative abundance is calculated relative to the
#'   community in the dataset (or subset). A plot of the rank abundance
#'   distribution is also returned if `plot = TRUE`. Customisable argument
#'   options (i.e. [graphics::par()]) to pass to `plot_args` as a list (and
#'   their defaults) for plotting include:
#'   - xlab = "Abundance rank"
#'   - ylab = "Relative abundance"
#'   - col = "black"
#'   - bg = "black"
#'   - pch = 20
#'   - cex = 1
#'
#'   @examples
#'   # Grab internal data
#'   occdf <- tetrapods
#'   # Remove NAs
#'   occdf <- subset(occdf, !is.na(order) & order != "NO_ORDER_SPECIFIED")
#'   # Summarise abundance
#'   ex <- tax_rel_abun(occdf = occdf, rank = "species", plot = TRUE)
#'   # Customise appearance
#'   ex <- tax_rel_abun(occdf = occdf, rank = "species", plot = TRUE,
#'                        plot_args = list(log = TRUE,
#'                                       pch = 21, col = "black", bg = "blue"))
#'
#'
#' @export
tax_rel_abun <- function(occdf,
                         name = "genus",
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


  # Tabulate absolute abundance of occurrences for each taxon
  occ_table <- table(occdf[[name]])
  occ_table <- data.frame(occ_table)
  colnames(occ_table) <- c("taxon", "abundance")

  # Calculate relative abundance for raw and log data
  rel_abun <- occ_table$abundance / sum(occ_table$abundance)
  log_rel_abun <- log(occ_table$abundance) / sum(log(occ_table$abundance))

  occ_table$rel_abundance <- rel_abun
  occ_table$log_rel_abundance <- log_rel_abun


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
                 bg = "black",
                 pch = 20,
                 cex = 1,
                 lty = 1,
                 lwd = 1,
                 log = FALSE)

    # Update any provided
    rpl <- match(names(plot_args), names(args))
    if (length(rpl) != 0) {
      args[rpl] <- plot_args

      if (plot_args$log != FALSE) {
        args$y <- occ_table$log_rel_abundance
        args$ylab <- "Log relative abundance"
      }
    }

    plot(x = occ_table$abundance_rank, y = args$y, axes = TRUE,
         xaxt = "n", xlab = args$xlab, ylab = args$ylab, main = args$main,
         col = args$col, bg = args$bg, pch = args$pch, cex = args$cex,
         lty = args$lty, lwd = args$lwd)
    axis(1)

  }

  return(occ_table)

}
