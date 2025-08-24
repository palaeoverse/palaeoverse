#' Summarise abundance of fossil occurrences
#'
#' A function to calculate absolute/relative abundance and rank abundance distribution of taxa in a fossil occurrence dataset
#'
#' @param occdf \code{dataframe}. A dataframe of fossil occurrences.
#' @param rank \code{character}. The taxonomic rank at which you want to calculate abundance.
#' @param plot \code{logical}. Should a rank abundance distribution plot be generated?
#' @param plot_args \code{list}. A list of optional arguments relevant to
#'   plotting. See Details for options.
#'
#' @return A \code{dataframe} containing the following columns: Taxon ID (`Taxon`),
#'  absolute abundance of each fossil taxon (`Abundance`),
#'  Relative abundance of each fossil taxon (`Rel_abundance`),
#'  natural logarithm of relative abundance of each fossil taxon (`Log_rel_abundance`),
#'  abundance rank within dataset (`Abundance_rank`).
#'
#'  @details The (absolute and relative) abundance of each taxon is calculated by tabulating all occurrences of
#'   unique taxa (`Taxon` column) from the input `occdf`. Absolute abundance is calculated by counting the number of occurrences,
#'   while relative abundance is calculated relative to the community in the dataset (or subset).
#'   A plot of the rank abundance distribution is also returned if `plot = TRUE`.
#'   Customisable argument options
#'   (i.e. [graphics::par()]) to pass to `plot_args` as a list (and their
#'   defaults) for plotting include:
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
#'   ex <- tax_range_time(occdf = occdf, rank = "species", plot = TRUE,
#'                        plot_args = list(ylab = "Relative Abundance",
#'                                       pch = 21, col = "black", bg = "blue"))
#'
#'
#' @export
tax_rel_abun <- function(occdf,
                         rank = "genus",
                         plot = FALSE,
                         plot_args = NULL) {

  #=== Handling errors ===
  if (is.data.frame(occdf) == FALSE) {
    stop("`occdf` should be a dataframe")
  }

  if (is.logical(plot) == FALSE) {
    stop("`plot` should be logical (TRUE/FALSE)")
  }

  if (!is.null(plot_args) && !is.list(plot_args)) {
    stop("`plot_args` must be either NULL, or a list")
  }


  # Replace 'species' with 'accepted name' as is standard on PBDB
  if (rank == "species") {
    rank <- "accepted_name"
  }


  # Tabulate absolute abundance of occurrences for each taxon
  occ_table <- table(occdf[[rank]])
  occ_table <- data.frame(occ_table)
  colnames(occ_table) <- c('Taxon', 'Abundance')

  # Calculate relative abundance for raw and log data
  rel_abun <- occ_table$Abundance/max(occ_table$Abundance)
  log_rel_abun <- log(occ_table$Abundance)/max(log(occ_table$Abundance))

  occ_table$Rel_abundance <- rel_abun
  occ_table$Log_rel_abundance <- log_rel_abun


  # Order by rank abundance
  occ_table <- occ_table[order(-occ_table$Rel_abundance), ]
  rownames(occ_table) <- c(1:nrow(occ_table))
  occ_table$Abundance_rank <- c(1:1:nrow(occ_table))


  # Plot rank abundance distribution
  if (plot == TRUE) {
    # Default plot args
    args <- list(main = "Rank abundance",
                 xlab = "Abundance rank",
                 ylab = "Relative abundance",
                 col = "black",
                 bg = "black",
                 pch = 20,
                 cex = 1,
                 lty = 1,
                 lwd = 1)



    xlim <- c(1, max(occ_table$Abundance_rank))
    ylim <- c(0, 1)
    plot(x = NA, y = NA, xlim = xlim, ylim = ylim, axes = TRUE,
         xaxt = "n", xlab = NA, ylab = args$ylab, main = args$main)
    points(x = occ_table$Abundance_rank, y = occ_table$Log_rel_abundance,
           pch = args$pch, col = args$col, bg = args$bg,
           cex = args$cex)
    axis(1)
    title(xlab = args$xlab, line = 4)

  }

  return(occ_table)

}
