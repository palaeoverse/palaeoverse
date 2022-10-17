#' Check phylogeny tip names
#'
#' A function to check the list of tip names in a phylogeny against a vector of
#' taxon names, and if desired, to trim the phylogeny to only include taxon
#' names within the vector.
#'
#' @param tree \code{phylo}. A phylo object containing the phylogeny.
#' @param list \code{character}. A vector of taxon names. Binomials can be
#' separated with either a space or an underscore. The names should not contain
#' any other punctuation.
#' @param out \code{character}. Determine whether to return either a
#' \code{dataframe} describing which taxa are included or not included in the
#' tree ("full_table", the default), the same table but with taxa included in
#' both the tree and the list removed ("diff_table"), the counts of taxa
#' included and not included in the tree ("counts"), or the phylogeny trimmed to
#' only include taxa in the provided list ("tree").
#' @param sort \code{character}. If out = "full_table" or out = "diff_table",
#' sort the names by presence in the tree ("presence", the default), or
#' alphabetically ("az").
#' @return If out = "full_table", a \code{dataframe} describing whether taxon
#' names are present in the list and/or the tree. If out = "diff_table", a
#' \code{dataframe} describing which taxon names are present in the list or the
#' tree, but not both. If out = "counts", a summary table containing the number
#' of taxa in the list but not the tree, in the tree but not the list, and in
#' both. If out = "tree", a phylo object consisting of the input phylogeny
#' trimmed to only include the tips present in the list.
#' @details Phylogenies can be read into R from .txt or .tree files containing
#' the Newick formatted tree using [ape::read.tree()], and can be saved as
#' files using [ape::write.tree()]. When out = "tree", tips are trimmed using
#' [ape::drop.tip()]; if your tree is not ultrametric (i.e. the tip dates are
#' not all the same), we recommend using [paleotree::fixRootTime()] to readjust
#' your branch lengths following pruning.
#' @importFrom ape drop.tip
#' @section Developer(s):
#' Bethany Allen
#' @section Reviewer(s):
#' William Gearty & Pedro Godoy
#'
#' @examples
#' #Read in example tree of ceratopsians from paleotree
#' library(paleotree)
#' data(RaiaCopesRule)
#' plot(ceratopsianTreeRaia, cex = 0.5)
#'
#' #Specify list of names
#' dinosaurs <- c("Nasutoceratops_titusi", "Diabloceratops_eatoni",
#' "Zuniceratops_christopheri", "Psittacosaurus_major",
#' "Psittacosaurus_sinensis", "Avaceratops_lammersi",
#' "Xenoceratops_foremostensis", "Leptoceratops_gracilis",
#' "Triceratops_horridus", "Triceratops_prorsus")
#'
#' #Table of taxon names in list, tree or both
#' ex1 <- phylo_check(tree = ceratopsianTreeRaia, list = dinosaurs)
#'
#' #Counts of taxa in list, tree or both
#' ex2 <- phylo_check(tree = ceratopsianTreeRaia, list = dinosaurs,
#'                    out = "counts")
#'
#' #Trim tree to tips in the list
#' my_ceratopsians <- phylo_check(tree = ceratopsianTreeRaia, list = dinosaurs,
#' out = "tree")
#' plot(my_ceratopsians)
#' @export

phylo_check <- function(tree = NULL, list = NULL, out = "full_table",
                        sort = "presence") {
  #Errors for incorrect input
  if (is.null(tree)) {
    stop("Phylogeny must be provided")
  }

  if (inherits(tree, "phylo") == FALSE) {
    stop("Phylogeny must be a phylo object")
  }

  if (is.null(list)) {
    stop("List of taxa to check against must be provided")
  }

  if (is.vector(list) == FALSE) {
    stop("List of taxa must be a vector")
  }

  if (any(grepl("[^[:alnum:][:space:]_]", list))) {
    stop("Taxon names should not contain punctuation except spaces or
         underscores")
  }

  if (out != "counts" && out != "full_table" && out != "diff_table" &&
      out != "tree") {
    stop("out must either be 'full_table', 'diff_table', 'counts' or 'tree'")
  }

  if (sort != "az" && sort != "presence") {
    stop("sort must either be 'az' or 'presence'")
  }

  if (out != "full_table" && out != "diff_table" && sort != "presence") {
    warning("sort is redundant when using outputs other than 'full_table' or
            'diff_table'")
  }

  #Function
  #Replace any spaces in taxon names with underscores
  list <- gsub(" ", "_", list)
  tree$tip.label <- gsub(" ", "_", tree$tip.label)

  #Give uniform capitalisation
  list <- gsub("^([a-z])", "\\U\\1", tolower(list), perl = TRUE)
  tree$tip.label <- gsub("^([a-z])", "\\U\\1", tolower(tree$tip.label),
                         perl = TRUE)

  #Create vectors of names, those in tree and those in list
  if (out == "counts" || out == "full_table" || out == "diff_table") {
    tip_names <- tree$tip.label
    all_names <- unique(c(tip_names, list))
    names_in_tree <- (all_names %in% tip_names)
    names_in_list <- (all_names %in% list)
  }

  #Determine which names are in which lists and table them
  if (out == "full_table" || out == "diff_table") {
    table <- data.frame(all_names, names_in_tree, names_in_list)

    if (sort == "az") {
      table <- table[order(table$all_names), ]
    } else {
      table <- table[order(table$names_in_list, decreasing = TRUE), ]
    }
  }

  if (out == "diff_table") {
    table <- subset(table, names_in_tree == FALSE | names_in_list == FALSE)
  }

  if (out == "full_table" || out == "diff_table") {
    colnames(table) <- c("Taxon name", "Present in tree", "Present in list")
    return(table)
  }

  #Determine which names are in which lists and sum them up
  if (out == "counts") {
    tree_counts <- as.numeric(names_in_tree)
    list_counts <- as.numeric(names_in_list)
    difference <- tree_counts - list_counts

    only_tree <- length(which(difference == 1))
    only_list <- length(which(difference == -1))
    in_both <- length(which(difference == 0))

    counts <- data.frame(c("Tree and list", "Only in tree", "Only in list"),
                        c(in_both, only_tree, only_list))
    colnames(counts) <- c("Category", "Number of taxa")

    return(counts)
  }

  #Trim names not in the list from the phylogeny
  if (out == "tree") {
    list_in_tree <- match(tree$tip.label, list)
    no_match <- tree$tip.label[which(is.na(list_in_tree))]
    smaller_tree <- drop.tip(tree, no_match)

    return(smaller_tree)
  }
}
