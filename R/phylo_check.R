#' Check phylogeny tip names
#'
#' A function to check the list of tip names in a phylogeny against a vector of
#' taxon names.
#'
#' @param tree \code{phylo}. A phylo object. Phylogenies can be read into R from
#' .txt or .tree files containing the Newick formatted tree using
#' \code{ape::read.tree}.
#' @param list \code{character}. A vector of taxon names. Binomials can be
#' separated with either a space or an underscore. The list should not contain
#' any punctuation.
#' @param out \code{character}. Determine whether to return either the counts
#' of taxa included and not included in the tree ("counts", the default), a
#' \code{dataframe} describing which taxa are included or not included in the
#' tree ("table"), or the phylogeny trimmed to only include taxa in the provided
#' list ("tree").
#' @return If out = "counts", a summary table containing the number of taxa in
#' the list but not the tree, in the tree but not the list, and in both. If out
#'  = "table", a \code{dataframe} describing whether taxon names are present in
#' the list and/or the tree. If out = "tree", the input phylogeny trimmed to
#' only include the tips present in the list; you can save your new tree using
#' \code{ape::write.tree}.
#' @importFrom ape
#' @section Developer(s):
#' Bethany Allen
#' @section Reviewer(s):
#'
#' @examples
#' #Counts of taxon names in list, tree or both
#' list <- c("Allosaurus fragilis", "Giganotosaurus carolinii",
#' "Stegosaurus duplex", "Archaeopteryx lithographica",
#' "Iguanodon galvensis")
#' phylo_check(tree, list)
#'
#' #Table of taxon names in list, tree or both
#' phylo_check(tree, list, out = "table")
#'
#' #Trim tree to tips in the list
#' new_tree <- phylo_check(tree, list, out = "tree")
#' plot(new_tree)
#' @export

library(ape)
tree <- read.tree("data/Lloyd1_Theropoda.tree")
list <- c("allosaurus_fragilis", "Giganotosaurus carolinii", "CaudipteRyx_zoui",
          "Stegosaurus duplex", "Archaeopteryx_lithOgraphica",
          "Iguanodon_galvensis", "DiploDocus_hallorum",
          "Deinonychus_antirrhopus")

phylo_check <- function(tree, list, out = "counts") {
  #Errors for incorrect input
  if (is.null(tree)) {
    stop("Phylogeny must be provided")
  }

  if (inherits(tree, "phylo") == F) {
    stop("Phylogeny must be a phylo object")
  }

  if (is.null(list)) {
    stop("List of taxa to check against must be provided")
  }

  if (is.vector(list) == F){
    stop("List of taxa must be a vector")
  }

  if (any(grepl("[^[:alnum:][:space:]_]", list))) {
    stop("List of taxa should not contain punctuation except spaces or
         underscores")
  }

  if (out != "counts" && out != "table" && out != "tree"){
    stop("out must either be counts, table or tree")
  }

  #Function
  #Replace any spaces in taxon names with underscores
  list <- gsub(" ", "_", list)
  tree$tip.label <- gsub(" ", "_", tree$tip.label)

  #Give uniform capitalisation
  list <- gsub("^([a-z])", "\\U\\1", tolower(list), perl = T)
  tree$tip.label <- gsub("^([a-z])", "\\U\\1", tolower(tree$tip.label),
                         perl = T)

  #Create vectors of names, those in tree and those in list
  if (out == "counts" || out == "table"){
    tip_names <- tree$tip.label
    all_names <- unique(c(tip_names, list))
    names_in_tree <- (all_names %in% tip_names)
    names_in_list <- (all_names %in% list)
  }

  #Determine which names are in which lists and sum them up
  if (out == "counts"){
    tree_counts <- as.numeric(names_in_tree)
    list_counts <- as.numeric(names_in_list)
    difference <- tree_counts - list_counts

    only_tree <- length(which(difference == 1))
    only_list <- length(which(difference == -1))
    in_both <- length(which(difference == 0))

    counts <- data.frame(c("Tree and list", "Only in tree", "Only in list"),
                        c(in_both, only_tree, only_list))
    colnames(counts) <- c("Category", "Taxa")

    return(counts)
  }

  #Determine which names are in which lists and table them
  if (out == "table"){
    table <- data.frame(all_names, names_in_tree, names_in_list)
    table <- table[order(table$all_names), ]
    colnames(table) <- c("Taxon name", "Present in tree", "Present in list")

    return(table)
  }

  #Trim names not in the list from the phylogeny
  if (out == "tree"){
    list_in_tree <- match(tree$tip.label, list)
    no_match <- tree$tip.label[which(is.na(list_in_tree))]
    smaller_tree <- drop.tip(tree, no_match)

    return(smaller_tree)
  }
}
