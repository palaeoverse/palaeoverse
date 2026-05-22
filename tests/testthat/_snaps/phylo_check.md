# phylo_check() works

    Code
      phylo_check()
    Condition
      Error in `phylo_check()`:
      ! Phylogeny must be provided

---

    Code
      phylo_check(tree)
    Condition
      Error in `phylo_check()`:
      ! List of taxa to check against must be provided

---

    Code
      phylo_check(tree = list)
    Condition
      Error in `phylo_check()`:
      ! Phylogeny must be a phylo object

---

    Code
      phylo_check(tree = tree, list = tree)
    Condition
      Error in `phylo_check()`:
      ! List of taxa must be a vector

---

    Code
      phylo_check(tree, list, out = "test")
    Condition
      Error in `phylo_check()`:
      ! out must either be 'full_table', 'diff_table', 'counts' or 'tree'

---

    Code
      phylo_check(tree, list, sort = "test")
    Condition
      Error in `phylo_check()`:
      ! sort must either be 'az' or 'presence'

---

    Code
      phylo_check(tree, list = ".")
    Condition
      Error in `phylo_check()`:
      ! Taxon names should not contain punctuation except spaces or
               underscores

