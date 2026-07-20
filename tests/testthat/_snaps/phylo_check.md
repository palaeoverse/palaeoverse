# basic behavior works

    Code
      phylo_check()
    Condition
      Error in `phylo_check()`:
      ! Phylogeny must be provided

---

    Code
      phylo_check(1)
    Condition
      Error in `phylo_check()`:
      ! Phylogeny must be a phylo object

---

    Code
      phylo_check(data.frame())
    Condition
      Error in `phylo_check()`:
      ! Phylogeny must be a phylo object

---

    Code
      phylo_check(NA)
    Condition
      Error in `phylo_check()`:
      ! Phylogeny must be a phylo object

# arg 'list' works

    Code
      phylo_check(tree, c("foo.bar"))
    Condition
      Error in `phylo_check()`:
      ! Taxon names should not contain punctuation except spaces or
               underscores

---

    Code
      phylo_check(tree)
    Condition
      Error in `phylo_check()`:
      ! List of taxa to check against must be provided

# arg 'out' works

    Code
      phylo_check(tree, list, out = "foo")
    Condition
      Error in `phylo_check()`:
      ! out must either be 'full_table', 'diff_table', 'counts' or 'tree'

---

    Code
      phylo_check(tree, list, out = 1)
    Condition
      Error in `phylo_check()`:
      ! out must either be 'full_table', 'diff_table', 'counts' or 'tree'

---

    Code
      phylo_check(tree, list, out = NA)
    Condition
      Error in `phylo_check()`:
      ! `out` must be of length 1.

---

    Code
      phylo_check(tree, list, out = NULL)
    Condition
      Error in `phylo_check()`:
      ! `out` must be of length 1.

# arg 'sort' works

    Code
      phylo_check(tree, list, sort = "foo")
    Condition
      Error in `phylo_check()`:
      ! sort must either be 'az' or 'presence'

---

    Code
      phylo_check(tree, list, sort = 1)
    Condition
      Error in `phylo_check()`:
      ! sort must either be 'az' or 'presence'

---

    Code
      phylo_check(tree, list, sort = NA)
    Condition
      Error in `phylo_check()`:
      ! `sort` must be of length 1.

---

    Code
      phylo_check(tree, list, sort = NULL)
    Condition
      Error in `phylo_check()`:
      ! `sort` must be of length 1.

