I've deprecated group in tax_check(), tax_range_time() and tax_range_strat() in favour of group_apply(), and all tests in the three test files pass. The vignettes didn't need changes: none of them use group with these functions.

Code: each function now defaults group to deprecated() and warns when it's used, as lat_bins_area(n) does. The warning reads: "The group argument of tax_range_time() is deprecated as of palaeoverse 2.0.0. Please use group_apply() instead." Passing group still works the same way, and its input checks still run.

Docs:
- group now has a deprecated badge pointing to group_apply().
- I removed the grouped examples and the text about grouping in tax_check()'s description, arguments, return value and details.
- tax_range_strat() is now listed among the functions that work with group_apply().
- There's a NEWS entry under Breaking changes, and I regenerated the man/ pages.

Examples: each function now has a group_apply() example (per class for the two range functions, per family for tax_check()), and I ran all three. The tax_check() one needs verbose = FALSE, and I had to drop NA genera first because some families have none, which makes tax_check() error.

Tests: each file has a new "works with group_apply()" test. The old group tests are now "deprecated but still works" tests, with new snapshots showing the warning. For tax_range_strat() I used expect_warning() rather than a snapshot, because a snapshot can't record the plot.

Things you should know:
- Grouped output is different. With group_apply(), taxon_id/ID restart at 1 in each group, and you get one plot per group instead of one plot with shaded group bands. tax_check() still compares names alphabetically within each group, and rows with a missing group are dropped rather than falling back to alphabetical groups.
- Leftover columns: without group, tax_range_strat() still returns stray group = NA and tmp_group columns, as it already did. I left that alone to keep this change small.
- Flaky plot test: the basic-plot vdiffr test in test-tax_range_strat.R failed twice when I ran it in the REPL after the other test files. It passed when run alone and on the next run in the same order, so it seems to depend on graphics state left in the session rather than on this change.
- My attempt to compare against the original code with git stash failed because .git is read-only in my environment, so nothing was stashed and your working tree is unchanged.