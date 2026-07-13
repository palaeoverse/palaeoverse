library(rmarkdown)
library(git2r)

# Delete RDS files and recompile the vignettes to create the RDS files again
rds_files <- list.files("vignettes/_data", pattern = "\\.rds$")
rds_files <- file.path("vignettes/_data", rds_files)
file.remove(rds_files)

rmarkdown::render("vignettes/phanerozoic-reefs.Rmd")
rmarkdown::render("vignettes/tetrapod-biodiversity.Rmd")

# Get the list of changed files
git_status <- git2r::status()
unstaged_modified <- git_status[["unstaged"]][
  names(git_status[["unstaged"]]) == "modified"
] |>
  unlist()
staged_modified <- git_status[["staged"]][
  names(git_status[["staged"]]) == "modified"
] |>
  unlist()

# If the RDS files have changed then it means they were outdated
changed_rds_files <- rds_files[
  rds_files %in% c(staged_modified, unstaged_modified)
]

if (length(changed_rds_files) > 0) {
  stop(
    "The following RDS files have changed:\n",
    paste0("* ", changed_rds_files, collapse = "\n")
  )
}
