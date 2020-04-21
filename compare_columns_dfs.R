## checks if two data frames have the same unique values on a variable and
## prints informative output to console.

# E.g., if you have a long and a wide version of the data, you want to check
# if all subjects in one data frame are present in the other and viceversa.

compare_columns_dfs <- function (
  my_df1,
  my_df2,
  vars  # a single string or string vector if comparing several columns
) {

  all_vars_attested <-
    sum(!vars %in% names(my_df1)) == 0 &
    sum(!vars %in% names(my_df2)) == 0
  if (! all_vars_attested) {
    stop("All variables need to exist in both data frames!")
  }

  df1_name <- deparse(substitute(my_df1))
  df2_name <- deparse(substitute(my_df2))

  # function that does the comparison
  compare <- function (df1, name1, df2, name2, var) {

    df2_missing <- sum(! unique(df1[[var]]) %in% unique(df2[[var]]))

    if (df2_missing == 0) {
      cat(sprintf(
        "\nLooks good! All values in '%s$%3$s' are also in '%s$%3$s'.\n",
        name1, name2, var
      ))
    } else {
      uni_df1 <- unique(df1[[var]])
      uni_df2 <- unique(df2[[var]])
      missing <- uni_df1[!uni_df1 %in% uni_df2]
      cat(sprintf(
        "\n%s values in '%s$%4$s' missing from '%s$%4$s'. ",
        df2_missing, name1, name2, var
      ))
      cat("Namely:\n", missing, "\n", sep = "")
    }
    # cat("\n")
  }
  # Make all possible comparisons
  for (myvar in vars) {
    cat("---------------------------------\n")
    cat("Comparing '", myvar, "':\n", sep = "")
    compare(my_df1, df1_name, my_df2, df2_name, var = myvar)
    compare(my_df2, df2_name, my_df1, df1_name, var = myvar)
    cat("---------------------------------\n")
  }
}

# # E.g., use it like this:
# compare_columns_dfs(dataframe1, dataframe2, c("subject", "dyad"))
