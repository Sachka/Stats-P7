# @author: Hermes Martinez
# functions.R file for assignment 2

# Exercice 1a

# Sum values in a column of a data frame.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d, provided as a string
#
# RETURN VALUE:
# if the specified column exists and contains numbers, returns the sum of
# all values in the column; otherwise, returns NULL

# [YOUR CODE HERE]
sum_column <- function(d, var) {
  result <- NULL
  x <- d[[var]]
  if (!is.null(x)) {
    if (any(is.numeric(x))) {
      result <- sum(x)
    }
  }
  return(result)
}

# Exercice 1b

# Sum values in a vector.
#
# ARGUMENTS:
# vec: a vector
#
# RETURN VALUE:
# if the vector contains numbers, returns the sum of
# all values; otherwise, returns NULL

my_sum <- function(vec) {
  result <- 0
  if (is.null(vec)) {
    return(NULL)
    }
  if (!any(is.numeric(vec))) {
    return(NULL)
    }
  for (x in vec) {
    if (is.numeric(x)) {
      result <- result + x
    }
  }
  return(result)
}


#Exercice 1c

# "Sum divided by" in a vector
#
# ARGUMENTS:
# vec: a vector
# k: a number
# 
# RETURN VALUE:
# The sum of vec divided by n
# return null if vec is an invalid character / empty or if n is 0 or invalid

sum_divided_by <- function(vec, n) {
  if (is.null(n) || k == 0 || !is.numeric(n)) {
    return(NULL)
    }
  if (is.null(vec)) {
    return(NULL)
  }
  if (!any(is.numeric(vec))) {
    return(NULL)
    }
  result <- 0
  for (x in vec) {
    result <- result + x
  }
  return(result / n)
}

# Exercice 1d

# Calculate the mean of a vector using sum_divided_by()
# 
# ARGUMENTS:
# vec: a vector


my_mean <- function(vec) {
  if(is.null(vec)) {
    return(NULL)
    }
  len_of_vec <- length(vec)
  mean_of_vec <- sum_divided_by(vec, len_of_vec)
  return(mean_of_vec)
}

#---------------ggplot---------------#

#Exercice 2a

# Return a violin plot.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d containing the dependent variable, provided as a
#     string
# grouping_var: the name of a column of d containing a grouping variable,
#               provided as a string
#
# RETURN VALUE:
# A ggplot plot object containing a violin plot, grouped by the values
# of the grouping variable.

grouped_violin_plot <- function(d, var, grouping_var) {
  p <- ggplot2::ggplot(d, ggplot2::aes_string(y=var,
                                              x=grouping_var,
                                              fill=grouping_var))
  p <- p + ggplot2::geom_violin(colour="blue")
  return(p)
}

# Exercice 3a

# Difference in the medians between two groups.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d containing the dependent variable, provided as a string
# grouping_var: the name of a column of d containing a grouping variable, provided as a string
# group1: the value of grouping_var that corresponds to the first group
# group2: the value of grouping_var that corresponds to the second group
#
# RETURN VALUE:
# The median value of var for the first group, minus the median value of var for the second
# group.
#

difference_in_medians <- function(d, var, grouping_var, group1, group2) {
  d_1 <- dplyr::filter(d, get(grouping_var) == group1)
  d_2 <- dplyr::filter(d, get(grouping_var) == group2)
  # YOUR CODE HERE: assign the difference in the medians to to the variable 'result'
  result <- median(d_1[[var]]) - median(d_2[[var]])
  return(result)
}




# Exercice 3b

# Randomize the order of a column.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d containing the variable to randomize,
#      provided as a string
#
# RETURN VALUE:
# A data frame or tibble exactly the same as d, except with the order of
# var permuted randomly.
#
randomize <- function(d, var) {
  n <- nrow(d)
  d[[var]] <- sample(d[[var]], n)
  return(d) }


# Exercice 3c
# Perform a permutation test for two groups.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of the column in d on which the test statistic will be calculated,
#      provided as a string
# grouping_var: the name of the column in d which gives the grouping
# group1: the value of grouping_var corresponding to the first group
# group2: the value of grouping_var corresponding to the second group
# statistic: a function yielding a test statistic, which takes as input
#            a data frame, the name of a variable on which to calculate the
#            test statistic, the name of a grouping variable, the value of
#            the grouping variable corresponding to the first group, and
#            the value of the grouping variable corresponding to the second
#            group
# n_samples: the number of permutation samples to draw (default: 9999)
#
# RETURN VALUE:
#
# A list containing two elements:
#
#  - observed: the value of statistic() in d
#  - permuted: a vector containing the values of statistic() under n_samples
#              permutations
#
permutation_twogroups <- function(d, var, grouping_var, group1, group2, statistic,
                                  n_samples=9999) {
  observed_statistic <- statistic(d, var, grouping_var, group1, group2)
  permutation_statistics <- rep(0, n_samples)
  for (i in 1:n_samples) {
    random_d <- randomize(d, var)
    permutation_statistics[i] <- statistic(random_d, var, grouping_var, group1, group2)
  }
  result <- list(observed=observed_statistic,
                 permuted=permutation_statistics)
  return(result)
}

# Exercice 3f
# Test statistics #
new_test_statistic <- function(d, var, grouping_var, group1, group2) {
  d_1 <- dplyr::filter(d, get(grouping_var) == group1)
  d_2 <- dplyr::filter(d, get(grouping_var) == group2)
  result <- mean(d_1[[var]]) - mean(d_2[[var]])
  return(result)
}

# Exercice 3g 
# Calculating p-values
permutation_pvalue_right <- function(p) {
  n_above <- sum(p$permuted >= p$observed)
  n_samples <- length(p$permuted)
  return((n_above + 1)/(n_samples + 1))
}
permutation_pvalue_left <- function(p) {
  n_below <- sum(p$permuted <= p$observed)
  n_samples <- length(p$permuted)
  return((n_below + 1)/(n_samples + 1))
}

# Exercice 3h
# TODO 3h


