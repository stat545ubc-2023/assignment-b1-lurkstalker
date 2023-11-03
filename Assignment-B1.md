Assignment-B1
================
Tongshuai Zhang
2023-11-02

# Exercise 1: Make a Function

In the MDA project, I repeated use the used **group_by() %\>%
summarise()** statement for **vancouver_trees** dataset Therefore, it
would be appropriate to create a generic function that group by a
specific column and summarize the proportion calculated by another
specific column in the dataset.

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(datateachr) # <- might contain the data you picked!
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ forcats   1.0.0     ✔ readr     2.1.4
    ## ✔ ggplot2   3.4.3     ✔ stringr   1.5.0
    ## ✔ lubridate 1.9.2     ✔ tibble    3.2.1
    ## ✔ purrr     1.0.2     ✔ tidyr     1.3.0

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
group_distinct_col_count_prop <- function(dataset, group_col, summary_col) {
  stopifnot(is.data.frame(dataset))
  
  # Check if group_col and summary_col are present in the dataset
  if (!(group_col %in% colnames(dataset))) {
    stop("Error: group_col is not a column in the dataset.")
  }
  if (!(summary_col %in% colnames(dataset))) {
    stop("Error: summary_col is not a column in the dataset.")
  }
  
  total_summary_col_distinct_count <- n_distinct(dataset[[summary_col]])
  
  result <- dataset %>%
    group_by(.data[[group_col]]) %>%
    summarise(total = n_distinct(.data[[summary_col]])) %>%
    mutate(proportion = total / total_summary_col_distinct_count)
  return(result)
}
```

# Exercise 2: Document the Function

``` r
#' @title Group by a Column and Calculate Proportion of Distinct Values in Another Column
#' @description 
#' This function groups a given dataset by a specified column and then calculates the  
#' proportion of distinct values in another specified column within each group. 
#' It is designed to provide a quick and generic way to summarize data, which is useful in exploratory data analysis.
#' @param dataset: A data frame containing the data to be grouped and summarized.
#' @param group_col: A string representing the name of the column to group by.
#' @param summary_col: A string representing the name of the column to calculate the proportion of distinct values.
#' @return: A data frame containing the grouping column, the total count of distinct values in the summary_col for each group, and 
#' the proportion of these distinct values relative to the total distinct values in summary_col across the entire dataset.

group_distinct_col_count_prop <- function(dataset, group_col, summary_col) {
  stopifnot(is.data.frame(dataset))
  
  # Check if group_col and summary_col are present in the dataset
  if (!(group_col %in% colnames(dataset))) {
    stop("Error: group_col is not a column in the dataset.")
  }
  if (!(summary_col %in% colnames(dataset))) {
    stop("Error: summary_col is not a column in the dataset.")
  }
  
  total_summary_col_distinct_count <- n_distinct(dataset[[summary_col]], na.rm = TRUE)
  
  result <- dataset %>%
    group_by(.data[[group_col]]) %>%
    summarise(total = n_distinct(.data[[summary_col]], na.rm = TRUE)) %>%
    mutate(proportion = total / total_summary_col_distinct_count)
  return(result)
}
```

# Exercise 3: Include examples

## Example 01:

``` r
# Check the number of species and its proportion to the total species for every distinct neighbourhood_name
vancouver_trees_summary_by_neighbourhood <- group_distinct_col_count_prop(vancouver_trees, group_col = "neighbourhood_name", summary_col = "species_name")
vancouver_trees_summary_by_neighbourhood
```

    ## # A tibble: 22 × 3
    ##    neighbourhood_name       total proportion
    ##    <chr>                    <int>      <dbl>
    ##  1 ARBUTUS-RIDGE              121      0.428
    ##  2 DOWNTOWN                    79      0.279
    ##  3 DUNBAR-SOUTHLANDS          161      0.569
    ##  4 FAIRVIEW                   119      0.420
    ##  5 GRANDVIEW-WOODLAND         146      0.516
    ##  6 HASTINGS-SUNRISE           176      0.622
    ##  7 KENSINGTON-CEDAR COTTAGE   159      0.562
    ##  8 KERRISDALE                 138      0.488
    ##  9 KILLARNEY                  122      0.431
    ## 10 KITSILANO                  171      0.604
    ## # ℹ 12 more rows

## Example 02:

``` r
# Check the number of species and its proportion to the total species for every distinct std_street
vancouver_trees_summary_by_str_street <- group_distinct_col_count_prop(vancouver_trees, group_col = "std_street", summary_col = "species_name")
vancouver_trees_summary_by_str_street
```

    ## # A tibble: 805 × 3
    ##    std_street   total proportion
    ##    <chr>        <int>      <dbl>
    ##  1 ABBOTT ST       10    0.0353 
    ##  2 ABERDEEN ST     12    0.0424 
    ##  3 ADANAC ST       50    0.177  
    ##  4 ADERA ST        42    0.148  
    ##  5 AISNE ST         2    0.00707
    ##  6 ALAMEIN AV       5    0.0177 
    ##  7 ALBERNI ST      20    0.0707 
    ##  8 ALBERTA ST      44    0.155  
    ##  9 ALDER ST         8    0.0283 
    ## 10 ALEXANDER ST    22    0.0777 
    ## # ℹ 795 more rows

## Example 03:

``` r
vancouver_trees
```

    ## # A tibble: 146,611 × 20
    ##    tree_id civic_number std_street    genus_name species_name cultivar_name  
    ##      <dbl>        <dbl> <chr>         <chr>      <chr>        <chr>          
    ##  1  149556          494 W 58TH AV     ULMUS      AMERICANA    BRANDON        
    ##  2  149563          450 W 58TH AV     ZELKOVA    SERRATA      <NA>           
    ##  3  149579         4994 WINDSOR ST    STYRAX     JAPONICA     <NA>           
    ##  4  149590          858 E 39TH AV     FRAXINUS   AMERICANA    AUTUMN APPLAUSE
    ##  5  149604         5032 WINDSOR ST    ACER       CAMPESTRE    <NA>           
    ##  6  149616          585 W 61ST AV     PYRUS      CALLERYANA   CHANTICLEER    
    ##  7  149617         4909 SHERBROOKE ST ACER       PLATANOIDES  COLUMNARE      
    ##  8  149618         4925 SHERBROOKE ST ACER       PLATANOIDES  COLUMNARE      
    ##  9  149619         4969 SHERBROOKE ST ACER       PLATANOIDES  COLUMNARE      
    ## 10  149625          720 E 39TH AV     FRAXINUS   AMERICANA    AUTUMN APPLAUSE
    ## # ℹ 146,601 more rows
    ## # ℹ 14 more variables: common_name <chr>, assigned <chr>, root_barrier <chr>,
    ## #   plant_area <chr>, on_street_block <dbl>, on_street <chr>,
    ## #   neighbourhood_name <chr>, street_side_name <chr>, height_range_id <dbl>,
    ## #   diameter <dbl>, curb <chr>, date_planted <date>, longitude <dbl>,
    ## #   latitude <dbl>

``` r
# Check the number of species and its proportion to the total species for every distinct genus
vancouver_trees_summary_by_genus <- group_distinct_col_count_prop(vancouver_trees, group_col = "genus_name", summary_col = "species_name")
vancouver_trees_summary_by_genus
```

    ## # A tibble: 97 × 3
    ##    genus_name  total proportion
    ##    <chr>       <int>      <dbl>
    ##  1 ABIES           8    0.0283 
    ##  2 ACER           31    0.110  
    ##  3 AESCULUS        6    0.0212 
    ##  4 AILANTHUS       1    0.00353
    ##  5 ALBIZIA         1    0.00353
    ##  6 ALNUS           4    0.0141 
    ##  7 AMELANCHIER     4    0.0141 
    ##  8 ARALIA          1    0.00353
    ##  9 ARAUCARIA       1    0.00353
    ## 10 ARBUTUS         2    0.00707
    ## # ℹ 87 more rows

# Exercise 4: Test the Function

``` r
library(testthat)
```

    ## 
    ## Attaching package: 'testthat'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     is_null

    ## The following objects are masked from 'package:readr':
    ## 
    ##     edition_get, local_edition

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     matches

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     matches

``` r
# Define the function group_distinct_col_count_prop here

# Sample test data
test_data <- data.frame(
  Species = c("Oak", "Maple", "Oak", "Pine", "Maple", "Pine", NA),
  Height = c(10, 15, 20, 25, 30, 35, 40)
)

# Test cases
test_that("group_distinct_col_count_prop function works correctly", {
  
  # Test 1: Vector with no NA's
  result1 <- group_distinct_col_count_prop(test_data[1:6, ], "Species", "Height")
  result1
  expect_equal(nrow(result1), 3)
  expect_equal(sum(result1$proportion), 1)
  
  # Test 2: Vector that has NA's
  result2 <- group_distinct_col_count_prop(test_data, "Species", "Height")
  expect_equal(nrow(result2), 4)
  expect_equal(sum(result2$proportion, na.rm = TRUE), 1)
  
  # Test 3: Vector of length 0
  empty_data <- test_data[FALSE, ]
  result3 <- group_distinct_col_count_prop(empty_data, "Species", "Height")
  expect_equal(nrow(result3), 0)
})
```

    ## Test passed 😸

``` r
# Test 4: Vector that hasn't the column specified in the function arguments
test_that("group_distinct_col_count_prop handles non-existent columns correctly", {
  expect_error(group_distinct_col_count_prop(test_data, "Species", "Heights"),
               "Error: summary_col is not a column in the dataset.")
})
```

    ## Test passed 🎊
