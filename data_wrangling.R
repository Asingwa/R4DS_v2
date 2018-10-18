#### DATA WRANGLING ####
# This script will focus on the chapters in the Wrangle section of the book; 9 to 16.
 
 
# Creating tibbles
library(tidyverse)

# Let's attach a data frame and see it's class
attach(iris)  
class(iris)

# Now to coerce the data frame to a tibble
iris_tib <- as_tibble(iris)
class(iris_tib)

# Creating a tibble from individual vectors
tbl <- tibble(
  x = 1:5,
  y = x^3,
  z = x*2+y/4
)

# Transposed tibble tribble() is customised for data entry in code: column headings are defined by
# formulas (i.e they start with ~), and entries are separated by commas.
tribble(
  ~x, ~y, ~z,
  #---/--/---
  "a", 2, 3.6,
  "b", 1, 8.5
)

# Printing tibble with controlled number of rows and columns
print(iris_tib, n = 20, width = Inf) 

# Subsetting
iris_tib[[2]]
iris_tib$Sepal.Length
iris_tib[["Sepal.Length"]]

# Non-syntactic
annoying <- tibble(
  `1` = 1:10,
  `2` = `1` * 2 + rnorm(length(`1`))
)
annoying[[1]]
annoying[[`1`]]
annoying$`1`

ggplot(data = annoying, mapping = aes(x=`1`, y=`2`))+
  geom_point()

# Create new column called 3 which is 2 divided by 1
annoying <- mutate(annoying, `3` = `2`/1)

rename(annoying, one = `1`, two = `2`, three = `3`)

# Factors
library(forcats) # Package providing tools for dealing with categorical variables.

x1 <- c("Dec","Apr","Jan","Mar")
sort(x1)
X2 <- c("Dec","Apr","Jam","Mar")
# Create levels
month_levels <- c("Jan","Feb","Mar","Apr","May","Jun",
                  "Jul","Aug","Sep","Oct","Nov","Dec")
y1 <- factor(x1, levels = month_levels)
sort(y1)

y2 <- factor(X2, levels = month_levels) # Silently converts values not in levels to NA
sort(y2)

# To get a warning when there is a value not in the levels use readr:parse_factor
y2 <- parse_factor(X2, levels = month_levels)


attach(gss_cat)

gss_cat %>%
  count(race)
