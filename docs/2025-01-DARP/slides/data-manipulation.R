

## data preparation and summarization

library(NHANES)

## Task 1: Subset / filter

nhsub <- subset(NHANES, Age >= 21)
nhsub <- nhsub[c("Race1", "Age", "Gender", "Height", "Weight")]

## other methods
## - subset()

## The above is equivalent to:

nhsub <- subset(NHANES, 
                subset = Age >= 21, 
                select = c("Race1", "Age", "Gender", "Height", "Weight"))


nhsub <- subset(NHANES, 
                subset = Age >= 21, 
                select = c(Race1, Age, Gender, Height, Weight))

## dplyr - grammar for data manipulation

## - dplyr::filter()
## - dplyr::select()

nhsub <- dplyr::filter(NHANES, Age >= 21)
nhsub <- dplyr::select(nhsub, c("Race1", "Age", "Gender", "Height", "Weight"))

## - pipeline

subset(NHANES, 
       subset = Age >= 21, 
       select = c("Race1", "Age", "Gender", "Height", "Weight")) |> str()


NHANES |>
  dplyr::filter(Age >= 21) |> 
  dplyr::select(c(Race1, Age, Gender, Height, Weight)) |>
  str()

NHANES |>
  subset(subset = Age >= 21) |>
  subset(select = c(Race1, Age, Gender, Height, Weight)) |>
  str()

## Another efficient approach to all these things: data.table package 
## (which we will not discuss)

## Task 2: Add derived variables

## Example: calculate BMI = kg / m^2

## - dplyr::mutate()

NHANES |>
  dplyr::filter(Age >= 21) |> 
  dplyr::select(c(Race1, Age, Gender, Height, Weight)) |>
  dplyr::mutate(HeightM = Height / 100, 
                BMI = Weight / HeightM^2) |>
  dplyr::select(!Height) |>
  str() |>
  system.time()

## doin the same thing stepwise

nhsub1 <- 
  NHANES |>
  dplyr::filter(Age >= 21) |> 
  dplyr::select(c(Race1, Age, Gender, Height, Weight))

nhsub2 <- 
  nhsub1 |>
    dplyr::mutate(HeightM = Height / 100, 
                  BMI = Weight / HeightM^2)

str(nhsub)

## - transform()

NHANES |>
  subset(Age >= 21) |> 
  subset(select = c(Race1, Age, Gender, Height, Weight)) |>
  transform(HeightM = Height / 100) |> 
  transform(BMI = Weight / HeightM^2) |>
  str() |>
  system.time()

## - within()

NHANES |>
  subset(Age >= 21) |> 
  subset(select = c(Race1, Age, Gender, Height, Weight)) |>
  within({
    HeightM <- Height / 100
    BMI <- Weight / HeightM^2
    rm(Height)
  }) |>
  str() |>
  system.time()

## Task 3: summarize by groups

## Example: Find mean BMI median BMI, normal CI

## - tapply()
## - dplyr::group_by()
## - dplyr::summarise()



