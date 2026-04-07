

## Summarizing data and comparing across groups

library(NHANES)
library(lattice)

nh_adults <- subset(NHANES, Age >= 20, 
                    select = c(Height, Weight, Gender, Race1))

mean(NHANES$Height, na.rm = TRUE)

## Sample mean height for Males and Females
table(nh_adults$Gender)

mean(subset(nh_adults, Gender == "female")$Height, na.rm = TRUE)
mean(subset(nh_adults, Gender == "male")$Height, na.rm = TRUE)

table(nh_adults$Race1)

mean(subset(nh_adults, Race1 == "Black")$Height, na.rm = TRUE)
mean(subset(nh_adults, Race1 == "Black" & Gender == "female")$Height, na.rm = TRUE)

## How can we do this more systematically?

## Two options: (a) base R functions, (b) dplyr

library(dplyr)

nh_adults |> 
  group_by(Gender, Race1) |>
  summarise(mean(Height, na.rm = TRUE))

## same as
summarise(group_by(nh_adults, Gender, Race1), mean(Height, na.rm = TRUE))


NHANES |>
  filter(Age >= 20) |>
  select(Height, Weight, Gender, Race1) |> 
  group_by(Gender, Race1) |>
  summarise(MeanHt = mean(Height, na.rm = TRUE)) |>
  dotplot(reorder(Race1, MeanHt) ~ MeanHt, 
          groups = Gender, auto.key = TRUE)

nh_summary <- 
  NHANES |>
  filter(Age >= 20) |>
  select(Height, Weight, Gender, Race1) |> 
  group_by(Gender, Race1) |>
  summarise(MeanWt = mean(Weight, na.rm = TRUE),
            MeanHt = mean(Height, na.rm = TRUE),
            CorHW = cor(Height, Weight, 
                        use = "pairwise.complete.obs"))

cor_all <- with(nh_adults, cor(Height, Weight, use = "pairwise.complete.obs"))

xyplot(nh_summary,
       reorder(Race1, CorHW) ~ CorHW, 
       groups = Gender, auto.key = list(columns = 2),
       abline = list(v = cor_all, h = 1:5, col = "grey90"))

xyplot(Weight ~ Height | Race1 + Gender, nh_adults, 
       cex= 0.5, alpha = 0.2)

xyplot(Weight ~ Height, nh_adults, 
       groups = Gender,
       cex= 0.5, alpha = 0.2)

## data from the "Old Faithful" geyser in USA
plot(faithful)

## A well known example of Simpson's paradox

apply(UCBAdmissions, c(1, 2), sum) |> t() |> 
  barchart(auto.key = list(columns = 2))

barchart(UCBAdmissions[,,1] |> t(), auto.key = list(columns = 2))
barchart(UCBAdmissions[,,2] |> t(), auto.key = list(columns = 2))
barchart(UCBAdmissions[,,3] |> t(), auto.key = list(columns = 2))
barchart(UCBAdmissions[,,4] |> t(), auto.key = list(columns = 2))
barchart(UCBAdmissions[,,5] |> t(), auto.key = list(columns = 2))
barchart(UCBAdmissions[,,6] |> t(), auto.key = list(columns = 2))

library(vcd)
mosaic(aperm(UCBAdmissions, c(3, 2, 1)))

