
survey <- read.csv("https://deepayan.github.io/BSDS/2024-01-DE/data/bsds-survey.csv")

str(survey)

table(survey$centre)
table(survey$centre, survey$dress)


xtabs(~ centre, data = survey)

xtabs(~ centre + dress, data = survey)
table(survey$centre, survey$dress)



T <- xtabs(~ centre + dress, data = survey)
dotchart(T)

library(lattice)
barchart(T, auto.key = TRUE, stack = FALSE)

xtabs(~ birth_month, survey) |> 
  sort() |> barchart(horizontal = TRUE)

stripchart(survey$height, method = "stack")

stripchart(height ~ dress, data = survey, method = "stack")

stripchart(height ~ centre, data = survey, method = "stack")
