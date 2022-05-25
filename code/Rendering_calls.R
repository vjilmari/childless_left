library(rmarkdown)
library(knitr)

# Render RMDs to .html and .md

render(input = "code/analysis/RQ1_RQ2.Rmd",
       envir = new.env())

render(input = "code/analysis/ERQ1.Rmd",
       envir = new.env())

render(input = "code/analysis/Descriptive_statistics.Rmd",
       envir = new.env())

render(input = "code/analysis/main_effects_and_moderations_by_gndr_minority_west.Rmd",
       envir = new.env())

# Convert RMD to R

purl(input="code/analysis/RQ1_RQ2.Rmd",
     output="code/analysis/RQ1_RQ2.R",
     documentation = 2)

purl(input="code/analysis/ERQ1.Rmd",
     output="code/analysis/ERQ1.R",
     documentation = 2)

purl(input="code/analysis/Descriptive_statistics.Rmd",
     output="code/analysis/Descriptive_statistics.R",
     documentation = 2)

