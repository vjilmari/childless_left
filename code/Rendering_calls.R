library(rmarkdown)
library(knitr)

# Render RMDs to .html and .md

render(input = "code/preparations/1_Extract_parties_voted_in_ESS.Rmd",
       envir = new.env())


render(input = "code/preparations/2_Assign_ESS_party_numbers_to_CHES.Rmd",
       envir = new.env())


render(input = "code/preparations/3_merge_ESS_and_CHES_by_vote.Rmd",
       envir = new.env())


render(input = "code/preparations/4_Variable_transformations.Rmd",
       envir = new.env())

# Convert RMD to R

purl(input="code/preparations/1_Extract_parties_voted_in_ESS.Rmd",
     output="code/preparations/1_Extract_parties_voted_in_ESS.R",
     documentation = 2)

purl(input="code/preparations/2_Assign_ESS_party_numbers_to_CHES.Rmd",
     output="code/preparations/2_Assign_ESS_party_numbers_to_CHES.R",
     documentation = 2)

purl(input="code/preparations/3_merge_ESS_and_CHES_by_vote.Rmd",
     output="code/preparations/3_merge_ESS_and_CHES_by_vote.R",
     documentation = 2)

purl(input="code/preparations/4_Variable_transformations.Rmd",
     output="code/preparations/4_Variable_transformations.R",
     documentation = 2)
