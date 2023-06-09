library(rmarkdown)
library(knitr)

# Render RMDs to .html and .md and Convert RMD to R

# Data preparations

render(input = "code/preparations/1_Extract_parties_voted_in_ESS.Rmd",
       envir = new.env())

purl(input="code/preparations/1_Extract_parties_voted_in_ESS.Rmd",
     output="code/preparations/1_Extract_parties_voted_in_ESS.R",
     documentation = 2)


render(input = "code/preparations/2_Assign_ESS_party_numbers_to_CHES.Rmd",
       envir = new.env())

purl(input="code/preparations/2_Assign_ESS_party_numbers_to_CHES.Rmd",
     output="code/preparations/2_Assign_ESS_party_numbers_to_CHES.R",
     documentation = 2)



render(input = "code/preparations/3_merge_ESS_and_CHES_by_vote.Rmd",
       envir = new.env())

purl(input="code/preparations/3_merge_ESS_and_CHES_by_vote.Rmd",
     output="code/preparations/3_merge_ESS_and_CHES_by_vote.R",
     documentation = 2)


render(input = "code/preparations/4_Variable_transformations.Rmd",
       envir = new.env())

purl(input="code/preparations/4_Variable_transformations.Rmd",
     output="code/preparations/4_Variable_transformations.R",
     documentation = 2)


# Analysis

render(input = "code/analysis/RQ1_RQ2.Rmd",
       envir = new.env())

purl(input="code/analysis/RQ1_RQ2.Rmd",
     output="code/analysis/RQ1_RQ2.R",
     documentation = 2)

render(input = "code/analysis/ERQ1.Rmd",
       envir = new.env())

purl(input="code/analysis/ERQ1.Rmd",
     output="code/analysis/ERQ1.R",
     documentation = 2)

render(input = "code/analysis/Descriptive_statistics.Rmd",
       envir = new.env())

purl(input="code/analysis/Descriptive_statistics.Rmd",
     output="code/analysis/Descriptive_statistics.R",
     documentation = 2)


render(input = "code/analysis/main_effects_and_moderations_by_gndr_minority_west.Rmd",
       envir = new.env())


# Exploratory analyses requested by reviewer

render(input = "code/analysis/Exploratory_left_right_self_placement.Rmd",
       envir = new.env())

purl(input="code/analysis/Exploratory_left_right_self_placement.Rmd",
     output="code/analysis/Exploratory_left_right_self_placement.R",
     documentation = 2)


render(input = "code/analysis/Exploratory_reduce_income_differences.Rmd",
       envir = new.env())

purl(input="code/analysis/Exploratory_reduce_income_differences.Rmd",
     output="code/analysis/Exploratory_reduce_income_differences.R",
     documentation = 2)


render(input = "code/analysis/Exploratory_gay_rights.Rmd",
       envir = new.env())

purl(input="code/analysis/Exploratory_gay_rights.Rmd",
     output="code/analysis/Exploratory_gay_rights.R",
     documentation = 2)

render(input = "code/analysis/Exploratory_immigration.Rmd",
       envir = new.env())

purl(input="code/analysis/Exploratory_immigration.Rmd",
     output="code/analysis/Exploratory_immigration.R",
     documentation = 2)



render(input = "code/analysis/Exploratory_shared_traditions.Rmd",
       envir = new.env())

purl(input="code/analysis/Exploratory_shared_traditions.Rmd",
     output="code/analysis/Exploratory_shared_traditions.R",
     documentation = 2)


render(input = "code/analysis/Exploratory_religiousness.Rmd",
       envir = new.env())

purl(input="code/analysis/Exploratory_religiousness.Rmd",
     output="code/analysis/Exploratory_religiousness.R",
     documentation = 2)