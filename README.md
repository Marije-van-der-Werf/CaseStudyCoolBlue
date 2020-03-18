# Case Study Coolblue

This project is made to give Coolblue useful insights in which marketing strategies of them are profitable. We gave this insights with use of a dynamic sales model, the error correction model. The variables we used are selected by overlapping group lasso. This is only the R-code which led to the results to interpret, there is also an article written. 

## Data
We have gotten data from Coolblue. Besides this, we also downloaded some weather data from the KNMI (The Royal Dutch Meteorological Institute). 

## Code structure
First we need to read in the data and make the dummies. Then all the variables are combined and interaction effects between them are made. Then we perform overlapping group lasso to select a subset of all of these variables. This subset is used in the error correction model. Finally, the elasticities are computed which are then interpreted.
When you want the results, you can just run `short_term.R` or `long_term.R`, these scripts are made in such a way that they call the previous scripts needed in the right order. Below there is a short description what all these scripts do. 

1. `read_in_data.R`: Script to read in the Coolblue data
1. `dummies.R`: Script for making dummies
1. `create_modeldata.R`: Script to create the data used in the model
1. `interaction_effects.R`: Script in which interaction effects are made and variables are standardized 
1. `lasso_corrected.R`: Script where a new lasso function is written
1. `overlapping_group_lasso.R`: Script for overlapping group lasso
1. `ECM_model.R`: Script for ECM with variables coming from lasso
1. `compute_stdev.R`: Script for computation of the standard deviations that are needed
1. `short_term.R`: Script for short term elasticities and interpretation
1. `long_term.R`: Script for long term elasticities and interpretation

## The Netherlands and Belgium
We split the data into two parts, one for the Netherlands and one for Belgium. Because some small things differ between the two countries, we also needed to double the scripts and make some small distinctions. When there is no addition in the name of the script, the script is made for the Netherlands. When the addition `_BE` is added (for example, `short_term_BE.R`), the script works for Belgium. The first two scripts (`read_in_data.R` and `dummies.R`) are the same for the two countries.