# Projects & Associated Files

 - ASPES: R implementation of practice exercise demonstrating [Harvill and Moulton's Analysis of Symmetrically Predicted Endogenous Subgroups (ASPES)](https://ies.ed.gov/funding/grantsearch/details.asp?ID=1645)
   + `ASPES/`
 - All-Time Home Run Leaders, by Season: Using `Lahman` data, calculate who the top 10 home run hitters were in every year, and the trajectories of each member to their current total.
   + `home_runs/home_run_leaders.R` - GIF version
   + `home_runs/app.R` - `shiny` version, deployed [here](https://michaelchirico.shinyapps.io/home_runs/)
 - Relative Frequency of the 676 first/last name initial pairs, as pulled from the 80 million names in the [Social Security Death Index Master File](http://ssdmf.info/).
   + `social_security_names/social_security_initials.R` - Extract from raw data to initial-by-year and initial-level aggregated counts
   + `social_security_names/app.r` - `shiny` version, deployed [here](https://michaelchirico.shinyapps.io/social_security_names/)
   + `social_security_names/social_security_tabulated.csv` - Aggregated data, counts by initial pair
   + `social_security_names/social_security_tabulated_by_year.csv` - Aggregated data, counts by initial pair by year
