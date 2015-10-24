#Michael Chirico
#MichaelChirico4@gmail.com
#October 23, 2015

setwd("~/Downloads/")
library(data.table)
#Install via devtools:install_github("MichaelChirico/funchir")
library(funchir)

#Some data cleaning from the Penn Climb Down
penn_data <-
  setnames(fread("~/Downloads/Penn Pull Down.csv",
                 colClasses = abbr_to_colClass("cinn", "7193")
                 #Delete blank first column
                 )[ , -1 , with = F],
           #set pretty column names
           c("paid", "school", "division_1", "gender", "name",
             "division", "overall_ranking", "total_score",
             "climb_" %+% 1:11))

#Cleaning school names
mapping <- 
  data.table(abbr = c("DU", "PENN", "PU", "RutU", 
                      "RUtU", "TU", "UD", "UDEL", "YORK"),
             nm = c("Drexel", "UPenn", "UPenn",
                    "Rutgers", "Rutgers", "Temple",
                    "UDel", "UDel", "York"))
penn_data[mapping, school := i.nm,
          on = c(school = "abbr")]

#Ordering division_1 as Advanced, Intermediate, Beginner
penn_data[ , division_1 := factor(division_1, 
                                  levels = c("A", "I", "B"))]

#Making sure calculated scores match the specified rule:
#  sum of five highest-score climbs for each climber
penn_data[ , total_score_check := apply(penn_data, 1L, function(y){
  sum(sort(as.numeric(y[("climb_" %+% 1:11)]),
           decreasing = TRUE)[1:5])})]
##Check, then delete check column
penn_data[ , all.equal(total_score,total_score_check)]
penn_data[ , total_score_check := NULL]

#Making sure calculated ranks match the scores
penn_data[ , overall_ranking_check :=
     frank(-total_score, ties.method = "min")]
#Check
penn_data[ , all.equal(overall_ranking, overall_ranking_check)]
#Cameron Fopenn_data and Kristaps Kancan were tied (at 2365) but
#  have different rankings; was there a tiebreaking rule?

penn_data[ , overall_ranking_check := NULL]

#Some output
##School rankings by division & gender
penn_data[ , sum(total_score), by=.(division_1, gender, school)
           ][order(division_1, gender, -V1)]
##School rankings by division
penn_data[ , sum(total_score), by=.(division_1, school)
           ][order(division_1, -V1)]

write.csv(penn_data[order(overall_ranking)],
          "penn_climbdown.csv", row.names = FALSE)