# Contents

bin_ratings.RData =  binarized data matrix that contains the names of our items in the column names

IBCF_model.RData = the item based collaborative filtering model

restaurant_names.RData = list of lists/ w menu items for every restaruant to populate drop down menus

app.R = shinyapp

# Instructions

Make sure all .RData files are in working directory, open "app.R" and run the entire file. The shiny app should open locally.

To publish this app on shinapps.io:

1. install rsconnect: install.packages("rsconnect")

2. authorize account: rsconnect::setAccountInfo(name = "tastespace", token = "xxxx", secret = "xxxx")

3. rsconnect::deployApp('path/to/app', account = "tastespace")