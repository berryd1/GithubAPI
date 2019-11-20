#install.packages("jsonlite")
library(jsonlite)
#install.packages("httpuv")
library(httpuv)
#install.packages("httr")
library(httr)
install.packages("plotly")
library(plotly)
#install.packages("devtools")
require(devtools)


# Can be github, linkedin etc depending on application
oauth_endpoints("github")

# Change based on what you 
myapp <- oauth_app(appname = "Access_Github",
                   key = "c33dd79b6c982f9ca892",
                   secret = "6fb96d15153640677e3677da5d208a23cfd899f1")

# Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

# Use API
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/users/jtleek/repos", gtoken)

# Take action on http error
stop_for_status(req)

# Extract content from a request
json1 = content(req)

# Convert to a data.frame
gitDF = jsonlite::fromJSON(jsonlite::toJSON(json1))

# Subset data.frame
gitDF[gitDF$full_name == "jtleek/datasharing", "created_at"] 

#Interrogate the Github API to extract data from my own github account and summarise

#Gets my data 
myData = fromJSON("https://api.github.com/users/berryd1")

#Displays number of followers
myData$followers

followers = fromJSON("https://api.github.com/users/berryd1/followers")
followers$login #Gives user names of all my followers

myData$following #Displays the number of people I am following

following = fromJSON("https://api.github.com/users/berryd1/following")
following$login #Gives the names of all the people I am following

myData$public_repos #Displays the number of repositories I have

repos = fromJSON("https://api.github.com/users/berryd1/repos")
repos$name #Details of the names of my public repositories
repos$created_at #Gives details of the date the repositories were created 
repos$full_name #gives names of repositories

#Used account of Sebastien Eustace to produce plots, one of the most popular developers on Github.
#Used instead of my account as his account would produce more accurate results.
#He is also the most influential user that I am following.
#Big sample size produces better results.
#Username is sdispater.

#Began to interrogate Sebastien Eustace's account to produce graphs, by first looking at his followers.
myData = GET("https://api.github.com/users/sdispater/followers?per_page=100;", gtoken)
stop_for_status(myData)
extract = content(myData)
#Converts into dataframe
githubDB = jsonlite::fromJSON(jsonlite::toJSON(extract))
githubDB$login

#Retrieve a list of usernames
id = githubDB$login
user_ids = c(id)

#Create an empty vector and data.frame
users = c()
usersDB = data.frame(
  username = integer(),
  following = integer(),
  followers = integer(),
  repos = integer(),
  dateCreated = integer()
)

#Loops through users and adds to list
for(i in 1:length(user_ids))
{
  
  followingURL = paste("https://api.github.com/users/", user_ids[i], "/following", sep = "")
  followingRequest = GET(followingURL, gtoken)
  followingContent = content(followingRequest)
  
  #Does not add users if they have no followers
  if(length(followingContent) == 0)
  {
    next
  }
  
  followingDF = jsonlite::fromJSON(jsonlite::toJSON(followingContent))
  followingLogin = followingDF$login
  
  #Loop through 'following' users
  for (j in 1:length(followingLogin))
  {
    #Check for duplicate users
    if (is.element(followingLogin[j], users) == FALSE)
    {
      #Adds user to the current list
      users[length(users) + 1] = followingLogin[j]
      
      #Obtain information from each user
      followingUrl2 = paste("https://api.github.com/users/", followingLogin[j], sep = "")
      following2 = GET(followingUrl2, gtoken)
      followingContent2 = content(following2)
      followingDF2 = jsonlite::fromJSON(jsonlite::toJSON(followingContent2))
      
      #Retrieves who user is following
      followingNumber = followingDF2$following
      
      #Retrieves users followers
      followersNumber = followingDF2$followers
      
      #Retrieves how many repository the user has 
      reposNumber = followingDF2$public_repos
      
      #Retrieve year which each user joined Github
      yearCreated = substr(followingDF2$created_at, start = 1, stop = 4)
      
      #Add users data to a new row in dataframe
      usersDB[nrow(usersDB) + 1, ] = c(followingLogin[j], followingNumber, followersNumber, reposNumber, yearCreated)
      
    }
    next
  }
  #Stop when there are more than 150 users
  if(length(users) > 150)
  {
    break
  }
  next
}

#Created link to plotly which creates online interactive graphs.
Sys.setenv("plotly_username"="berryd1")
Sys.setenv("plotly_api_key"="XyyQOXDnbEyvvNTJPWzz")

#Plot one graphs repositories vs followers by year.
#Takes into account 150 of Sebastien Eustace's followers.
#The data is represented by a scatter plot.
#X-axis displays 'repositories' which shows the no. of repositories per user.
#Y-axis displays 'followers' which shows the no. of followers of each each of Sebastien Eustace's followers.
plot1 = plot_ly(data = usersDB, x = ~repos, y = ~followers, text = ~paste("Followers: ", followers, "<br>Repositories: ", repos, "<br>Date Created:", dateCreated), color = ~dateCreated)
plot1
#Plot can be viewed on plotly for more interactive visualisation of the data: https://plot.ly/~berryd1/1/#/

