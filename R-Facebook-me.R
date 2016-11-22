setwd("/Users/Danny/Documents/R Project/R-Facebook/")
library(Rfacebook)

# Create Oauthentication token and save for further use
myFB_Oauth <- fbOAuth(app_id = "myFBAppID", app_secret = "myFBAppSecret")
save(myFB_Oauth, file = "myFB_Oauth")
load("myFB_Oauth")

# Create user as MySelf
me <- getUsers("me", token = myFB_Oauth)
me$name

# Data of where user give likes to
my_likes <- getLikes(user = "me", token = myFB_Oauth)

# Retrive Public Page information (id = KTC CSR Club)
my_Page <- getPage(195457950893312, myFB_Oauth)

# Retrive Public Page's Post information
my_Post <- getPost(post = my_Page$id[1], myFB_Oauth)

# Retrive Public Post on Public Group (id = KTC CSR Club)
group <- getGroup(195457950893312,token = myFB_Oauth, n = 10)

# Use Regrep to retrive #hashtag
#ossts
group[grepl("#[Oo][Ss]{2}&*[Tt][Ss]", group$message),"message"]
#ktcccfe
group[grepl("#[Kk][Tt][Cc]{3}[Ff][Ee]", group$message),"message"]

# Specific post date range
# Since use UTC time , to specific Asia/Banghkok time +0700 hrs
test <- getGroup(195457950893312,token = myFB_Oauth, 
                 since = "2016-10-27T17:00:00", until = "2016-10-28T17:00:00", n = 200)

post27Oct <- getGroup(195457950893312,token = myFB_Oauth, 
                      since = "2016/10/27", until = "2016/10/28", n = 200)

post28Oct <- getGroup(195457950893312,token = myFB_Oauth, 
                      since = "2016/10/28", until = "2016/10/29", n = 200)

post29Oct <- getGroup(195457950893312,token = myFB_Oauth, 
                      since = "2016/10/29", until = "2016/10/30", n = 500)

post30Oct <- getGroup(195457950893312,token = myFB_Oauth, 
                      since = "2016/10/30", until = "2016/10/31", n = 500)

post31Oct <- getGroup(195457950893312,token = myFB_Oauth, 
                      since = "2016/10/31", until = "2016/11/01", n = 1000)

save(post28Oct, post29Oct, post30Oct, post31Oct, file = "FBPost28-31Oct.RData")

post1Nov <- getGroup(195457950893312,token = myFB_Oauth, 
                      since = "2016/11/01", until = "2016/11/02", n = 1500)

post2Nov <- getGroup(195457950893312,token = myFB_Oauth, 
                     since = "2016/11/02", until = "2016/11/03", n = 3000)

post3Nov <- getGroup(195457950893312,token = myFB_Oauth, 
                     since = "2016/11/03", until = "2016/11/04", n = 5000)

post4Nov <- getGroup(195457950893312,token = myFB_Oauth, 
                     since = "2016/11/04", until = "2016/11/05", n = 5000)

post5Nov <- getGroup(195457950893312,token = myFB_Oauth, 
                     since = "2016/11/05", until = "2016/11/06", n = 5000)

post6Nov <- getGroup(195457950893312,token = myFB_Oauth, 
                     since = "2016/11/06", until = "2016/11/07", n = 5000)

post7Nov <- getGroup(195457950893312,token = myFB_Oauth, 
                     since = "2016/11/07", until = "2016/11/08", n = 5000)

load("FBPost28-31Oct.RData")

library(dplyr)

post28Oct_7Nov <- bind_rows(post28Oct, post29Oct, post30Oct, post31Oct, post1Nov,
                            post2Nov, post3Nov, post4Nov, post5Nov, post6Nov,
                            post7Nov)
save(post27Oct_7Nov, file = "FBPost27Oct_7Nov")
load("FBPost27Oct_7Nov")

# Count hashtag #ossts 28Oct-7Nov
sum(grepl("#[Oo][Ss]{2}&*[Tt][Ss]", post27Oct_7Nov$message))

post27Oct_7Nov$post.date.time <- as.POSIXct(substr(post27Oct_7Nov$created_time, 1, 19), 
                                      format = "%Y-%m-%dT%H:%M:%S", tz = "GMT")

# change time zone from GMT to Asia/Bangkok
attr(post27Oct_7Nov$post.date.time, "tzone") <- "Asia/Bangkok"

sum(as.Date(post27Oct_7Nov$post.date.time) == as.Date("2016-11-07"))

sum(grepl("#[Oo][Ss]{2}&*[Tt][Ss]", post27Oct_7Nov$message))


library(dplyr)

post27Oct_7Nov %>%
  filter(post27Oct_7Nov$post.date.time <= "2016-11-07 18:00:00") %>%
  group_by(as.Date(post.date.time, tz = "Asia/Bangkok")) %>%
  summarise(post = n(), 
            csrtag = sum(grepl("#[Kk][Tt][Cc]89000ความ+", message)),
            osststag = sum(grepl("#[Oo][Ss]{2}&*[Tt][Ss]", message)),
            osswithcsr = sum(grepl("#[Kk][Tt][Cc]89000ความ+", message) & grepl("#[Oo][Ss]{2}&*[Tt][Ss]", message)))
            