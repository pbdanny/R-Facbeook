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
# Find public page id for analysis
my_likes <- getLikes(user = "me", token = myFB_Oauth)

# Retrive Post by page admin (Use page id from getLike above)
ktcCSR.page <- getPage(195457950893312, myFB_Oauth)

# Retrive each Page Admin's Post information (use post id from getPage)
ktcCSR.post <- getPost(post = ktcCSR.page$id[1], myFB_Oauth)

# Retrive visitor post (use page id from getLike)
ktcCSR.visitor.post <- getGroup(195457950893312,token = myFB_Oauth, n = 10)

# Retive visitor post with specific post date range
# Since use UTC time , -0700 hrs add to Asia/Banghkok time 
# Total project period since = "2016-10-28T02:00:00", until = "2016-12-04T04:00:00"
# Splitting data retrival 3 priods
# 28 Oct T02:00:00 - 18 Nov T06:00:00
# 18 Nov T06:00:01 - 30 Nov T06:00:00
# 30 Nov T06:00:01 - 04 Dec T04:00:00

p1 <- getGroup(195457950893312,token = myFB_Oauth, 
                 since = "2016-10-28T02:00:00", until = "2016-11-18T06:00:00", n = 50000)

p2 <- getGroup(195457950893312,token = myFB_Oauth, 
                      since = "2016/11/18T06:00:01", until = "2016/11/30T06:00:00", n = 50000)

p3 <- getGroup(195457950893312,token = myFB_Oauth, 
               since = "2016/11/30T06:00:01", until = "2016/12/04T04:00:00", n = 50000)
# Combine data and save 

library(dplyr)
post <- bind_rows(p1, p2, p3)
save(post, file = "FBPost28Oct_4Dec.RData")
load(file = "FBPost28Oct_4Dec.RData")

# Create R date-time data
post$post.date.time <- as.POSIXct(substr(post$created_time, 1, 19), 
                                  format = "%Y-%m-%dT%H:%M:%S", tz = "GMT")
# change time zone from GMT to Asia/Bangkok
attr(post$post.date.time, "tzone") <- "Asia/Bangkok"

# Count hashtag #oss&ts
sum(grepl("#[Oo][Ss]{2}&*[Tt][Ss]", post$message))

# Summarized by #hash tag
d <- post %>%
  mutate(post.date = as.Date(post.date.time, tz = "Asia/Bangkok")) %>%
  group_by(post.date) %>%
  summarise(post = n(), 
            csrtag = sum(grepl("#[Kk][Tt][Cc]89000ความ+", message)),
            osststag = sum(grepl("#[Oo][Ss]{2}&*[Tt][Ss]", message)),
            osswithcsr = sum(grepl("#[Kk][Tt][Cc]89000ความ+", message) & grepl("#[Oo][Ss]{2}&*[Tt][Ss]", message)))

library(ggplot2)

ggplot(data = d, aes(x = post.date, y = post)) + 
  geom_bar(stat = "identity") + 
  scale_x_date(date_labels = "%d-%b", date_minor_breaks = "1 day")

