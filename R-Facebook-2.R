# Data loading from Facebook ----
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
# Fixed part
p4 <- getGroup(195457950893312,token = myFB_Oauth, 
               since = "2016/11/06T00:00:01", until = "2016/11/09T23:59:59", n = 50000)

p5 <- getGroup(195457950893312,token = myFB_Oauth, 
               since = "2016/11/10T00:00:01", until = "2016/11/10T23:59:59", n = 50000)

p6 <- getGroup(195457950893312,token = myFB_Oauth, 
               since = "2016/11/12T00:00:01", until = "2016/11/15T23:59:59", n = 50000)

p7 <- getGroup(195457950893312,token = myFB_Oauth, 
               since = "2016/11/13T00:00:01", until = "2016/11/14T23:59:59", n = 50000)

p8 <- getGroup(195457950893312,token = myFB_Oauth, 
               since = "2016/11/12T00:00:01", until = "2016/11/12T23:59:59", n = 50000)

p9 <- getGroup(195457950893312,token = myFB_Oauth, 
               since = "2016/11/15T00:00:01", until = "2016/11/15T23:59:59", n = 50000)

p10 <- getGroup(195457950893312,token = myFB_Oauth, 
               since = "2016/11/20T00:00:01", until = "2016/11/20T23:59:59", n = 50000)

post.fix <- bind_rows(p4, p5, p6)
save(post.fix, file = "FBPostFix.RData")
load(file = "FBPostFix.RData")

post.fix <- post.fix[!(as.Date(post.fix$post.date.time) %in%
                        c(as.Date("2016-11-12"), as.Date("2016-11-15"), as.Date("2016-11-20"))),  ]

post.fix <- bind_rows(post.fix, p8, p9, p10)
save(post.fix, file = "FBPostFix.RData")
load(file = "FBPostFix.RData")

# Convert charactor to POSIXct
post.fix$post.date.time <- as.POSIXct(post.fix$created_time, 
                                      format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")

# Convert time zone from UTC to Asia/Bangkok, also convert to charactor again
post.fix$post.date.time <- format(post.fix$post.date.time, tz = "Asia/Bangkok")

# Convert charactor back to POSIXct and define timezone
# R show UTC+7.00 as ICT = Indo China Time
post.fix$post.date.time <- as.POSIXct(post.fix$post.date.time, tz = "Asia/Bangkok")

# Combine data and save ----

library(dplyr)
post <- bind_rows(p1, p2, p3)
save(post, file = "FBPost28Oct_4Dec.RData")
load(file = "FBPost28Oct_4Dec.RData")

# Convert charactor to POSIXct 
post$post.date.time <- as.POSIXct(post$created_time,
                                  format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
# Convert time zone from UTC to Aisa/Bangkok, also convert to charactor again
post$post.date.time <- format(post$post.date.time, tz = "Asia/Bangkok")

# Convert charactor back to POSIXct again
post$post.date.time <- as.POSIXct(post$post.date.time, tz = "Asia/Bangkok")

# recheck time zone from attribue
attr(post$post.date.time, "tzone")

# Remove error time period 
library(dplyr)
d <- post.fix %>%
  group_by(as.Date(post.date.time)) %>%
  summarise(n=n())

post.trim <- post[!(as.Date(post$post.date.time) %in% 
                      c(seq.Date(as.Date("2016-11-06"), as.Date("2016-11-10"), by = 'day'), 
                        as.Date("2016-11-12"), as.Date("2016-11-15"))),  ]

post.new <- bind_rows(post.trim, post.fix)
save(post.new, file = "FBPostEdit.RData")
load(file = "FBPostEdit.RData")

# Count hashtag #oss&ts
sum(grepl("#[Oo][Ss]{2}&*[Tt][Ss]", post.new$message))

# Summarized by #hash tag
library(ggplot2)

d <- post.new %>%
  mutate(post.date = as.Date(post.date.time)) %>%
  group_by(post.date) %>%
  summarise(post = n(), 
            csrtag = sum(grepl("#[Kk][Tt][Cc]89000ความ+", message)),
            osststag = sum(grepl("#[Oo][Ss]{2}&*[Tt][Ss]", message)),
            osswithcsr = sum(grepl("#[Kk][Tt][Cc]89000ความ+", message) & grepl("#[Oo][Ss]{2}&*[Tt][Ss]", message)))

ggplot(data = d, aes(x = post.date, y = post)) + 
  geom_bar(stat = "identity") + 
  scale_x_date(date_labels = "%d-%b", date_minor_breaks = "1 day")

# Explanatory data analysis ----
# use tidyr to gather data from wide format to long format for plotting

library(tidyr)
d2 <- gather(d, "group", "n", 2:5)

# mulitple bar plot side by side with command position = 'dodge"
g2 <- ggplot(data = d2, aes(x = post.date, y = n)) + 
  geom_bar(stat = "identity", aes(fill = group, color = group), position = "dodge") + 
  scale_x_date(date_labels = "%d-%b", date_minor_breaks = "1 day")

g2

# apply plotly for interactive graph
library(plotly)

# combine ggplot2 + plotly use ggplotly
ggplotly(g2)

# use plotly command to plot bar graph (more meanningful than ggplot)
plot_ly(data = d2, type = "bar", x = d2$post.date, y = d2$n, color = d2$group)


# Data Analysis ----

load(file = "FBPostEdit.RData")
# keep bacward compatability with old code
post <- post.new
rm(post.new)

# Data cleaning
# Find highest poster
count.id.post <- aggregate(post["id"], by = post["from_id"], FUN = length)
count.id.post <- count.id.post[order(count.id.post$id, decreasing = TRUE), ]
View(count.id.post)
summary(count.id.post)

library(plotly)

plot_ly(count.id.post, x = ~count.id.post$id, type = "histogram",
        autobinx = FALSE, xbins = list(start = 0, end = 5 , size = 1))
# Plotly complex parameter use list data type

# trim outliner - 5% top & last ----
# n.trim <- round(0.05 * nrow(count.id.post))
# rownames(count.id.post) <- NULL  # Reset rownames index
# form_id.trim exclude 1:n.trim of head and tails
# from_id.trim <- count.id.post[n.trim:(nrow(count.id.post)-n.trim),"from_id"]
# Check histogram plot of trimmed data
# with(count.id.post[(count.id.post$from_id %in% from_id.trim), ], hist(id))
# filter post trim from post where from_id in from_id.trim
# post.trim <- post[(post$from_id %in% from_id.trim), ]

# Analysis # Like in first post effect on # post ----

load(file = "FBPostEdit.RData")

# Transform data to list
post.list <- split(post[, c("from_id", "type", "likes_count",
                            "comments_count", "shares_count", 
                            "post.date.time")], post$from_id)
# sort data.frame in list by post.date.time
post.list <- lapply(post.list, 
                    FUN = function(df) {
                      df[order(df$post.date.time), ]
                      })

# No. of liked in first post
firstPost.liked <- lapply(post.list,
                          FUN = function(df) {
                            df[1,"likes_count"]
                            })

firstPost.commnet <- lapply(post.list,
                          FUN = function(df) {
                            df[1,"comments_count"]
                          })

# No. of post after first post
noPost.afterFirst <- lapply(post.list, 
                            FUN = function(df) {nrow(df[-1, ])
                            })
df <- cbind(as.data.frame(unlist(firstPost.liked)),
            as.data.frame(unlist(firstPost.commnet)),
            as.data.frame(unlist(noPost.afterFirst)))
colnames(df) <- c("firstPostLiked", "firstPostCommnet", "noPostAfterfirst")

summary(df)
plot(x = df$firstPostLiked, y = df$noPostAfterfirst
     , ylim = c(0, 1000), xlim = c(0, 80))
# from EDA show no relation between no.first post liked <-> no post after first

plot(x = df$firstPostCommnet, y = df$noPostAfterfirst)
# from basic EDA shwo no relation comment <-> no post after first post