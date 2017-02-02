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

# Analysis # Like in first post (1st-post-#liked) effect on # post ----

load(file = "FBPostEdit.RData")

# Transform data to list
post.list <- split(post.new[, c("from_id", "type", "likes_count",
                            "comments_count", "shares_count", 
                            "post.date.time")], post.new$from_id)

# sort data.frame in list by post.date.time
post.list <- lapply(post.list, 
                    FUN = function(df) {
                      df[order(df$post.date.time), ]
                      })

# No. 1st-post-#liked (row 1 of sorted data)
firstPost.liked <- lapply(post.list,
                          FUN = function(df) {
                            df[1,"likes_count"]
                            })

# No. of comment in first post (row 1 of sorted data)
firstPost.comment <- lapply(post.list,
                          FUN = function(df) {
                            df[1,"comments_count"]
                          })

# No. of post after first post (number of row except the first row)
noPost.afterFirst <- lapply(post.list, 
                            FUN = function(df) {
                              nrow(df[-1, ])
                            })

# Combine all list to dataframe & named
df <- cbind(as.data.frame(unlist(firstPost.liked)),
            as.data.frame(unlist(firstPost.comment)),
            as.data.frame(unlist(noPost.afterFirst)))
colnames(df) <- c("firstPostLiked", "firstPostCommnet", "noPostAfterfirst")

# Cut data by 1st-post-#liked
df$like.range <- cut(df$firstPostLiked, 
                  c(0, 1, 10, 20, 30, 150), right = FALSE) # not include right-side range
summary(df)

# Boxplot by range of 1st-post-#liked

library(ggplot2)
ggplot(data = df, aes(x = like.range, y = noPostAfterfirst)) + 
  geom_boxplot(aes(color = like.range)) +
  coord_cartesian(y = c(0, 250))

# from EDA show some relation between 1st-post-#liked <-> # of post after first

# t.test group [0,1) vs. [1,10) ; set var.equal = FALSE for expected diff varience of each group
t.test(subset(df, like.range == "[0,1)", select = noPostAfterfirst), 
       subset(df, like.range == "[1,10)", select = noPostAfterfirst),
       paired = FALSE, var.equal = FALSE)  # var.equal = FALSE
# Can not reject mean 1st-post-#liked of [0,1) = of [1,10)

# t.test group [0,1) vs. [10,20) ; set var.equal = FALSE for expected diff varience of each group
t.test(subset(df, like.range == "[0,1)", select = noPostAfterfirst), 
       subset(df, like.range == "[10,20)", select = noPostAfterfirst),
       paired = FALSE, var.equal = FALSE)  # var.equal = FALSE

# Can reject mean 1st-post-#liked of [0,1) = of [10,20)
# Confident interval in -49 .. -4 = mean of [0,1) smaller than of [10,20)

# Try clustering data ----
clust <- post.new[, c("from_id", "likes_count", "comments_count", "post.date.time")]

# Convert POSIXlt to 24hr
clust$hr <- as.integer(format(clust$post.date.time, "%H"))
clust$post.date.time <- NULL

# Filter outliner like > 100 out
clust <- clust[clust$likes_count <= 100, ]

# Scale varible (normal curve scales)
clust$likes_count.scales <- scale(clust$likes_count)
clust$hr.scales <- scale(clust$hr)

# Check scaled data mean ~ 0, sd ~ 1
colMeans(clust$likes_count.scales)
sd(clust$likes_count.scales)
colMeans(clust$hr.scales)
sd(clust$hr.scales)

# Run kmeans clustering on scales data with center (k) = 5
kclust <- kmeans(clust[,c(5,6)], centers = 5)

# Plot data points by clusetered and 
plot(x = clust$likes_count, y = clust$hr, col = kclust$cluster)
points(kclust$centers, pch = 2)

# Features engineering ----

load(file = "FBPostEdit.RData")

library(dplyr)
library(plotly)
library(tidyr)

d <- post.new %>%
  mutate(post.date = as.Date(post.date.time)) %>%
  group_by(post.date) %>%
  summarise(n = n())

plot_ly(d, type = 'bar', x = d$post.date, y = d$n)
rm("d")

s <- post.new %>%
  select(from_id, message, likes_count) %>%
  filter(!is.na(message)) %>%
  mutate(post.len = nchar(message)) %>%
  group_by(from_id) %>%
  summarise(n = n(), mean.post.len = mean(post.len), mean.like.count = mean(likes_count))

# Create period of time bin
# 0-6am = 1, 7-12 = 2, 13-18 = 3, 19-24 = 4
t.bin <- post.new %>%
  select(from_id, post.date.time) %>%
  mutate(post.time = as.integer(format(post.date.time, "%H"))) %>%
  mutate(time.bin = findInterval(post.time, seq(0, 24, 6))) %>%
  group_by(from_id, time.bin) %>%
  summarise(n = n())
         
t.bin.df <- t.bin %>%
  mutate(time.bin = paste("t", time.bin, sep = "_")) %>%
  spread(time.bin, n, fill = 0)

# Create no of post from start - end project
d.seq <- post.new %>%
  select(from_id, post.date.time) %>%
  mutate(post.date = as.Date(post.date.time)) %>%
  mutate(post.date.seq = sprintf("%02d", difftime(post.date, as.Date("2016-10-27") , units = "days"))) %>%
  group_by(from_id, post.date.seq) %>%
  summarise(n = n())

d.seq.df <- d.seq %>%
  mutate(post.date.seq = paste("d", post.date.seq, sep = "_")) %>%
  spread(post.date.seq, n, fill = 0)

# Combine all 3 table use from_id as key

df <- s %>%
  left_join(t.bin.df, by = "from_id") %>%
  left_join(d.seq.df, by = "from_id")

# Remove temp table
rm(list = c("s", "d.seq", "d.seq.df", "t.bin", "t.bin.df"))
save(df, file = "FBCluster.RData")
load(file = "FBCluster.RData")

# Partitioning Clustering ----
load(file = "FBCluster.RData")

# K - Means ----
# Scale data
df.scale <- data.frame(scale(df[c(-1875, -772),-1]))

# Determine number of clusters
# use total within-cluster sum square
# Select by elbow graph
# add nstart > 1 for 10 times random start points for clustering
wss <- 0
for (i in 1:15) wss[i] <- kmeans(df.scale, centers = i, nstart = 10)$tot.withinss
plot(1:15, wss, type = "b", xlab = "Number of Clusters",
     ylab = "Within-cluster sum of squares")

# Result show 8 clusters yield best clustering
k.means.fit <- kmeans(df.scale, centers = 8, nstart = 10)

# Plot clustering results
library(cluster)
clusplot(df.scale, k.means.fit$cluster, 
         color = TRUE, shade = TRUE, labels = 2)
# with clusplot labels = 1, use point & click to identify the outliner
# row 1875, 772 then remove from data and re clustering
# any way the partition of kmeans still overlapping, segmentation not yield
# heterogeneity between each cluster

# Recheck clusterting result with PCA
df.pca <- prcomp(df.scale)
plot(x = df.pca$x[ ,1], y = df.pca$x[, 2])

# PAM (Patition Around Medoids) clustering ----
# find optimum k of cluster find maximum width in silluate data
sil.width <- 0
for(i in 2:15) sil.width[i] <- pam(scale(df[, -1]), i)$silinfo$avg.width

k <- which.max(sil.width)  # Find cluster no. that maximized avg width between cluster

df.pam <- pam(scale(df[, -1]), k)

df.pam$medoids  # Show point selected as medoids
df.pam$clustering  # Cluster assigned
table(df.pam$clustering)  # No. of obs in each cluster

library(cluster)
clusplot(df.pam)
# silhouette plot show obs. in each cluster & avg Sihouette witdh
# minus silhouette width = missed cluster
# use parameter border = NA to solve color not show
sil <- silhouette(df.pam)
plot(sil, col = 2:3, border = NA)
# From plot result the clustering not so clear
# The silhouette width of each cluster have minus value (close to adjacent cluster)

# Identify the minus silhouette width
minus.sil.idx <- which(sil[,'sil_width'] < 0)
sil[minus.sil.idx, ]

# Trace back form minus silhouette to data points
View(df[as.integer(row.names(sil[minus.sil.idx, ])), ])

# CLARA ----
# Sample data, use PAM algorithm to assigned medoids 
# Use the best (least dissimilarity) cluster result
df.clara <- clara(df.scale, k = 2, samples = 100)
plot(df.clara) # plot both clusplot & silhouette

# Hierarchical clustering ----
load(file = "FBCluster.RData")

# Basic Hierarchical hclust (agglomerative)
dist <- dist(scale(df[, -1]), method = "euclidean")
clust <- hclust(dist, method = "ward.D2")
plot(clust)

# Agnes (agglomerative)
clust.agnes <- agnes(df[, -1], metric = "euclidean", stand = TRUE, method = "ward")
plot(clust.agnes)

# Diana (divisive)
clust.diana <- diana(df[, -1], metric = 'euclidean', stand = TRUE)
plot(clust.diana)

# Use correlation based distance measure
dist <- cor(t(df[, -1]), method = "pearson")  # Transpose to compute Corr by pairwised obs
clust <- hclust(as.dist(1 - dist), method = "ward.D2")
plot(clust)

# Use cluster = 3 group
group <- cutree(clust, k = 3)

# Plot dentogram with cutree
rect.hclust(clust, k = 3 , border = 'red')

# Map back group to data
df.hclust <- df
df.hclust$clust <- group

# Exploratory each cluster key parameter
library(ggplot2)

ggplot(df.hclust, aes(n, fill = factor(clust), color = factor(clust))) +
  geom_density(alpha = 0.2) +
  coord_cartesian(xlim = c(0, 100))

ggplot(df.hclust, aes(mean.post.len, fill = factor(clust), color = factor(clust))) +
  geom_density(alpha = 0.2) +
  coord_cartesian(xlim = c(0, 500))

ggplot(df.hclust, aes(x = n, y = mean.post.len, fill = factor(clust), color = factor(clust))) +
  geom_point(alpha = 0.2) +
  geom_smooth(stat = "smooth") +
  coord_cartesian(xlim = c(0, 500), ylim = c(0, 1000))

ggplot(df.hclust, aes(x = n, y = mean.like.count, fill = factor(clust), color = factor(clust))) +
  geom_point(alpha = 0.2) +
  geom_smooth(stat = "smooth") +
  coord_cartesian(xlim = c(0, 500))
  
# Test cluterability Hopkin stat.

library(clustertend)

hopkins(df[, -1], n = 2000)

# Hopkins stat = 0.0083 (clusterable), means that compare with random distributed data with same
# number of data points if distance of this data are more closed to each others(clusterable)
# then Hopkons -> 0

# group 1 : "Effective poster" moderate poster, tailer content to gain likes
# group 2 : "Just poster" Rarely post, don't care like received
# group 3 : "Power poster" Super post, don't care like received

# Try combination of distance & clustering
# d <- c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")
# c <- c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid")
# 
# for (i in 1:length(d)){
#   for (j in 1:length(c)){
#     plot(hclust(dist(df.scale, method = d[i]), method = c[j]),
#          main = sprintf("Dist. = %s : Clust = %s", d[i], c[j]))
#   }
# }

