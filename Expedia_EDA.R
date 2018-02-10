#-----------------------------------------------#
# Capstone Data Exploration: Expedia            #
#                                               #
# Exploring the Expedia data from DataFest 2017 #
#-----------------------------------------------#

setwd("~/Spring 2018/Capstone")
library(readr)     # reading in data
library(lubridate) # better date functions
library(stringr)   # string manipulation

### Customer Data file

data <- read_delim("Datafest/Data/data.txt", "\t", escape_double = FALSE
                   , col_types = cols(date_time = col_character()
                                      , orig_destination_distance = col_double()
                                      , srch_ci = col_character(), srch_co = col_character()
                                      , user_location_latitude = col_double()
                                      , user_location_longitude = col_double())
                   , na = "NULL", trim_ws = TRUE)

summary(data)

n.records <- nrow(data) # 10,884,539 rows, 27 cols

# Split up the date_time into dates (YYYY-MM-DD), time of day (HH:MM), day of week
srch_date <- date(data$date_time)
summary(srch_date)

srch_day <- day(srch_date)
hist(srch_day, main="Day of Search")
# Most at beginning of month but then maybe biweekly trends? Not sure how to normalize that

srch_month <- month(srch_date)
hist(srch_month, main="Month of Search", col="blue")
# Most searches in summertime with spike in March

srch_wkday <- wday(srch_date, label=F)
hist(srch_wkday, main="Day of Week Search", col="red")
# More frequent at beginning of week, peak on Wednesdays

srch_hour <- hour(data$date_time)
hist(srch_hour, main="Hour of Day Search", col="green")
#(expected) lull from 1-5 AM, then two humps peaking at 10 AM and 5/6 PM


# Flags for site name 
is_dotCom <- ifelse(data$site_name == "EXPEDIA.COM", 1, 0)
hist(is_dotCom)
sum(is_dotCom)/n.records # 0.8379 = 84% expedia.com

## My 9: is_mobile, is_package, channel, srch_ci, srch_co, srch_adults_cnt, srch_children_cnt, srch_rm_cnt, srch_destination_id

# Mobile users
prop.mobile <- sum(data$is_mobile)/n.records # 22.1% mobile
sum(is.na(data$is_mobile)) # No missing values

# Packages
prop.package <- sum(data$is_package)/n.records # 19.65% package
sum(is.na(data$is_package)) # No missing values

# Channels
sort(table(data$channel))
4257365/n.records # channel 541 accounts for 39.11% of records
1673366/n.records # channel 510 accounts for 15.37% of records
1434155/n.records # channel 231 accounts for 13.18% of records
1377754/n.records # channel 293 accounts for 12.66% of records
968520/n.records # channel 262 accounts for 8.90% of records
702659/n.records # channel 324 accounts for 6.46% of records
214910/n.records # channel 386 accounts for 1.97% of records
120388/n.records # channel 355 accounts for 1.11% of records
105516/n.records # channel 448 accounts for 0.97% of records
28252/n.records # channel 479 accounts for 0.26% of records
1654/n.records # channel 417 accounts for 0.02% of records



# Check-In Dates
ci_date <- as_date(data$srch_ci)
summary(ci_date) # first date is 2015-01-01, last is 3015-12-25; 4966 missing dates
nrow(data[which(year(as_date(data$srch_ci)) > 2015),]) # 1,090,499 records aren't in 2015 (10%)
table(year(ci_date)) # 89.9% of records are in 2015 and 10% are in 2016; 443 are in 2017, 2 are in 2020, 2 are in 2025, 2 are in 2070, 2 are in 2915, and 1 is in 3015 

ci_day <- day(ci_date)
summary(ci_day)
hist(ci_day, main="Frequency of Check-Ins by Day of the Month")

ci_wkday <- wday(ci_date, label=FALSE) # Weekday is shown as numbers where 1 = Sunday, 2 = Monday, etc.
summary(ci_wkday)
hist(ci_wkday, main="Frequency of Check-Ins by Day of the Week", col="gray") # Most frequent on Fridays, followed by Saturday and Thursday

ci_month <- month(ci_date)
summary(ci_month)
hist(ci_month, main="Frequency of Check-Ins by Month", col="gray") # potential seasonality based on peak in the summer months, but interesting increase in December that isn't duplicated in Check-Outs

## Almost 5000 records (4961) are missing a check-in date, but some have check out dates and 8 were bookings (not a significant amount tho)
#missing_data <- data[which(is.na(data$srch_ci)),]


# Check-Out Dates
co_date <- as_date(data$srch_co)
summary(co_date) # first date is 1510-10-21 (probs 2015-10-21 with fat fingers) and last date 2070-01-01 (maybe NYD 2017?); 4971 missing values
table(year(co_date)) # most (88.26%) check-outs in 2015, 11.68% in 2016; 1 in 1510, 1 in 2014, 733 in 2017, 2 in 2020, 2 in 2025, and 2 in 2070

co_day <- day(co_date)
summary(co_day) # 4971 records are missing check-out dates
hist(co_day, main="Frequency of Check-Outs by Day of Month") # day 1 is the most common

co_wkday <- wday(co_date, label=FALSE) # Weekday is Sunday = 1, Monday = 2, etc. 
summary(co_wkday)
hist(co_wkday, main="Frequency of Check-Outs by Day of Week", col="gray") # most check outs on Sundays, followed by Monday and Saturday

co_month <- month(co_date)
summary(co_month)
hist(co_month, main="Frequency of Check-Outs by Month", col="gray") # potential seasonality because higher frequency in the summer than winter


# Length of stay (calculated from CO-CI)
length_of_stay <- as.numeric(difftime(co_date, ci_date, units="days"))
summary(length_of_stay)
hist(length_of_stay, main="Frequency of Length of Stay") # will be clearer after removing outliers


# Amount of time between search and check-in
timediff <- as.numeric(difftime(ci_date, srch_date, units="days"))
summary(timediff)
hist(timediff)

### Need to clean up CI and CO dates or remove records with missing values

data <- data[which(data$srch_ci != "NULL"),]
data <- data[which(data$srch_ci < "2017-01-01"),]
summary(as_date(data$srch_ci))
data <- data[which(data$srch_co >= "2015-01-01"),]
data <- data[which(data$srch_co < "2017-01-01"),]
data <- data[which(data$srch_co >= data$srch_ci),]
data <- data[which(data$srch_ci >= data$date_time),]
data <- data[which(is.na(as_date(data$srch_ci)) == FALSE),]
summary(as_date(data$srch_co))
summary(as_date(data$srch_ci))
summary(as_date(data$date_time))


### Cleaned/created variables

srch_date <- date(data$date_time)
is_dotCom <- ifelse(data$site_name == "EXPEDIA.COM", 1, 0)
ci_date <- as_date(data$srch_ci)
co_date <- as_date(data$srch_co)
length_of_stay <- as.numeric(difftime(co_date, ci_date, units="days"))
timediff <- as.numeric(difftime(ci_date, srch_date, units="days"))
summary(timediff)
n.records <- nrow(data)
# Time groups based on histogram of search hour values: 6-10, 10-3, 3-8, 8-1, 1-6?
srch_time_group <- as.factor(ifelse( (hour(data$date_time) >= 6) & (hour(data$date_time) < 10), "morning"
                                     , ifelse( (hour(data$date_time) >= 10) & (hour(data$date_time) < 15), "mid-day"
                                               , ifelse( (hour(data$date_time) >= 15) & (hour(data$date_time) < 20), "evening"
                                                         , ifelse((hour(data$date_time) >= 1) & (hour(data$date_time) < 6), "early_morning", "night" ) ) ) ) )
channel_group <- as.factor(ifelse(data$channel == 541, "541"
                                  , ifelse(data$channel == 510, "510"
                                           , ifelse(data$channel == 231, "231"
                                                    , ifelse(data$channel == 293, "293", "other")))))


# Number of Adults
hist(data$srch_adults_cnt, main="Frequency of Adults", col="blue") # most are 2 people -- look at splitting into groups: 1 person = business?, 2 people = couple, 3+ people = family vacation
adult_groups <- as.factor(ifelse(data$srch_adults_cnt < 2, "business", ifelse(data$srch_adults_cnt > 2, "family", "couple") ))
summary(adult_groups)
plot(adult_groups)

# Number of Kids
hist(data$srch_children_cnt, main="Frequency of Children", col = "red") # majority are 0 -- maybe different populations?
has_kids <- ifelse(data$srch_children_cnt > 0, 1, 0)
hist(has_kids)
sum(has_kids)/n.records # 18.24% have children

#### Might need to be careful about people who mess around with the numbers while searching

# Number of Rooms
hist(data$srch_rm_cnt, main="Frequency of Number of Rooms", col="darkgreen") # most want 1, interesting to see how >1 room users match with other populations
nrow(data[which(data$srch_rm_cnt != 1),]) # 666523 (6.12%) of records want more than 1 room

# Destination ID
summary(data$srch_destination_id)
table(data$srch_destination_id)




# Potentially useful variables:
# datetime, country, is_dotCom, orig_destination_distance, is_mobile, is_package, channel, srch_ci, srch_co, length_of_stay, difftime, srch_adult_cnt,
# adult_groups, srch_children_cnt, has_kids, srch_rm_cnt, srch_destination_id, hotel_country, is_booking,
# prop_is_branded, prop_starrating, distance_band, hist_price_band, popularity_band

set.seed(123)
sample.records <- sample(x=1:n.records, size=10000, replace = FALSE )
data_clean <- data.frame(data[,c(1,3,8,10:20,22:26)], is_dotCom, length_of_stay, timediff, adult_groups, has_kids, channel_group, srch_time_group)
# 10323135 rows, 24 variables

data_clean <- data_clean[sample.records,]
# continuous variables: orig_destination_distance, length_of_stay, timediff
pairs(data_clean[,c(3,21,22)])


# flags: is_dotCom, is_mobile, is_package, is_booking, has_kids
table(data_clean$is_dotCom, data_clean$is_booking)
pchisq(1.49932691, df=1, lower.tail=F) # sample Chisq test: 0.220775 -- is_dotCom is not dependent
table(data_clean$is_mobile, data_clean$is_booking)
pchisq(11.534628, df=1, lower.tail=F) # sample Chisq test: 0.00068311 -- is_mobile is dependent
#table(data_clean$is_mobile, data_clean$has_kids)
table(data_clean$is_package, data_clean$is_booking)
pchisq(85.42057, df=1, lower.tail = F) # sample Chisq test: 2.411x10^-20 -- is_package is dependent
table(data_clean$has_kids, data_clean$is_booking)
pchisq(8.966762, df=1, lower.tail = F) # sample Chisq test: 0.002749 -- has_kids is dependent (0.05)
#table(data_clean$is_package, data_clean$has_kids)
table(data_clean$is_booking, data_clean$prop_is_branded)
pchisq(20.6253271, df=1, lower.tail=F) # sample Chisq test: 5.58523x10^-6 -- prop_is_branded is dependent

# factors: channel (channel_group), srch_month, srch_day_of_week, srch_hour (group?), ci_month, ci_day_of_week, co_month, co_day_of_week, adult_groups, 
# srch_adult_cnt, srch_children_cnt, srch_rm_cnt, prop_is_branded, prop_starrating, distance_band, hist_price_band, popularity_band

table(data_clean$is_booking, as.factor(data_clean$channel))
pchisq(29.37580439, df=10, lower.tail = F) # Sample chisq test: 0.00108 -- channel is dependent
table(data_clean$is_booking, data_clean$channel_group)
pchisq(15.910564, df=4, lower.tail=F) # sample chisq test: 0.00314 -- channel_group is dependent & simpler
table(data_clean$is_booking, data_clean$adult_groups)
pchisq(17.08912726, df=2, lower.tail=F) # sample chisq test: 0.0001946 -- adult_group is dependent & simpler
table(data_clean$is_booking, data_clean$srch_adults_cnt)
pchisq(21.10888219, df=9, lower.tail=F) # sample chisq test: 0.01217 -- srch_adults_cnt is dependent
table(data_clean$is_booking, data_clean$srch_children_cnt)
pchisq(13.5094797, df=7, lower.tail = F) # sample chisq test: 0.06062 -- srch_children_cnt is not dependent
table(data_clean$is_booking, data_clean$srch_rm_cnt)
pchisq(7.07044503, df=7, lower.tail = F) # sample chisq test: 0.42158 -- srch_rm_cnt is not dependent
table(data_clean$is_booking, as.factor(data_clean$prop_starrating))
pchisq(31.32153381, df=5, lower.tail = F) # sample chisq test: 8.092777x10^-6 -- prop_starrating is dependent
table(data_clean$is_booking, as.factor(data_clean$distance_band))
pchisq(11.40539558, df=4, lower.tail = F) # sample chisq test: 0.02236 -- distance_band is dependent (0.05)
table(data_clean$is_booking, as.factor(data_clean$hist_price_band))
pchisq(2.300517495, df=4, lower.tail = F) # sample chisq test: 0.6806748 -- price_band isn't dependent
table(data_clean$is_booking, as.factor(data_clean$popularity_band))
pchisq(8.55546191, df=4, lower.tail = F) # sample chisq test: 0.07322381 -- popularity band isn't dependent (0.05)


### recreate date values for sample ###
table(data_clean$is_booking, wday(date(data_clean$date_time), label=T)) # Search weekday
pchisq(7.523459545, df=6, lower.tail = F) # sample: 0.275 -- independent
table(data_clean$is_booking, month(date(data_clean$date_time), label = T)) # Search month
pchisq(12.9501323, df=11, lower.tail = F) # sample: 0.2965972 -- independent
table(data_clean$is_booking, data_clean$srch_time_group)
pchisq(6.027629, df=4, lower.tail = F) # sample: 0.1970944 -- independent
table(data_clean$is_booking, wday(data_clean$srch_ci, label=T)) # Check-in weekday
pchisq(5.094150769, df=6, lower.tail = F) # sample: 0.53179 -- independent
table(data_clean$is_booking, month(data_clean$srch_ci, label=T)) # Check-in month
pchisq(3.14969503, df=11, lower.tail = F) # sample: 0.98859 -- super independent?
table(data_clean$is_booking, wday(data_clean$srch_co, label=T)) # Check-out weekday
pchisq(7.126870965, df=6, lower.tail=F) # sample: 0.30927 -- not dependent
table(data_clean$is_booking, month(data_clean$srch_co, label=T)) # Check-out month
pchisq(6.62339237, df=11, lower.tail = F) # sample: 0.8287108 -- not dependent


### Destination file

dest <- read_delim("Datafest/Data/dest.txt", "\t", escape_double = FALSE
                   , na = "NULL", trim_ws = TRUE)

summary(dest)

max.casino <- max(dest$popular_entertainment_casino)
dest[which(dest$popular_entertainment_casino == max.casino),2]

###
# Is it okay to average the log10 probability? Or should we transform it back before averaging?
#
# Want to group columns into 19 groups and take the localized average for each row


# From https://www.statmethods.net/advstats/cluster.html
dest2 <- dest[,6:144]
dest2 <- scale(dest2)

## KNN Clustering
# Determine number of clusters
wss <- (nrow(dest2)-1)*sum(apply(dest2,2,var))
for (i in 2:29) wss[i] <- sum(kmeans(dest2, 
                                     centers=i)$withinss)
plot(1:29, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# there is no lowest point, even with scaled data, so I could try 5, 15, and 25 to see what happens

# K-Means Cluster Analysis
kfit5 <- kmeans(dest2, 5) # 5 cluster solution
# get cluster means 
aggregate(dest2,by=list(kfit5$cluster),FUN=mean)


## Ward Hierarchical Clustering
d <- dist(dest2, method = "euclidean") # distance matrix
wfit <- hclust(d, method="ward") 
plot(wfit) # display dendogram
wgroups <- cutree(wfit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(wfit, k=5, border="red")

