library(ISLR)
library(MASS)
library(e1071)

# Data Setup --------------------------------------------------------------
# setwd("path to directory where the data files are stored")
getwd() # to check if the working directory is the correct path

kddcup.data = read.csv("kddcup.data.csv")
kddcup.data.ten.percent = read.csv("kddcup.data_10_percent.csv")
kddcup.testdata = read.csv("kddcup.testdata.csv")

bad_connections <- c(
  "back.",
  "buffer_overflow.",
  "ftp_write.",
  "guess_passwd.",
  "imap.",
  "ipsweep.",
  "land.",
  "loadmodule.",
  "multihop.",
  "neptune.",
  "nmap.",
  "perl.",
  "phf.",
  "pod.",
  "portsweep.",
  "rootkit.",
  "satan.",
  "smurf.",
  "spy.",
  "teardrop.",
  "warezclient.",
  "warezmaster."
)
#good_connections <- c()

kddcup.data <- cbind(kddcup.data, rep("Good", dim(kddcup.data)[1]))
kddcup.data.ten.percent <- cbind(kddcup.data.ten.percent, rep("Good", dim(kddcup.data.ten.percent)[1]))

kddcup.testdata <- cbind(kddcup.testdata, rep(NA, dim(kddcup.testdata)[1]))
kddcup.testdata <- cbind(kddcup.testdata, rep(NA, dim(kddcup.testdata)[1]))

column_names <- c(
  "duration",
  "protocol_type",
  "service",
  "flag",
  "src_bytes",
  "dst_bytes",
  "land",
  "wrong_fragment",
  "urgent",
  "hot",
  "num_failed_logins",
  "logged_in",
  "num_compromised",
  "root_shell",
  "su_attempted",
  "num_root",
  "num_file_creations",
  "num_shells",
  "num_access_files",
  "num_outbound_cmds",
  "is_host_login",
  "is_guest_login",
  "count",
  "srv_count",
  "serror_rate",
  "srv_serror_rate",
  "rerror_rate",
  "srv_rerror_rate",
  "same_srv_rate",
  "diff_srv_rate",
  "srv_diff_host_rate",
  "dst_host_count",
  "dst_host_srv_count",
  "dst_host_same_srv_rate",
  "dst_host_diff_srv_rate",
  "dst_host_same_src_port_rate",
  "dst_host_srv_diff_host_rate",
  "dst_host_serror_rate",
  "dst_host_srv_serror_rate",
  "dst_host_rerror_rate",
  "dst_host_srv_rerror_rate",
  "connection_type",
  "access_type"
)

colnames(kddcup.data) = column_names
colnames(kddcup.data.ten.percent) = column_names
colnames(kddcup.testdata) = column_names

kddcup.data$access_type = "Good"
kddcup.data.ten.percent$access_type = "Good"

kddcup.data$access_type[kddcup.data$connection_type %in% bad_connections] = "Bad"
kddcup.data.ten.percent$access_type[kddcup.data.ten.percent$connection_type %in% bad_connections] = "Bad"

train = kddcup.data

##### Remove Random Period From The Dataset
# connection_types <- data.frame(kddcup.data.ten.percent$connection_type)
# kddcup.data.ten.percent$connection_type = substr(connection_types, 0, nchar(connection_types)-1)


# Feature Selection -------------------------------------------------------
library(e1071)
names = names(train)
classes = sapply(train, class)
par(mfrow=c(4,4))
numeric.classes = c("integer", "numeric")
for (name in names[classes %in% numeric.classes]) {
  print(name)
  hist.predictor = hist(train[,name], xlab=name)
  hist.predictor
  print(summary(hist.predictor$density))
}


# based off of the density skews, we can drop the following predictors
# duration
# src_bytes
# dst_bytes
# hot
# num_compromised
# num_root
# count
# srv_count

summary(train$connection_type)

hist(train$duration)
skewness(train$duration)
summary(train$duration)

summary(train$protocol_type)

summary(train$service)

# Dimensionality Reduction ------------------------------------------------
set.seed(666)

train = kddcup.data
train = train[train$duration < 26.73927, ]  #From Skewness
train = train[train$duration < 1, ]         #Using Histogram

protocols = names(summary(train$protocol_type))
services = names(summary(train$service))
flags = names(summary(train$flag))

train = train[0, ]
sample.size = 1750000

for (protocol in 1:length(protocols)) {
  print(protocols[protocol])
  train.sample = kddcup.data[kddcup.data$protocol_type == protocol, ]
  train.sample = train.sample[sample(nrow(train.sample), size = sample.size, replace = nrow(train.sample) < sample.size), ]
  train = rbind(train, train.sample)
  print(nrow(train.sample) < sample.size)
}

train.protocol.icmp = train[train$protocol_type == "icmp", ]
train.protocol.icmp = train.protocol.icmp[sample(nrow(train.protocol.icmp), size = 1750000, replace = FALSE), ]
train.protocol.tcp = train[train$protocol_type == "tcp", ]
train.protocol.tcp = train.protocol.tcp[sample(nrow(train.protocol.tcp), size = 1750000, replace = FALSE), ]
train.protocol.udp = train[train$protocol_type == "udp", ]
train.protocol.udp = train.protocol.udp[sample(nrow(train.protocol.udp), size = 1750000, replace = TRUE), ]

train = rbind(train.protocol.icmp, train.protocol.tcp, train.protocol.icmp)

rm(train.protocol.udp)
rm(train.protocol.tcp)
rm(train.protocol.icmp)

unused.services <- c("domain_u", "http_2784", "ntp_u", "tftp_u")
train = train[!train$service %in% unused.services, ]


unnecessary.features = c("duration", "connection_type", "access_type")


# Trainng and Testing Set Sample ------------------------------------------
set.seed(420)

train.good = train[train$access_type == "Good", ]
train.bad = train[train$access_type == "Bad", ]
train.good.1 = train.good[sample(nrow(train.good), size = 500000, replace = FALSE), ]
train.bad.1 = train.bad[sample(nrow(train.bad), size = 500000, replace = FALSE), ]
train.1M = rbind(train.good.1, train.bad.1)

rm(train.good)
rm(train.bad)
rm(train.good.1)
rm(train.bad.1)

test.1 = kddcup.data.ten.percent[!kddcup.data.ten.percent$service == "pm_dump", ]
test.2 = kddcup.testdata[sample(nrow(kddcup.testdata), size = 1000000, replace = FALSE), ]

# Logistic Regression -----------------------------------------------------
glm.fit.time <- proc.time()

glm.fit = glm(access_type~.-num_outbound_cmds-connection_type, data=train.1M, family=binomial)
summary(glm.fit)
glm.probs = predict(glm.fit, newdata = test.1, type = "response")
glm.pred = ifelse(glm.probs > 0.5, 1, 0)
glm.pred.accesses = test.1$access_type

glm.time = proc.time() - glm.fit.time
glm.time[3]/60

table(glm.pred, glm.pred.accesses)
mean(glm.pred == glm.pred.accesses)


# Linear Discriminant Analysis (LDA) --------------------------------------
lda.fit = lda(train$connection_type~train$flag + train$access_type + train$num_outbound_cmds, data=train, family = binomial)
lda.pred = predict(lda.fit, train$connection_type, type = "response")
table(lda.pred$class, train$connection_type)
mean(lda.pred$class == train$connection_type)
proc.time() - lda.time


