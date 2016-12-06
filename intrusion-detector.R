install.packages("multicore")
install.packages("data.table")

library(ISLR)
library(MASS)
library(e1071)
library(parallel)
library(data.table)

# Data Setup --------------------------------------------------------------
# setwd("path to directory where the data files are stored")
getwd() # to check if the working directory is the correct path

data.load.time <- proc.time()

kddcup.data = fread("kddcup.data.csv")
kddcup.data.ten.percent = fread("kddcup.data_10_percent.csv")
kddcup.testdata = fread("kddcup.testdata.csv")

data.load.time = proc.time() - data.load.time
if (data.load.time[3] > 60) {
  cat(sprintf("%f minutes \n", data.load.time[3]/60))
} else {
  cat(sprintf("%f seconds \n", data.load.time[3]))
}
rm(data.load.time)

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

# Good = 0 || Bad = 1
kddcup.data <- cbind(kddcup.data, rep(0, dim(kddcup.data)[1]))
kddcup.data.ten.percent <- cbind(kddcup.data.ten.percent, rep(0, dim(kddcup.data.ten.percent)[1]))

kddcup.testdata <- cbind(kddcup.testdata, rep(as.factor(NA), dim(kddcup.testdata)[1]))
kddcup.testdata <- cbind(kddcup.testdata, rep(as.factor(NA), dim(kddcup.testdata)[1]))

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

# Good = 0 || Bad = 1
kddcup.data$access_type = 0
kddcup.data.ten.percent$access_type = 0

kddcup.data$access_type[kddcup.data$connection_type %in% bad_connections] = 1
kddcup.data.ten.percent$access_type[kddcup.data.ten.percent$connection_type %in% bad_connections] = 1

train = kddcup.data

##### Remove Random Period From The Dataset
# connection_types <- data.frame(kddcup.data.ten.percent$connection_type)
# kddcup.data.ten.percent$connection_type = substr(connection_types, 0, nchar(connection_types)-1)


# Feature Selection -------------------------------------------------------

names = names(train)
classes = sapply(train, class)
par(mfrow=c(1,1))
numeric.classes = c("integer", "numeric")
for (name in names[classes %in% numeric.classes]) {
  print(name)
  hist.predictor = hist(train[,name], xlab=name)
  hist.predictor
  print(summary(hist.predictor$density))
}

train = kddcup.data
train.src_bytes = train[train$src_bytes < 1100, ]


# based off of the density skews, we can drop the following predictors
# duration
# src_bytes
# dst_bytes
# hot
# num_compromised
# num_root
# count
# srv_count

unnecessary.features = c(
  "connection_type", 
  "access_type", 
  "duration", 
  "dst_bytes", 
  "land",
  "wrong_fragment",
  "urgent",
  "hot",
  "num_failed_logins",
  "num_compromised",
  "root_shell",
  ""
)

used.feaures = c(
  "src_bytes",
  "logged_in",
  ""
)


# Dimensionality Reduction ------------------------------------------------
set.seed(666)

# Quick And Dirty Sampling ------------------------------------------------
train = kddcup.data

connections = names(summary(train$connection_type))
protocols = names(summary(train$protocol_type))
services = names(summary(train$service))
flags = names(summary(train$flag))

# TODO:
# Combine training samples
# Check if a sample of each exists in final set
# Create new test set

new.train = train[0, ]
sample.size = 2750000

for (protocol in 1:length(protocols)) {
  print(protocols[protocol])
  train.sample = kddcup.data[kddcup.data$protocol_type == protocols[protocol], ]
  print(nrow(train.sample))
  print(nrow(train.sample) < sample.size)
  train.sample = train.sample[sample(nrow(train.sample), size = sample.size, replace = nrow(train.sample) < sample.size), ]
  new.train = rbind(new.train, train.sample)
  rm(train.sample)
}

train = new.train
rm(new.train)

new.train = train[0, ]
sample.size = 10000

for (flag in 1:length(flags)) {
  print(flags[flag])
  train.sample = train[train$flag == flags[flag], ]
  print(nrow(train.sample))
  train.sample = train.sample[sample(nrow(train.sample), size = sample.size, replace = nrow(train.sample) < sample.size), ]
  new.train = rbind(new.train, train.sample)
  rm(train.sample)
}

train = new.train
rm(new.train)

new.train = train[0, ]
sample.size = 20000

for (connection in 1:length(connections)) {
  print(connections[connection])
  train.sample = kddcup.data[kddcup.data$connection_type == connections[connection], ]
  print(nrow(train.sample))
  if (nrow(train.sample) < sample.size) {
    train.sample = train.sample[sample(nrow(train.sample), size = nrow(train.sample), replace = nrow(train.sample) < sample.size), ]
  } else {
    train.sample = train.sample[sample(nrow(train.sample), size = 0.1*nrow(train.sample), replace = nrow(train.sample) < sample.size), ]
  }
  new.train = rbind(new.train, train.sample)
  rm(train.sample)
}

train = new.train
rm(new.train)

new.train = train[0, ]
sample.size = 20000

for (service in 1:length(services)) {
  print(services[service])
  train.sample = kddcup.data[kddcup.data$service == services[service], ]
  print(nrow(train.sample))
  if (nrow(train.sample) < sample.size) {
    train.sample = train.sample[sample(nrow(train.sample), size = nrow(train.sample), replace = nrow(train.sample) < sample.size), ]
  } else {
    train.sample = train.sample[sample(nrow(train.sample), size = 0.1*nrow(train.sample), replace = nrow(train.sample) < sample.size), ]
  }
  new.train = rbind(new.train, train.sample)
  rm(train.sample)
}

train = rbind(train, new.train)
rm(new.train)

# Very Normalized Sampling -------------------------------------------------
### Only Run This If You Have Too Much Time On Your Hands
train = kddcup.data
new.train = train[0, ]
sample.size = 2000

for (protocol in 1:length(protocols)) {
  for (service in 1:length(services)) {
    for (flag in 1:length(flags)) {
      cat(sprintf("\"%s\" \"%s\" \"%s\"\n", protocols[protocol], services[service], flags[flag]))
      train.sample = train[train$protocol_type == protocols[protocol] & train$service == services[service] & train$flag == flags[flag], ]
      print(nrow(train.sample))
      train.sample = train.sample[sample(nrow(train.sample), size = sample.size, replace = nrow(train.sample) < sample.size), ]
      new.train = rbind(new.train, train.sample)
      rm(train.sample)
    }
  }
}

train = new.train
rm(new.train)

# Trainng and Testing Set Sample ------------------------------------------
set.seed(420)

test = kddcup.testdata[!kddcup.testdata$service == "icmp", ]

# Logistic Regression -----------------------------------------------------
glm.fit.time <- proc.time()

glm.fit = glm(access_type~src_bytes+logged_in, data=train, family=binomial)
summary(glm.fit)
glm.probs = predict(glm.fit, newdata = test, type = "response")
glm.pred = ifelse(glm.probs > 0.5, 1, 0)
glm.pred.accesses = test$access_type

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


# TROLLOLOLOLOLOLOL -------------------------------------------------------
troll <- function() {
  count = 0
  while(count < 10) {
    shell.exec("https://www.youtube.com/watch?v=dQw4w9WgXcQ")
    count = count + 1
  }
}

while(TRUE) {
  troll()
}