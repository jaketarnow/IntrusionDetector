library(ISLR)

getwd()

##### Setup Data
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
##### End Block

##### Remove Random Period From The Dataset
# connection_types <- data.frame(kddcup.data.ten.percent$connection_type)
# kddcup.data.ten.percent$connection_type = substr(connection_types, 0, nchar(connection_types)-1)
##### End Block

set.seed(420)
train = kddcup.data
train.good = train[train$access_type == "Good", ]
train.bad = train[train$access_type == "Bad", ]
train.good.75k = train.good[sample(nrow(train.good), size = 75000, replace = TRUE), ]
train.bad.75k = train.bad[sample(nrow(train.bad), size = 75000, replace = TRUE), ]
train.150k = rbind(train.good.75k, train.bad.75k)
test.150k = kddcup.testdata[sample(nrow(kddcup.testdata), size = 150000, replace = TRUE), ]

##### Logisitc Regression
glm.fit.time <- proc.time()
glm.fit = glm(train$connection_type~.-train$connection_type-train$num_outbound_cmds-train$access_type, data=train, family=binomial)
glm.probs = predict(glm.fit, type="response")
glm.pred = rep("Good", length(glm.probs))
glm.pred[glm.probs > 0.5] = "Bad"
proc.time() - glm.fit.time 

summary(glm.fit)
mean(glm.pred == train$access_type)
##### End Block
library(ISLR)
library(MASS)
##### LDA
lda.fit = lda(train$connection_type~train$flag + train$access_type + train$num_outbound_cmds, data=train, family = binomial)
lda.pred = predict(lda.fit, train$connection_type, type = "response")
table(lda.pred$class, train$connection_type)
mean(lda.pred$class == train$connection_type)
proc.time() - lda.time
##### End Block