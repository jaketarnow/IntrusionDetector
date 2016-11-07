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

train = kddcup.data.ten.percent

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
lda.time <- proc.time()
lda.fit = lda(train$connection_type~train$protocol_type+train$service+train$flag+train$src_bytes+train$dst_bytes+train$land+train$count+train$srv_count, data=train)
lda.pred = predict(lda.fit, train$connection_type)
table(lda.pred$class, train$connection_type)
mean(lda.pred$class == train$connection_type)
proc.time() - lda.time
##### End Block

run_qda = function() {
  qda.time <- proc.time()
  
  qda.fit = qda(connection_type~.-connection_type, data=train)
  #qda.class = predict(qda.fit, Weekly.09_2_10)$class
  #table(qda.class, Direction.09_2_10)
  
  #mean(qda.class == Direction.09_2_10)
  proc.time() - qda.time
}