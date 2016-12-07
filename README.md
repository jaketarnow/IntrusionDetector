# IntrusionDetector
Intrusion Detection Learning adapted from [KDD](http://kdd.ics.uci.edu/databases/kddcup99/task.html)

## Data
The dataset for this project has been supplied via KDD Cup 1999 Data Information and
Computer Science, University of California, Irvine. Last modified on October 28, 1999. The raw
training data is about 4GB of compressed binary. The TCP dump data contains seven weeks of
network traffic. The data was processed into roughly five million connection records.
Source: http://kdd.ics.uci.edu/databases/kddcup99/kddcup99.html

### Features
Gathered from the KDD Cup Data Set, the features outlined by
Stolfo, defined features that help in distinguishing normal connections from bad connection, i.e.
attacks. They categorized the features into the following: same host, same service, time-based
traffic, host-based traffic, and content features. Same host features examine only connections in
the past two seconds. These features have the same destination host as the current
connections. Same service features examine connections that have the same service as the
current connection in the past two seconds. Both of these together, same host and same
service, are defined as time-based traffic. Host-based traffic on the other hand, involves sorting
connection records by destination host. Thus, focusing on the same host instead of a specific
time window. Finally, content features are added features that help in determining other
predictors that may add to certain behaviors in the data.

### Statistically Significant Features 
24 of the 41 predictors are featured here that are significant for the models below
+flag
+src_bytes
+logged_in
+num_root
+num_file_creations
+count
+srv_count
+serror_rate
+srv_serror_rate
+rerror_rate
+srv_rerror_rate
+same_srv_rate
+diff_srv_rate
+srv_diff_host_rate
+dst_host_count
+dst_host_srv_count
+dst_host_same_srv_rate
+dst_host_diff_srv_rate
+dst_host_same_src_port_rate
+dst_host_srv_diff_host_rate
+dst_host_serror_rate
+dst_host_srv_serror_rate
+dst_host_rerror_rate
+dst_host_srv_rerror_rate

## Models
Due to the fact that our target variable is a class with k > 2 possible values based on the
network connections and types of attacks, we are considering the following models: Logistic
Regression, LDA, (linear discriminant analysis) and QDA (quadratic discriminant analysis). If the
data was less skewed and did not take hours to run, we would have implemented: SVM,
Bagging, and Random Forests. We believe that those models would be good options for this
data, yet testing led to memory errors and system crashes. An alternative for future work is to
over-sample all of the bootstrapping, or to use no random sampling, with the method call of
stratified. The use of bagging and random forest would benefit us as we have a large amount of
data and a large overhead of computation. As mentioned in previous sections, we need to
create a specific sample size for our model testing, thus stratifying our sampling brings many
benefits. The benefits of stratifying the sampling is that it allows us to determine the best
stratification for our data, yet still ensuring the minimum sample necessary to satisfy our
constraints (network connection of good/bad).

## Challenges
The main challenge is the amount of time spent for computation. Our current dataset is a
reduced number of rows, roughly 10% from the original dataset of 400K records. If we work with
a smaller dataset we could possibly get relatively more accurate predictions, yet we still need to
measure skewness. When we find skewness we can then try to balance our data more
appropriately. As mentioned above, we determined the predictors that caused a significant level
of skewness and removed them from the dataset after creating our new sample. Through the
use of doParallel and iterating through the dataset we create a sample of every possible
combination of non-numeric features. This proved to aid in our predictions and gain some solid
insight into the data.

## Results
From the above prediction accuracy results, we are able to distinguish that LDA is the best
option out of the three for predicting the access type of a TCP connection. In our first iteration
we found that logistic regression would be the best option. Yet, after balancing the data with our
new dataset and sample, we found that LDA best predicts the access type. While gaining a
good prediction, users can determine whether an incoming connection is good or bad in
advance. This would protect people from various DOS(denial of service), R2L(unauthorized
access from remote), U2R(unauthorized access to root), and probing attacks.

