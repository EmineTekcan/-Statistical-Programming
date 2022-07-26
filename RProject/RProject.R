data<-read.table("DatasetNA.txt",header=TRUE,sep=' ')
data[13,5]<-NA
data[25,8]<-NA
data[44,11]<-NA
data[57,6]<-NA
data[57,9]<-NA
data2<-data[1:nrow(data),4:ncol(data)]

###########################################q1##############################################################3

my_descriptive_table<-function(a){
  
  #Number of observations
  n<-rep(0,ncol(a))
  for(j in 1:ncol(a))
  {
    count<-0
    for(i in 1:nrow(a))
      if(is.na(a[i,j])){
        count<-count
      }
    else{
      count<-count+1
      n[j]<-count
    }
  }
  
  #Minimum
  Minimum<-rep(0,ncol(a))
  
  for(j in 1:ncol(a))
  {
    mini<-a[1,j]
    for(i in 1:nrow(a))
      if(is.na(a[i,j])){
        mini<-mini
      }
    else if(mini>a[i,j]){
      mini<-a[i,j]
    }
    else if (mini<=a[i,j]){
      mini<-mini
    }
    Minimum[j]<-mini
  }
  
  #Maximum
  Maximum<-rep(0,ncol(a))
  
  for(j in 1:ncol(a))
  {
    maxi<-a[1,j]
    for(i in 1:nrow(a))
      if (is.na(a[i,j])){
        maxi<-maxi
      }
    else if(maxi<a[i,j])
    {
      maxi<-a[i,j]
    }
    else {
      maxi<-maxi
    }
    Maximum[j]<-maxi
  }
  
  #Range
  Range<-Maximum-Minimum
  
  #Sum
  Sum<-rep(0,ncol(a))
  for(j in 1:ncol(a))
  {
    sum<-0
    for(i in 1:nrow(a))
      if(is.na(a[i,j])){
        sum<-sum
      }
    else{
      sum<-sum+a[i,j]
      Sum[j]<-sum
    }
  }
  
  #Mean
  Means<-Sum/n
  
  #Median
  Medians<-rep(0,ncol(a))
  for(j in 1:ncol(a))
  {
    vj<-a[1:nrow(a),j]
    s<-sort(vj)
    b<-n[j]
    if(b%%2==0){
      Medians[j]<-(s[b/2]+s[(b/2)+1])/2 
    }
    else{
      Medians[j]<-s[(b+1)/2]
    }
  }
  
  
  #Sum of squares
  SS<--rep(0,ncol(a))
  for(j in 1:ncol(a))
  {
    sum<-0
    for(i in 1:nrow(a))
      if(is.na(a[i,j])){
        sum<-sum
      }
    else{
      sum<-sum+(a[i,j]-Means[j])^2
      SS[j]<-sum
    }
  }
  
  #Variance
  Variance<-SS/n
  
  #Standard deviation
  Std_dev<-sqrt(Variance)
  
  #Descriptive table
  variable<-c("V1","V2","V3","V4","V5","V6","V7","V8")
  d_table<-rbind(variable,n,Minimum,Maximum,Range,Sum,Means,Medians,SS,Variance,Std_dev)
  print(d_table)
  
}

my_descriptive_table(data2)

#####################################################################################################33
my_covariance<-function(a){
  #Number of observations
  n<-rep(0,ncol(a))
  for(j in 1:ncol(a))
  {
    count<-0
    for(i in 1:nrow(a))
      if(is.na(a[i,j])){
        count<-count
      }
    else{
      count<-count+1
      n[j]<-count
    }
  }
  
  #Sum
  Sum<-rep(0,ncol(a))
  for(j in 1:ncol(a))
  {
    sum<-0
    for(i in 1:nrow(a))
      if(is.na(a[i,j])){
        sum<-sum
      }
    else{
      sum<-sum+a[i,j]
      Sum[j]<-sum
    }
  }
  
  #Mean
  Means<-Sum/n
  
  #covariance
  my_cov<-matrix(0,ncol(a),ncol(a))
  for(j in 1:ncol(a))
  {
    for(b in 1:ncol(a))
    {
      sum<-0
      for(i in 1:nrow(a))
        if(is.na(a[i,j])|is.na(a[i,b])){
          sum<-sum
        }
      else{
        sum<-sum+((a[i,j]-Means[j])*(a[i,b]-Means[b]))/n[j]
        my_cov[j,b]<-sum
        
      }
    }
  }
  print(my_cov)
}

my_covariance(data2)

###########################################################################################################

my_correlation<-function(a){
  #Number of observations
  n<-rep(0,ncol(a))
  for(j in 1:ncol(a))
  {
    count<-0
    for(i in 1:nrow(a))
      if(is.na(a[i,j])){
        count<-count
      }
    else{
      count<-count+1
      n[j]<-count
    }
  }
  
  #Sum
  Sum<-rep(0,ncol(a))
  for(j in 1:ncol(a))
  {
    sum<-0
    for(i in 1:nrow(a))
      if(is.na(a[i,j])){
        sum<-sum
      }
    else{
      sum<-sum+a[i,j]
      Sum[j]<-sum
    }
  }
  
  #Mean
  Means<-Sum/n
  
  #covariance
  my_cov<-matrix(0,ncol(a),ncol(a))
  for(j in 1:ncol(a))
  {
    for(b in 1:ncol(a))
    {
      sum<-0
      for(i in 1:nrow(a))
        if(is.na(a[i,j])|is.na(a[i,b])){
          sum<-sum
        }
      else{
        sum<-sum+((a[i,j]-Means[j])*(a[i,b]-Means[b]))/n[j]
        my_cov[j,b]<-sum
        
      }
    }
  }
  
  #Sum of squares
  SS<--rep(0,ncol(a))
  for(j in 1:ncol(a))
  {
    sum<-0
    for(i in 1:nrow(a))
      if(is.na(a[i,j])){
        sum<-sum
      }
    else{
      sum<-sum+(a[i,j]-Means[j])^2
      SS[j]<-sum
    }
  }
  
  #Variance
  Variance<-SS/n
  
  #Standard deviation
  Std_dev<-sqrt(Variance)
  
  #correlations
  my_corr<-matrix(0,ncol(a),ncol(a))
  for(j in 1:nrow(my_cov))
  {
    for(i in 1:ncol(my_cov))
      my_corr[i,j]<-my_cov[i,j]/(Std_dev[i]*Std_dev[j])
  }
  print(my_corr)
}

my_correlation(data2)

###########################################q2#####################################################

#BY GROUP
#Number of observations
n_group1<-c(0,0,0,0,0,0,0,0)
n_group2<-c(0,0,0,0,0,0,0,0)
n_group3<-c(0,0,0,0,0,0,0,0)
n_group4<-c(0,0,0,0,0,0,0,0)

for(j in 4:ncol(data))
{
  count1<-0
  count2<-0
  count3<-0
  count4<-0
  for(i in 1:nrow(data))
    if(data$Group[i]=="Group1"){
      if(is.na(data[i,j])){
        count1<-count1
      }
      else{
        count1<-count1+1
        n_group1[j-3]<-count1
      }
    }
  else if(data$Group[i]=="Group2"){
    if(is.na(data[i,j])){
      count2<-count2
    }
    else{
      count2<-count2+1
      n_group2[j-3]<-count2
    }
  }
  else if(data$Group[i]=="Group3"){
    if(is.na(data[i,j])){
      count3<-count3
    }
    else{
      count3<-count3+1
      n_group3[j-3]<-count3
    }
  }
  else if(data$Group[i]=="Group4"){
    if(is.na(data[i,j])){
      count4<-count4
    }
    else{
      count4<-count4+1
      n_group4[j-3]<-count4
    }
  }
} 
print(n_group1)
print(n_group2)
print(n_group3)
print(n_group4)



#Minimum
Minimum_group1<-c(0,0,0,0,0,0,0,0)
Minimum_group2<-c(0,0,0,0,0,0,0,0)
Minimum_group3<-c(0,0,0,0,0,0,0,0)
Minimum_group4<-c(0,0,0,0,0,0,0,0)

for(j in 4:ncol(data))
{
  mini<-100
  for(i in 1:nrow(data))
    if(data$Group[i]=="Group1"){
      if(is.na(data[i,j])){
        mini<-mini
      }
      else if(mini>data[i,j])
      {
        mini<-data[i,j]
      }
      else {
        mini<-mini
      }
      Minimum_group1[j-3]<-mini
    }
}
print(Minimum_group1)

for(j in 4:ncol(data))
{
  mini<-100
  for(i in 1:nrow(data))
    if(data$Group[i]=="Group2"){
      if(is.na(data[i,j])){
        mini<-mini
      }
      else if(mini>data[i,j])
      {
        mini<-data[i,j]
      }
      else {
        mini<-mini
      }
      Minimum_group2[j-3]<-mini
    }
}
print(Minimum_group2)

for(j in 4:ncol(data))
{
  mini<-100
  for(i in 1:nrow(data))
    if(data$Group[i]=="Group3"){
      if(is.na(data[i,j])){
        mini<-mini
      }
      else if(mini>data[i,j])
      {
        mini<-data[i,j]
      }
      else {
        mini<-mini
      }
      Minimum_group3[j-3]<-mini
    }
}
print(Minimum_group3)

for(j in 4:ncol(data))
{
  mini<-100
  for(i in 1:nrow(data))
    if(data$Group[i]=="Group4"){
      if(is.na(data[i,j])){
        mini<-mini
      }
      else if(mini>data[i,j])
      {
        mini<-data[i,j]
      }
      else {
        mini<-mini
      }
      Minimum_group4[j-3]<-mini
    }
}
print(Minimum_group4)

print(Minimum_group1)
print(Minimum_group2)
print(Minimum_group3)
print(Minimum_group4)



#Maximum
Maximum_group1<-c(0,0,0,0,0,0,0,0)
Maximum_group2<-c(0,0,0,0,0,0,0,0)
Maximum_group3<-c(0,0,0,0,0,0,0,0)
Maximum_group4<-c(0,0,0,0,0,0,0,0)

for(j in 4:ncol(data))
{
  maxi<-0.1
  for(i in 1:nrow(data))
    if(data$Group[i]=="Group1"){
      if(is.na(data[i,j])){
        maxi<-maxi
      }
      else if(maxi<data[i,j])
      {
        maxi<-data[i,j]
      }
      else {
        maxi<-maxi
      }
      Maximum_group1[j-3]<-maxi
    }
}
print(Maximum_group1)

for(j in 4:ncol(data))
{
  maxi<-0.1
  for(i in 1:nrow(data))
    if(data$Group[i]=="Group2"){
      if(is.na(data[i,j])){
        maxi<-maxi
      }
      else if(maxi<data[i,j])
      {
        maxi<-data[i,j]
      }
      else {
        maxi<-maxi
      }
      Maximum_group2[j-3]<-maxi
    }
}
print(Maximum_group2)

for(j in 4:ncol(data))
{
  maxi<-0.1
  for(i in 1:nrow(data))
    if(data$Group[i]=="Group3"){
      if(is.na(data[i,j])){
        maxi<-maxi
      }
      else if(maxi<data[i,j])
      {
        maxi<-data[i,j]
      }
      else {
        maxi<-maxi
      }
      Maximum_group3[j-3]<-maxi
    }
}
print(Maximum_group3)

for(j in 4:ncol(data))
{
  maxi<-0.1
  for(i in 1:nrow(data))
    if(data$Group[i]=="Group4"){
      if(is.na(data[i,j])){
        maxi<-maxi
      }
      else if(maxi<data[i,j])
      {
        maxi<-data[i,j]
      }
      else {
        maxi<-maxi
      }
      Maximum_group4[j-3]<-maxi
    }
}
print(Maximum_group4)

print(Maximum_group1)
print(Maximum_group2)
print(Maximum_group3)
print(Maximum_group4)


#Range
Range_group1<-Maximum_group1-Minimum_group1
Range_group2<-Maximum_group2-Minimum_group2
Range_group3<-Maximum_group3-Minimum_group3
Range_group4<-Maximum_group4-Minimum_group4

print(Range_group1)
print(Range_group2)
print(Range_group3)
print(Range_group4)


#Sum
Sum_group1<-c(0,0,0,0,0,0,0,0)
Sum_group2<-c(0,0,0,0,0,0,0,0)
Sum_group3<-c(0,0,0,0,0,0,0,0)
Sum_group4<-c(0,0,0,0,0,0,0,0)

for(j in 4:ncol(data))
{
  sum1<-0
  sum2<-0
  sum3<-0
  sum4<-0
  for(i in 1:nrow(data))
    if(data$Group[i]=="Group1"){
      if(is.na(data[i,j])){
        sum1<-sum1
      }
      else{
        sum1<-sum1+data[i,j]
        Sum_group1[j-3]<-sum1
      }
    }
  else if(data$Group[i]=="Group2"){
    if(is.na(data[i,j])){
      sum2<-sum2
    }
    else{
      sum2<-sum2+data[i,j]
      Sum_group2[j-3]<-sum2 
    }
  }
  else if(data$Group[i]=="Group3"){
    if(is.na(data[i,j])){
      sum3<-sum3
    }
    else{
      sum3<-sum3+data[i,j]
      Sum_group3[j-3]<-sum3
    }
  }
  else if(data$Group[i]=="Group4"){
    if(is.na(data[i,j])){
      sum4<-sum4
    }
    else{
      sum4<-sum4+data[i,j]
      Sum_group4[j-3]<-sum4
    }
  }
}
print(Sum_group1)
print(Sum_group2)
print(Sum_group3)
print(Sum_group4)


#Mean
Ort_group1<-Sum_group1/n_group1
Ort_group2<-Sum_group2/n_group2
Ort_group3<-Sum_group3/n_group3
Ort_group4<-Sum_group4/n_group4
print(Ort_group1)
print(Ort_group2)
print(Ort_group3)
print(Ort_group4)


#Median
g1<-data[data$Group=="Group1",]
Medyan_g1<-c(0,0,0,0,0,0,0,0)
for(j in 4:ncol(g1))
{
  vj<-g1[1:nrow(g1),j]
  s<-sort(vj)
  a<-n_group1[j-3]
  if(a%%2==0){
    Medyan_g1[j-3]<-(s[a/2]+s[(a/2)+1])/2 
  }
  else{
    Medyan_g1[j-3]<-s[(a+1)/2]
  }
}
print(Medyan_g1)

g2<-data[data$Group=="Group2",]
Medyan_g2<-c(0,0,0,0,0,0,0,0)
for(j in 4:ncol(g2))
{
  vj<-g2[1:nrow(g2),j]
  s<-sort(vj)
  a<-n_group2[j-3]
  if(a%%2==0){
    Medyan_g2[j-3]<-(s[a/2]+s[(a/2)+1])/2 
  }
  else{
    Medyan_g2[j-3]<-s[(a+1)/2]
  }
}
print(Medyan_g2)

g3<-data[data$Group=="Group3",]
Medyan_g3<-c(0,0,0,0,0,0,0,0)
for(j in 4:ncol(g3))
{
  vj<-g3[1:nrow(g3),j]
  s<-sort(vj)
  a<-n_group3[j-3]
  if(a%%2==0){
    Medyan_g3[j-3]<-(s[a/2]+s[(a/2)+1])/2 
  }
  else{
    Medyan_g3[j-3]<-s[(a+1)/2]
  }
}
print(Medyan_g3)

g4<-data[data$Group=="Group4",]
Medyan_g4<-c(0,0,0,0,0,0,0,0)
for(j in 4:ncol(g4))
{
  vj<-g4[1:nrow(g4),j]
  s<-sort(vj)
  a<-n_group4[j-3]
  if(a%%2==0){
    Medyan_g4[j-3]<-(s[a/2]+s[(a/2)+1])/2 
  }
  else{
    Medyan_g4[j-3]<-s[(a+1)/2]
  }
}
print(Medyan_g4)

print(Medyan_g1)
print(Medyan_g2)
print(Medyan_g3)
print(Medyan_g4)

#Sum of squares
SS_group1<--c(0,0,0,0,0,0,0,0)
SS_group2<--c(0,0,0,0,0,0,0,0)
SS_group3<--c(0,0,0,0,0,0,0,0)
SS_group4<--c(0,0,0,0,0,0,0,0)
for(j in 4:ncol(data))
{
  sum1<-0
  sum2<-0
  sum3<-0
  sum4<-0
  for(i in 1:nrow(data))
    if(data$Group[i]=="Group1"){
      if(is.na(data[i,j])){
        sum1<-sum1
      }
      else{
        sum1<-sum1+(data[i,j]-Ort_group1[j-3])^2
        SS_group1[j-3]<-sum1
      }
    }
  else if(data$Group[i]=="Group2"){
    if(is.na(data[i,j])){
      sum2<-sum2
    }
    else{
      sum2<-sum2+(data[i,j]-Ort_group2[j-3])^2
      SS_group2[j-3]<-sum2
    }
  }
  else if(data$Group[i]=="Group3"){
    if(is.na(data[i,j])){
      sum3<-sum3
    }
    else{
      sum3<-sum3+(data[i,j]-Ort_group3[j-3])^2
      SS_group3[j-3]<-sum3
    }
  }
  else if(data$Group[i]=="Group4"){
    if(is.na(data[i,j])){
      sum4<-sum4
    }
    else{
      sum4<-sum4+(data[i,j]-Ort_group4[j-3])^2
      SS_group4[j-3]<-sum4
    }
  }
}
print(SS_group1)
print(SS_group2)
print(SS_group3)
print(SS_group4)


#Variance
Variance_g1<-SS_group1/n_group1
Variance_g2<-SS_group2/n_group2
Variance_g3<-SS_group3/n_group3
Variance_g4<-SS_group4/n_group4
print(Variance_g1)
print(Variance_g2)
print(Variance_g3)
print(Variance_g4)

#Standard deviation
Std_dev_g1<-sqrt(Variance_g1)
Std_dev_g2<-sqrt(Variance_g2)
Std_dev_g3<-sqrt(Variance_g3)
Std_dev_g4<-sqrt(Variance_g4)
print(Std_dev_g1)
print(Std_dev_g2)
print(Std_dev_g3)
print(Std_dev_g4)



#Descriptive table
variable<-c("V1","V2","V3","V4","V5","V6","V7","V8")

d_table_group1<-rbind(variable,n_group1,Minimum_group1,Maximum_group1,Range_group1,Sum_group1,Ort_group1,Medyan,SS_group1,Variance_g1,Std_dev_g1)
print(d_table_group1)

d_table_group2<-rbind(variable,n_group2,Minimum_group2,Maximum_group2,Range_group2,Sum_group2,Ort_group2,Medyan,SS_group2,Variance_g2,Std_dev_g2)
print(d_table_group2)

d_table_group3<-rbind(variable,n_group3,Minimum_group3,Maximum_group3,Range_group3,Sum_group3,Ort_group3,Medyan,SS_group3,Variance_g3,Std_dev_g3)
print(d_table_group3)

d_table_group4<-rbind(variable,n_group4,Minimum_group4,Maximum_group4,Range_group4,Sum_group4,Ort_group4,Medyan,SS_group4,Variance_g4,Std_dev_g4)
print(d_table_group4)


#Covariance
g1<-data[data$Group=="Group1",]
my_cov_g1<-matrix(0,8,8)
for(j in 4:ncol(g1))
{
  for(a in 4:ncol(g1))
  {
    sum<-0
    for(i in 1:nrow(g1))
      if(is.na(g1[i,j])|is.na(g1[i,a])){
        sum<-sum
      }
    else{
      sum<-sum+((g1[i,j]-Ort_group1[j-3])*(g1[i,a]-Ort_group1[a-3]))/n_group1[j-3]
      my_cov_g1[j-3,a-3]<-sum
    }
  }
}
print(my_cov_g1)

g2<-data[data$Group=="Group2",]
my_cov_g2<-matrix(0,8,8)
for(j in 4:ncol(g2))
{
  for(a in 4:ncol(g2))
  {
    sum<-0
    for(i in 1:nrow(g2))
      if(is.na(g2[i,j])|is.na(g2[i,a])){
        sum<-sum
      }
    else{
      sum<-sum+((g2[i,j]-Ort_group2[j-3])*(g2[i,a]-Ort_group2[a-3]))/n_group2[j-3]
      my_cov_g2[j-3,a-3]<-sum
    }
  }
}
print(my_cov_g2)

g3<-data[data$Group=="Group3",]
my_cov_g3<-matrix(0,8,8)
for(j in 4:ncol(g3))
{
  for(a in 4:ncol(g3))
  {
    sum<-0
    for(i in 1:nrow(g3))
      if(is.na(g3[i,j])|is.na(g3[i,a])){
        sum<-sum
      }
    else{
      sum<-sum+((g3[i,j]-Ort_group3[j-3])*(g3[i,a]-Ort_group3[a-3]))/n_group3[j-3]
      my_cov_g3[j-3,a-3]<-sum
    }
  }
}
print(my_cov_g3)

g4<-data[data$Group=="Group4",]
my_cov_g4<-matrix(0,8,8)
for(j in 4:ncol(g4))
{
  for(a in 4:ncol(g4))
  {
    sum<-0
    for(i in 1:nrow(g4))
      if(is.na(g4[i,j])|is.na(g4[i,a])){
        sum<-sum
      }
    else{
      sum<-sum+((g4[i,j]-Ort_group4[j-3])*(g4[i,a]-Ort_group4[a-3]))/n_group4[j-3]
      my_cov_g4[j-3,a-3]<-sum
    }
  }
}
print(my_cov_g4)


#correlations
my_corr_g1<-matrix(0,8,8)
for(j in 1:nrow(my_cov_g1))
{
  for(i in 1:ncol(my_cov_g1))
    my_corr_g1[i,j]<-my_cov_g1[i,j]/(Std_dev_g1[i]*Std_dev_g1[j])
}
print(my_corr_g1)

my_corr_g2<-matrix(0,8,8)
for(j in 1:nrow(my_cov_g2))
{
  for(i in 1:ncol(my_cov_g2))
    my_corr_g2[i,j]<-my_cov_g2[i,j]/(Std_dev_g2[i]*Std_dev_g2[j])
}
print(my_corr_g2)

my_corr_g3<-matrix(0,8,8)
for(j in 1:nrow(my_cov_g3))
{
  for(i in 1:ncol(my_cov_g3))
    my_corr_g3[i,j]<-my_cov_g3[i,j]/(Std_dev_g3[i]*Std_dev_g3[j])
}
print(my_corr_g3)

my_corr_g4<-matrix(0,8,8)
for(j in 1:nrow(my_cov_g4))
{
  for(i in 1:ncol(my_cov_g4))
    my_corr_g4[i,j]<-my_cov_g4[i,j]/(Std_dev_g4[i]*Std_dev_g4[j])
}
print(my_corr_g4)


#BY GROUP and GENDER
#Number of observations
n_g1f<-c(0,0,0,0,0,0,0,0)
n_g1m<-c(0,0,0,0,0,0,0,0)
n_g2f<-c(0,0,0,0,0,0,0,0)
n_g2m<-c(0,0,0,0,0,0,0,0)
n_g3f<-c(0,0,0,0,0,0,0,0)
n_g3m<-c(0,0,0,0,0,0,0,0)
n_g4f<-c(0,0,0,0,0,0,0,0)
n_g4m<-c(0,0,0,0,0,0,0,0)

for(j in 4:ncol(data))
{
  count1f<-0
  count1m<-0
  count2f<-0
  count2m<-0
  count3f<-0
  count3m<-0
  count4f<-0
  count4m<-0
  for(i in 1:nrow(data))
    if(data$Group[i]=="Group1"&data$Gender[i]=="Female"){
      if(is.na(data[i,j])){
        count1f<-count1f
      }
      else{
        count1f<-count1f+1
        n_g1f[j-3]<-count1f
      }
    }
  else if(data$Group[i]=="Group1"&data$Gender[i]=="Male"){
    if(is.na(data[i,j])){
      count1m<-count1m
    }
    else{
      count1m<-count1m+1
      n_g1m[j-3]<-count1m
    }
  }
  else if(data$Group[i]=="Group2"&data$Gender[i]=="Female"){
    if(is.na(data[i,j])){
      count2f<-count2f
    }
    else{
      count2f<-count2f+1
      n_g2f[j-3]<-count2f
    }
  }
  else if(data$Group[i]=="Group2"&data$Gender[i]=="Male"){
    if(is.na(data[i,j])){
      count2m<-count2m
    }
    else{
      count2m<-count2m+1
      n_g2m[j-3]<-count2m
    }
  }
  else if(data$Group[i]=="Group3"&data$Gender[i]=="Female"){
    if(is.na(data[i,j])){
      count3f<-count3f
    }
    else{
      count3f<-count3f+1
      n_g3f[j-3]<-count3f
    }
  }
  else if(data$Group[i]=="Group3"&data$Gender[i]=="Male"){
    if(is.na(data[i,j])){
      count3m<-count3m
    }
    else{
      count3m<-count3m+1
      n_g3m[j-3]<-count3m
    }
  }
  else if(data$Group[i]=="Group4"&data$Gender[i]=="Female"){
    if(is.na(data[i,j])){
      count4f<-count4f
    }
    else{
      count4f<-count4f+1
      n_g4f[j-3]<-count4f
    }
  }
  else if(data$Group[i]=="Group4"&data$Gender[i]=="Male"){
    if(is.na(data[i,j])){
      count4m<-count4m
    }
    else{
      count4m<-count4m+1
      n_g4m[j-3]<-count4m
    }
  }
}
print(n_g1f)
print(n_g1m)
print(n_g2f)
print(n_g2m)
print(n_g3f)
print(n_g3m)
print(n_g4f)
print(n_g4m)


#Minimum
Minimum_g1f<-c(0,0,0,0,0,0,0,0)
Minimum_g1m<-c(0,0,0,0,0,0,0,0)
Minimum_g2f<-c(0,0,0,0,0,0,0,0)
Minimum_g2m<-c(0,0,0,0,0,0,0,0)
Minimum_g3f<-c(0,0,0,0,0,0,0,0)
Minimum_g3m<-c(0,0,0,0,0,0,0,0)
Minimum_g4f<-c(0,0,0,0,0,0,0,0)
Minimum_g4m<-c(0,0,0,0,0,0,0,0)

for(j in 4:ncol(data))
{
  mini<-100
  for(i in 1:nrow(data))
    if(data$Group[i]=="Group1"&data$Gender[i]=="Female"){
      if(is.na(data[i,j])){
        mini<-mini
      }
      else if(mini>data[i,j])
      {
        mini<-data[i,j]
      }
      else {
        mini<-mini
      }
      Minimum_g1f[j-3]<-mini
    }
}
print(Minimum_g1f)

for(j in 4:ncol(data))
{
  mini<-100
  for(i in 1:nrow(data))
    if(data$Group[i]=="Group1"&data$Gender[i]=="Male"){
      if(is.na(data[i,j])){
        mini<-mini
      }
      else if(mini>data[i,j])
      {
        mini<-data[i,j]
      }
      else {
        mini<-mini
      }
      Minimum_g1m[j-3]<-mini
    }
}
print(Minimum_g1m)

for(j in 4:ncol(data))
{
  mini<-100
  for(i in 1:nrow(data))
    if(data$Group[i]=="Group2"&data$Gender[i]=="Female"){
      if(is.na(data[i,j])){
        mini<-mini
      }
      else if(mini>data[i,j])
      {
        mini<-data[i,j]
      }
      else {
        mini<-mini
      }
      Minimum_g2f[j-3]<-mini
    }
}
print(Minimum_g2f)

for(j in 4:ncol(data))
{
  mini<-100
  for(i in 1:nrow(data))
    if(data$Group[i]=="Group2"&data$Gender[i]=="Male"){
      if(is.na(data[i,j])){
        mini<-mini
      }
      else if(mini>data[i,j])
      {
        mini<-data[i,j]
      }
      else {
        mini<-mini
      }
      Minimum_g2m[j-3]<-mini
    }
}
print(Minimum_g2m)

for(j in 4:ncol(data))
{
  mini<-100
  for(i in 1:nrow(data))
    if(data$Group[i]=="Group3"&data$Gender[i]=="Female"){
      if(is.na(data[i,j])){
        mini<-mini
      }
      else if(mini>data[i,j])
      {
        mini<-data[i,j]
      }
      else {
        mini<-mini
      }
      Minimum_g3f[j-3]<-mini
    }
}
print(Minimum_g3f)

for(j in 4:ncol(data))
{
  mini<-100
  for(i in 1:nrow(data))
    if(data$Group[i]=="Group3"&data$Gender[i]=="Male"){
      if(is.na(data[i,j])){
        mini<-mini
      }
      else if(mini>data[i,j])
      {
        mini<-data[i,j]
      }
      else {
        mini<-mini
      }
      Minimum_g3m[j-3]<-mini
    }
}
print(Minimum_g3m)

for(j in 4:ncol(data))
{
  mini<-100
  for(i in 1:nrow(data))
    if(data$Group[i]=="Group4"&data$Gender=="Female"){
      if(is.na(data[i,j])){
        mini<-mini
      }
      else if(mini>data[i,j])
      {
        mini<-data[i,j]
      }
      else {
        mini<-mini
      }
      Minimum_g4f[j-3]<-mini
    }
}
print(Minimum_g4f)

for(j in 4:ncol(data))
{
  mini<-100
  for(i in 1:nrow(data))
    if(data$Group[i]=="Group4"&data$Gender[i]=="Male"){
      if(is.na(data[i,j])){
        mini<-mini
      }
      else if(mini>data[i,j])
      {
        mini<-data[i,j]
      }
      else {
        mini<-mini
      }
      Minimum_g4m[j-3]<-mini
    }
}
print(Minimum_g4m)

print(Minimum_g1f)
print(Minimum_g1m)
print(Minimum_g2f)
print(Minimum_g2m)
print(Minimum_g3f)
print(Minimum_g3m)
print(Minimum_g4f)
print(Minimum_g4m)


#Maximum
Maximum_g1f<-c(0,0,0,0,0,0,0,0)
Maximum_g1m<-c(0,0,0,0,0,0,0,0)
Maximum_g2f<-c(0,0,0,0,0,0,0,0)
Maximum_g2m<-c(0,0,0,0,0,0,0,0)
Maximum_g3f<-c(0,0,0,0,0,0,0,0)
Maximum_g3m<-c(0,0,0,0,0,0,0,0)
Maximum_g4f<-c(0,0,0,0,0,0,0,0)
Maximum_g4m<-c(0,0,0,0,0,0,0,0)

for(j in 4:ncol(data))
{
  maxi<-0.1
  for(i in 1:nrow(data))
    if(data$Group[i]=="Group1"&data$Gender[i]=="Female"){
      if(is.na(data[i,j])){
        maxi<-maxi
      }
      else if(maxi<data[i,j])
      {
        maxi<-data[i,j]
      }
      else {
        maxi<-maxi
      }
      Maximum_g1f[j-3]<-maxi
    }
}
print(Maximum_g1f)

for(j in 4:ncol(data))
{
  maxi<-0.1
  for(i in 1:nrow(data))
    if(data$Group[i]=="Group1"&data$Gender[i]=="Male"){
      if(is.na(data[i,j])){
        maxi<-maxi
      }
      else if(maxi<data[i,j])
      {
        maxi<-data[i,j]
      }
      else {
        maxi<-maxi
      }
      Maximum_g1m[j-3]<-maxi
    }
}
print(Maximum_g1m)

for(j in 4:ncol(data))
{
  maxi<-0.1
  for(i in 1:nrow(data))
    if(data$Group[i]=="Group2"&data$Gender[i]=="Female"){
      if(is.na(data[i,j])){
        maxi<-maxi
      }
      else if(maxi<data[i,j])
      {
        maxi<-data[i,j]
      }
      else {
        maxi<-maxi
      }
      Maximum_g2f[j-3]<-maxi
    }
}
print(Maximum_g2f)

for(j in 4:ncol(data))
{
  maxi<-0.1
  for(i in 1:nrow(data))
    if(data$Group[i]=="Group2"&data$Gender[i]=="Male"){
      if(is.na(data[i,j])){
        maxi<-maxi
      }
      else if(maxi<data[i,j])
      {
        maxi<-data[i,j]
      }
      else {
        maxi<-maxi
      }
      Maximum_g2m[j-3]<-maxi
    }
}
print(Maximum_g2m)

for(j in 4:ncol(data))
{
  maxi<-0.1
  for(i in 1:nrow(data))
    if(data$Group[i]=="Group3"&data$Gender[i]=="Female"){
      if(is.na(data[i,j])){
        maxi<-maxi
      }
      else if(maxi<data[i,j])
      {
        maxi<-data[i,j]
      }
      else {
        maxi<-maxi
      }
      Maximum_g3f[j-3]<-maxi
    }
}
print(Maximum_g3f)

for(j in 4:ncol(data))
{
  maxi<-0.1
  for(i in 1:nrow(data))
    if(data$Group[i]=="Group3"&data$Gender[i]=="Male"){
      if(is.na(data[i,j])){
        maxi<-maxi
      }
      else if(maxi<data[i,j])
      {
        maxi<-data[i,j]
      }
      else {
        maxi<-maxi
      }
      Maximum_g3m[j-3]<-maxi
    }
}
print(Maximum_g3m)

for(j in 4:ncol(data))
{
  maxi<-0.1
  for(i in 1:nrow(data))
    if(data$Group[i]=="Group4"&data$Gender[i]=="Female"){
      if(is.na(data[i,j])){
        maxi<-maxi
      }
      else if(maxi<data[i,j])
      {
        maxi<-data[i,j]
      }
      else {
        maxi<-maxi
      }
      Maximum_g4f[j-3]<-maxi
    }
}
print(Maximum_g4f)

for(j in 4:ncol(data))
{
  maxi<-0.1
  for(i in 1:nrow(data))
    if(data$Group[i]=="Group4"&data$Gender[i]=="Male"){
      if(is.na(data[i,j])){
        maxi<-maxi
      }
      else if(maxi<data[i,j])
      {
        maxi<-data[i,j]
      }
      else {
        maxi<-maxi
      }
      Maximum_g4m[j-3]<-maxi
    }
}
print(Maximum_g4m)

print(Maximum_g1f)
print(Maximum_g1m)
print(Maximum_g2f)
print(Maximum_g2m)
print(Maximum_g3f)
print(Maximum_g3m)
print(Maximum_g4f)
print(Maximum_g4m)


#Range
Range_g1f<-Maximum_g1f-Minimum_g1f
Range_g1m<-Maximum_g1m-Minimum_g1m
Range_g2f<-Maximum_g2f-Minimum_g2f
Range_g2m<-Maximum_g2m-Minimum_g2m
Range_g3f<-Maximum_g3f-Minimum_g3f
Range_g3m<-Maximum_g3m-Minimum_g3m
Range_g4f<-Maximum_g4f-Minimum_g4f
Range_g4m<-Maximum_g4m-Minimum_g4m

print(Range_g1f)
print(Range_g1m)
print(Range_g2f)
print(Range_g2m)
print(Range_g3f)
print(Range_g3m)
print(Range_g4f)
print(Range_g4m)


#Sum
Sum_g1f<-c(0,0,0,0,0,0,0,0)
Sum_g1m<-c(0,0,0,0,0,0,0,0)
Sum_g2f<-c(0,0,0,0,0,0,0,0)
Sum_g2m<-c(0,0,0,0,0,0,0,0)
Sum_g3f<-c(0,0,0,0,0,0,0,0)
Sum_g3m<-c(0,0,0,0,0,0,0,0)
Sum_g4f<-c(0,0,0,0,0,0,0,0)
Sum_g4m<-c(0,0,0,0,0,0,0,0)

for(j in 4:ncol(data))
{
  sum1f<-0
  sum1m<-0
  sum2f<-0
  sum2m<-0
  sum3f<-0
  sum3m<-0
  sum4f<-0
  sum4m<-0
  for(i in 1:nrow(data))
    if(data$Group[i]=="Group1"&data$Gender[i]=="Female"){
      if(is.na(data[i,j])){
        sum1f<-sum1f
      }
      else{
        sum1f<-sum1f+data[i,j]
        Sum_g1f[j-3]<-sum1f
      }
    }
  else if(data$Group[i]=="Group1"&data$Gender[i]=="Male"){
    if(is.na(data[i,j])){
      sum1m<-sum1m
    }
    else{
      sum1m<-sum1m+data[i,j]
      Sum_g1m[j-3]<-sum1m 
    }
  }
  else if(data$Group[i]=="Group2"&data$Gender[i]=="Female"){
    if(is.na(data[i,j])){
      sum2f<-sum2f
    }
    else{
      sum2f<-sum2f+data[i,j]
      Sum_g2f[j-3]<-sum2f
    }
  }
  else if(data$Group[i]=="Group2"&data$Gender[i]=="Male"){
    if(is.na(data[i,j])){
      sum2m<-sum2m
    }
    else{
      sum2m<-sum2m+data[i,j]
      Sum_g2m[j-3]<-sum2m 
    }
  }
  else if(data$Group[i]=="Group3"&data$Gender[i]=="Female"){
    if(is.na(data[i,j])){
      sum3f<-sum3f
    }
    else{
      sum3f<-sum3f+data[i,j]
      Sum_g3f[j-3]<-sum3f
    }
  }
  else if(data$Group[i]=="Group3"&data$Gender[i]=="Male"){
    if(is.na(data[i,j])){
      sum3m<-sum3m
    }
    else{
      sum3m<-sum3m+data[i,j]
      Sum_g3m[j-3]<-sum3m 
    }
  }
  else if(data$Group[i]=="Group4"&data$Gender[i]=="Female"){
    if(is.na(data[i,j])){
      sum4f<-sum4f
    }
    else{
      sum4f<-sum4f+data[i,j]
      Sum_g4f[j-3]<-sum4f
    }
  }
  else if(data$Group[i]=="Group4"&data$Gender[i]=="Male"){
    if(is.na(data[i,j])){
      sum4m<-sum4m
    }
    else{
      sum4m<-sum4m+data[i,j]
      Sum_g4m[j-3]<-sum4m 
    }
  }
}
print(Sum_g1f)
print(Sum_g1m)
print(Sum_g2f)
print(Sum_g2m)
print(Sum_g3f)
print(Sum_g3m)
print(Sum_g4f)
print(Sum_g4m)


#Mean
Ort_g1f<-Sum_g1f/n_g1f
Ort_g1m<-Sum_g1m/n_g1m
Ort_g2f<-Sum_g2f/n_g2f
Ort_g2m<-Sum_g2m/n_g2m
Ort_g3f<-Sum_g3f/n_g3f
Ort_g3m<-Sum_g3m/n_g3m
Ort_g4f<-Sum_g4f/n_g4f
Ort_g4m<-Sum_g4m/n_g4m
print(Ort_g1f)
print(Ort_g1m)
print(Ort_g2f)
print(Ort_g2m)
print(Ort_g3f)
print(Ort_g3m)
print(Ort_g4f)
print(Ort_g4m)


#Median
g1f<-data[data$Group=="Group1"&data$Gender=="Female",]
Medyan_g1f<-c(0,0,0,0,0,0,0,0)
for(j in 4:ncol(g1f))
{
  vj<-g1f[1:nrow(g1f),j]
  s<-sort(vj)
  a<-n_g1f[j-3]
  if(a%%2==0){
    Medyan_g1f[j-3]<-(s[a/2]+s[(a/2)+1])/2 
  }
  else{
    Medyan_g1f[j-3]<-s[(a+1)/2]
  }
}
print(Medyan_g1f)

g1m<-data[data$Group=="Group1"&data$Gender=="Male",]
Medyan_g1m<-c(0,0,0,0,0,0,0,0)
for(j in 4:ncol(g1m))
{
  vj<-g1m[1:nrow(g1m),j]
  s<-sort(vj)
  a<-n_g1m[j-3]
  if(a%%2==0){
    Medyan_g1m[j-3]<-(s[a/2]+s[(a/2)+1])/2 
  }
  else{
    Medyan_g1m[j-3]<-s[(a+1)/2]
  }
}
print(Medyan_g1m)

g2f<-data[data$Group=="Group2"&data$Gender=="Female",]
Medyan_g2f<-c(0,0,0,0,0,0,0,0)
for(j in 4:ncol(g2f))
{
  vj<-g2f[1:nrow(g2f),j]
  s<-sort(vj)
  a<-n_g2f[j-3]
  if(a%%2==0){
    Medyan_g2f[j-3]<-(s[a/2]+s[(a/2)+1])/2 
  }
  else{
    Medyan_g2f[j-3]<-s[(a+1)/2]
  }
}
print(Medyan_g2f)

g2m<-data[data$Group=="Group2"&data$Gender=="Male",]
Medyan_g2m<-c(0,0,0,0,0,0,0,0)
for(j in 4:ncol(g2m))
{
  vj<-g2m[1:nrow(g2m),j]
  s<-sort(vj)
  a<-n_g2m[j-3]
  if(a%%2==0){
    Medyan_g2m[j-3]<-(s[a/2]+s[(a/2)+1])/2 
  }
  else{
    Medyan_g2m[j-3]<-s[(a+1)/2]
  }
}
print(Medyan_g2m)

g3f<-data[data$Group=="Group3"&data$Gender=="Female",]
Medyan_g3f<-c(0,0,0,0,0,0,0,0)
for(j in 4:ncol(g3f))
{
  vj<-g3f[1:nrow(g3f),j]
  s<-sort(vj)
  a<-n_g3f[j-3]
  if(a%%2==0){
    Medyan_g3f[j-3]<-(s[a/2]+s[(a/2)+1])/2 
  }
  else{
    Medyan_g3f[j-3]<-s[(a+1)/2]
  }
}
print(Medyan_g3f)

g3m<-data[data$Group=="Group3"&data$Gender=="Male",]
Medyan_g3m<-c(0,0,0,0,0,0,0,0)
for(j in 4:ncol(g3m))
{
  vj<-g3m[1:nrow(g3m),j]
  s<-sort(vj)
  a<-n_g3m[j-3]
  if(a%%2==0){
    Medyan_g3m[j-3]<-(s[a/2]+s[(a/2)+1])/2 
  }
  else{
    Medyan_g3m[j-3]<-s[(a+1)/2]
  }
}
print(Medyan_g3m)

g4f<-data[data$Group=="Group4"&data$Gender=="Female",]
Medyan_g4f<-c(0,0,0,0,0,0,0,0)
for(j in 4:ncol(g4f))
{
  vj<-g4f[1:nrow(g4f),j]
  s<-sort(vj)
  a<-n_g4f[j-3]
  if(a%%2==0){
    Medyan_g4f[j-3]<-(s[a/2]+s[(a/2)+1])/2 
  }
  else{
    Medyan_g4f[j-3]<-s[(a+1)/2]
  }
}
print(Medyan_g4f)

g4m<-data[data$Group=="Group4"&data$Gender=="Male",]
Medyan_g4m<-c(0,0,0,0,0,0,0,0)
for(j in 4:ncol(g4m))
{
  vj<-g4m[1:nrow(g4m),j]
  s<-sort(vj)
  a<-n_g4m[j-3]
  if(a%%2==0){
    Medyan_g4m[j-3]<-(s[a/2]+s[(a/2)+1])/2 
  }
  else{
    Medyan_g4m[j-3]<-s[(a+1)/2]
  }
}
print(Medyan_g4m)


#Sum of squares
SS_g1f<--c(0,0,0,0,0,0,0,0)
SS_g1m<--c(0,0,0,0,0,0,0,0)
SS_g2f<--c(0,0,0,0,0,0,0,0)
SS_g2m<--c(0,0,0,0,0,0,0,0)
SS_g3f<--c(0,0,0,0,0,0,0,0)
SS_g3m<--c(0,0,0,0,0,0,0,0)
SS_g4f<--c(0,0,0,0,0,0,0,0)
SS_g4m<--c(0,0,0,0,0,0,0,0)
for(j in 4:ncol(data))
{
  sum1f<-0
  sum1m<-0
  sum2f<-0
  sum2m<-0
  sum3f<-0
  sum3m<-0
  sum4f<-0
  sum4m<-0
  for(i in 1:nrow(data))
    if(data$Group[i]=="Group1"&data$Gender[i]=="Female"){
      if(is.na(data[i,j])){
        sum1f<-sum1f
      }
      else{
        sum1f<-sum1f+(data[i,j]-Ort_g1f[j-3])^2
        SS_g1f[j-3]<-sum1f
      }
    }
  else if(data$Group[i]=="Group1"&data$Gender[i]=="Male"){
    if(is.na(data[i,j])){
      sum1m<-sum1m
    }
    else{
      sum1m<-sum1m+(data[i,j]-Ort_g1m[j-3])^2
      SS_g1m[j-3]<-sum1m
    }
  }
  else if(data$Group[i]=="Group2"&data$Gender[i]=="Female"){
    if(is.na(data[i,j])){
      sum2f<-sum2f
    }
    else{
      sum2f<-sum2f+(data[i,j]-Ort_g2f[j-3])^2
      SS_g2f[j-3]<-sum2f
    }
  }
  else if(data$Group[i]=="Group2"&data$Gender[i]=="Male"){
    if(is.na(data[i,j])){
      sum2m<-sum2m
    }
    else{
      sum2m<-sum2m+(data[i,j]-Ort_g2m[j-3])^2
      SS_g2m[j-3]<-sum2m
    }
  }
  else if(data$Group[i]=="Group3"&data$Gender[i]=="Female"){
    if(is.na(data[i,j])){
      sum3f<-sum3f
    }
    else{
      sum3f<-sum3f+(data[i,j]-Ort_g3f[j-3])^2
      SS_g3f[j-3]<-sum3f
    }
  }
  else if(data$Group[i]=="Group3"&data$Gender[i]=="Male"){
    if(is.na(data[i,j])){
      sum3m<-sum3m
    }
    else{
      sum3m<-sum3m+(data[i,j]-Ort_g3m[j-3])^2
      SS_g3m[j-3]<-sum3m
    }
  }
  else if(data$Group[i]=="Group4"&data$Gender[i]=="Female"){
    if(is.na(data[i,j])){
      sum4f<-sum4f
    }
    else{
      sum4f<-sum4f+(data[i,j]-Ort_g4f[j-3])^2
      SS_g4f[j-3]<-sum4f
    }
  }
  else if(data$Group[i]=="Group4"&data$Gender[i]=="Male"){
    if(is.na(data[i,j])){
      sum4m<-sum4m
    }
    else{
      sum4m<-sum4m+(data[i,j]-Ort_g4m[j-3])^2
      SS_g4m[j-3]<-sum4m
    }
  }
}
print(SS_g1f)
print(SS_g1m)
print(SS_g2f)
print(SS_g2m)
print(SS_g3f)
print(SS_g3m)
print(SS_g4f)
print(SS_g4m)


#Variance
Variance_g1f<-SS_g1f/n_g1f
Variance_g1m<-SS_g1m/n_g1m
Variance_g2f<-SS_g2f/n_g2f
Variance_g2m<-SS_g2m/n_g2m
Variance_g3f<-SS_g3f/n_g3f
Variance_g3m<-SS_g3m/n_g3m
Variance_g4f<-SS_g4f/n_g4f
Variance_g4m<-SS_g4m/n_g4m
print(Variance_g1f)
print(Variance_g1m)
print(Variance_g2f)
print(Variance_g2m)
print(Variance_g3f)
print(Variance_g3m)
print(Variance_g4f)
print(Variance_g4m)

#Standard deviation
Std_dev_g1f<-sqrt(Variance_g1f)
Std_dev_g1m<-sqrt(Variance_g1m)
Std_dev_g2f<-sqrt(Variance_g2f)
Std_dev_g2m<-sqrt(Variance_g2m)
Std_dev_g3f<-sqrt(Variance_g3f)
Std_dev_g3m<-sqrt(Variance_g3m)
Std_dev_g4f<-sqrt(Variance_g4f)
Std_dev_g4m<-sqrt(Variance_g4m)
print(Std_dev_g1f)
print(Std_dev_g1m)
print(Std_dev_g2f)
print(Std_dev_g2m)
print(Std_dev_g3f)
print(Std_dev_g3m)
print(Std_dev_g4f)
print(Std_dev_g4m)


#covariance
my_cov_g1f<-matrix(0,8,8)
for(j in 4:ncol(g1f))
{
  for(a in 4:ncol(g1f))
  {
    sum<-0
    for(i in 1:nrow(g1f))
      if(is.na(g1f[i,j])|is.na(g1f[i,a])){
        sum<-sum
      }
    else{
      sum<-sum+((g1f[i,j]-Ort_g1f[j-3])*(g1f[i,a]-Ort_g1f[a-3]))/n_g1f[j-3]
      my_cov_g1f[j-3,a-3]<-sum
    }
  }
}
print(my_cov_g1f)

my_cov_g1m<-matrix(0,8,8)
for(j in 4:ncol(g1m))
{
  for(a in 4:ncol(g1m))
  {
    sum<-0
    for(i in 1:nrow(g1m))
      if(is.na(g1m[i,j])|is.na(g1m[i,a])){
        sum<-sum
      }
    else{
      sum<-sum+((g1m[i,j]-Ort_g1m[j-3])*(g1m[i,a]-Ort_g1m[a-3]))/n_g1m[j-3]
      my_cov_g1m[j-3,a-3]<-sum
    }
  }
}
print(my_cov_g1m)

my_cov_g2f<-matrix(0,8,8)
for(j in 4:ncol(g2f))
{
  for(a in 4:ncol(g2f))
  {
    sum<-0
    for(i in 1:nrow(g2f))
      if(is.na(g2f[i,j])|is.na(g2f[i,a])){
        sum<-sum
      }
    else{
      sum<-sum+((g2f[i,j]-Ort_g2f[j-3])*(g2f[i,a]-Ort_g2f[a-3]))/n_g2f[j-3]
      my_cov_g2f[j-3,a-3]<-sum
    }
  }
}
print(my_cov_g2f)

my_cov_g2m<-matrix(0,8,8)
for(j in 4:ncol(g2m))
{
  for(a in 4:ncol(g2m))
  {
    sum<-0
    for(i in 1:nrow(g2m))
      if(is.na(g2m[i,j])|is.na(g2m[i,a])){
        sum<-sum
      }
    else{
      sum<-sum+((g2m[i,j]-Ort_g2m[j-3])*(g2m[i,a]-Ort_g2m[a-3]))/n_g2m[j-3]
      my_cov_g2m[j-3,a-3]<-sum
    }
  }
}
print(my_cov_g2m)

my_cov_g3f<-matrix(0,8,8)
for(j in 4:ncol(g3f))
{
  for(a in 4:ncol(g3f))
  {
    sum<-0
    for(i in 1:nrow(g3f))
      if(is.na(g3f[i,j])|is.na(g3f[i,a])){
        sum<-sum
      }
    else{
      sum<-sum+((g3f[i,j]-Ort_g3f[j-3])*(g3f[i,a]-Ort_g3f[a-3]))/n_g3f[j-3]
      my_cov_g3f[j-3,a-3]<-sum
    }
  }
}
print(my_cov_g3f)

my_cov_g3m<-matrix(0,8,8)
for(j in 4:ncol(g3m))
{
  for(a in 4:ncol(g3m))
  {
    sum<-0
    for(i in 1:nrow(g3m))
      if(is.na(g3m[i,j])|is.na(g3m[i,a])){
        sum<-sum
      }
    else{
      sum<-sum+((g3m[i,j]-Ort_g3m[j-3])*(g3m[i,a]-Ort_g3m[a-3]))/n_g3m[j-3]
      my_cov_g3m[j-3,a-3]<-sum
    }
  }
}
print(my_cov_g3m)

my_cov_g4f<-matrix(0,8,8)
for(j in 4:ncol(g4f))
{
  for(a in 4:ncol(g4f))
  {
    sum<-0
    for(i in 1:nrow(g4f))
      if(is.na(g4f[i,j])|is.na(g4f[i,a])){
        sum<-sum
      }
    else{
      sum<-sum+((g4f[i,j]-Ort_g4f[j-3])*(g4f[i,a]-Ort_g4f[a-3]))/n_g4f[j-3]
      my_cov_g4f[j-3,a-3]<-sum
    }
  }
}
print(my_cov_g4f)

my_cov_g4m<-matrix(0,8,8)
for(j in 4:ncol(g4m))
{
  for(a in 4:ncol(g4m))
  {
    sum<-0
    for(i in 1:nrow(g4m))
      if(is.na(g4m[i,j])|is.na(g4m[i,a])){
        sum<-sum
      }
    else{
      sum<-sum+((g4m[i,j]-Ort_g4m[j-3])*(g4m[i,a]-Ort_g4m[a-3]))/n_g4m[j-3]
      my_cov_g4m[j-3,a-3]<-sum
    }
  }
}
print(my_cov_g4m)


#correlations
my_corr_g1f<-matrix(0,8,8)
for(j in 1:nrow(my_cov_g1f))
{
  for(i in 1:ncol(my_cov_g1f))
    my_corr_g1f[i,j]<-my_cov_g1f[i,j]/(Std_dev_g1f[i]*Std_dev_g1f[j])
}
print(my_corr_g1f)

my_corr_g1m<-matrix(0,8,8)
for(j in 1:nrow(my_cov_g1m))
{
  for(i in 1:ncol(my_cov_g1m))
    my_corr_g1m[i,j]<-my_cov_g1m[i,j]/(Std_dev_g1m[i]*Std_dev_g1m[j])
}
print(my_corr_g1m)

my_corr_g2f<-matrix(0,8,8)
for(j in 1:nrow(my_cov_g2f))
{
  for(i in 1:ncol(my_cov_g2f))
    my_corr_g2f[i,j]<-my_cov_g2f[i,j]/(Std_dev_g2f[i]*Std_dev_g2f[j])
}
print(my_corr_g2f)

my_corr_g2m<-matrix(0,8,8)
for(j in 1:nrow(my_cov_g2m))
{
  for(i in 1:ncol(my_cov_g2m))
    my_corr_g2m[i,j]<-my_cov_g2m[i,j]/(Std_dev_g2m[i]*Std_dev_g2m[j])
}
print(my_corr_g2m)

my_corr_g3f<-matrix(0,8,8)
for(j in 1:nrow(my_cov_g3f))
{
  for(i in 1:ncol(my_cov_g3f))
    my_corr_g3f[i,j]<-my_cov_g3f[i,j]/(Std_dev_g3f[i]*Std_dev_g3f[j])
}
print(my_corr_g3f)

my_corr_g3m<-matrix(0,8,8)
for(j in 1:nrow(my_cov_g3m))
{
  for(i in 1:ncol(my_cov_g3m))
    my_corr_g3m[i,j]<-my_cov_g3m[i,j]/(Std_dev_g3m[i]*Std_dev_g3m[j])
}
print(my_corr_g3m)

my_corr_g4f<-matrix(0,8,8)
for(j in 1:nrow(my_cov_g4f))
{
  for(i in 1:ncol(my_cov_g4f))
    my_corr_g4f[i,j]<-my_cov_g4f[i,j]/(Std_dev_g4f[i]*Std_dev_g4f[j])
}
print(my_corr_g4f)

my_corr_g4m<-matrix(0,8,8)
for(j in 1:nrow(my_cov_g4m))
{
  for(i in 1:ncol(my_cov_g4m))
    my_corr_g4m[i,j]<-my_cov_g4m[i,j]/(Std_dev_g4m[i]*Std_dev_g4m[j])
}
print(my_corr_g4m)

##################################q4################################################################

my_scaled_data<-function(a){
  for(b in 1:ncol(a)){
    z=1
    if(is.character(a[2,b])==TRUE){
      z<-z
    }
    else if(is.character(a[2,b])==FALSE){
      z<-b
      
    }
    if(z>1) break;
  }
  
  
  #Number of observations
  n<-rep(0,ncol(a)-z+1)
  for(j in z:ncol(a))
  {
    count<-0
    for(i in 1:nrow(a))
      if(is.na(a[i,j])){
        count<-count
      }
    else{
      count<-count+1
      n[j-z+1]<-count
    }
  }
  
  #Sum
  Sum<-rep(0,ncol(a)-z+1)
  for(j in z:ncol(a))
  {
    sum<-0
    for(i in 1:nrow(a))
      if(is.na(a[i,j])){
        sum<-sum
      }
    else{
      sum<-sum+a[i,j]
      Sum[j-z+1]<-sum
    }
  }
  
  #Mean
  Ort<-Sum/n
  
  o1<-matrix(Ort[1],nrow(a),1)
  o2<-matrix(Ort[2],nrow(a),1)
  o3<-matrix(Ort[3],nrow(a),1)
  o4<-matrix(Ort[4],nrow(a),1)
  o5<-matrix(Ort[5],nrow(a),1)
  o6<-matrix(Ort[6],nrow(a),1)
  o7<-matrix(Ort[7],nrow(a),1)
  o8<-matrix(Ort[8],nrow(a),1)
  
  Mean_matrix<-cbind(o1,o2,o3,o4,o5,o6,o7,o8)
  
  
  #Sum of squares
  SS<--rep(0,ncol(a)-z+1)
  for(j in z:ncol(a))
  {
    sum<-0
    for(i in 1:nrow(a))
      if(is.na(a[i,j])){
        sum<-sum
      }
    else{
      sum<-sum+(a[i,j]-Ort[j-z+1])^2
      SS[j-z+1]<-sum
    }
  }
  
  #Variance
  Variance<-SS/n
  
  #Standard deviation
  Std_dev<-sqrt(Variance)
  
  Std_dev
  s1<-matrix(Std_dev[1],nrow(a),1)
  s2<-matrix(Std_dev[2],nrow(a),1)
  s3<-matrix(Std_dev[3],nrow(a),1)
  s4<-matrix(Std_dev[4],nrow(a),1)
  s5<-matrix(Std_dev[5],nrow(a),1)
  s6<-matrix(Std_dev[6],nrow(a),1)
  s7<-matrix(Std_dev[7],nrow(a),1)
  s8<-matrix(Std_dev[8],nrow(a),1)
  Std_dv_matrix<-cbind(s1,s2,s3,s4,s5,s6,s7,s8)
  
  data2<-a[1:nrow(a),z:ncol(a)]
  x_data<-(data2-Mean_matrix)/Std_dv_matrix
  
  scaledx_data<-data
  scaledx_data[1:nrow(a),z:ncol(a)]<-x_data
  print(scaledx_data)
}

my_scaled_data(data)

