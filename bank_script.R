deposit <- rep(0,length(x$deposit))
table(deposit)
for(i in 1:length(x$deposit))
  if(x$deposit[i]=="yes")
    deposit[i]=1
table(deposit)

default <- rep(0,length(x$default))
table(default)
for(i in 1:length(x$default))
  if(x$default[i]=="yes")
    default[i]=1
table(default)

housing <- rep(0,length(x$housing))
table(housing)
for(i in 1:length(x$housing))
  if(x$housing[i]=="yes")
    housing[i]=1
table(housing)

loan <- rep(0,length(x$loan))
table(loan)
for(i in 1:length(x$loan))
  if(x$loan[i]=="yes")
    loan[i]=1
table(loan)

admin <- rep(0,length(x$job))
table(admin)
for(i in 1:length(x$job))
  if(x$job[i]=="admin.")
    admin[i]=1
table(admin)

blue_collar <- rep(0,length(x$job))
table(blue_collar)
for(i in 1:length(x$job))
  if(x$job[i]=="blue-collar")
    blue_collar[i]=1
table(blue_collar)

entrepreneur <- rep(0,length(x$job))
table(entrepreneur)
for(i in 1:length(x$job))
  if(x$job[i]=="entrepreneur")
    entrepreneur[i]=1
table(entrepreneur)

housemaid <- rep(0,length(x$job))
table(housemaid)
for(i in 1:length(x$job))
  if(x$job[i]=="housemaid")
    housemaid[i]=1
table(housemaid)

management <- rep(0,length(x$job))
table(management)
for(i in 1:length(x$job))
  if(x$job[i]=="management")
    management[i]=1
table(management)

retired <- rep(0,length(x$job))
table(retired)
for(i in 1:length(x$job))
  if(x$job[i]=="retired")
    retired[i]=1
table(retired)

self_employed <- rep(0,length(x$job))
table(self_employed)
for(i in 1:length(x$job))
  if(x$job[i]=="self-employed")
    self_employed[i]=1
table(self_employed)

services <- rep(0,length(x$job))
table(services)
for(i in 1:length(x$job))
  if(x$job[i]=="services")
    services[i]=1
table(services)

student <- rep(0,length(x$job))
table(student)
for(i in 1:length(x$job))
  if(x$job[i]=="student")
    student[i]=1
table(student)

technician <- rep(0,length(x$job))
table(technician)
for(i in 1:length(x$job))
  if(x$job[i]=="technician")
    technician[i]=1
table(technician)

unemployed <- rep(0,length(x$job))
table(unemployed)
for(i in 1:length(x$job))
  if(x$job[i]=="unemployed")
    unemployed[i]=1
table(unemployed)

unk_job <- rep(0,length(x$job))
table(unk_job)
for(i in 1:length(x$job))
  if(x$job[i]=="unknown")
    unk_job[i]=1
table(unk_job)

single <- rep(0,length(x$marital))
table(single)
for(i in 1:length(x$marital))
  if(x$marital[i]=="single")
    single[i]=1
table(single)

divorced <- rep(0,length(x$marital))
table(divorced)
for(i in 1:length(x$marital))
  if(x$marital[i]=="divorced")
    divorced[i]=1
table(divorced)

married <- rep(0,length(x$marital))
table(married)
for(i in 1:length(x$marital))
  if(x$marital[i]=="married")
    married[i]=1
table(married)

table(x$education)

primary <- rep(0,length(x$education))
table(primary)
for(i in 1:length(x$education))
  if(x$education[i] == "primary")
    primary[i]=1
table(primary)

secondary <- rep(0,length(x$education))
table(secondary)
for(i in 1:length(x$education))
  if(x$education[i] == "secondary")
    secondary[i]=1
table(secondary)

tertiary <- rep(0,length(x$education))
table(tertiary)
for(i in 1:length(x$education))
  if(x$education[i] == "tertiary")
    tertiary[i]=1
table(tertiary)

unk_education <- rep(0,length(x$education))
table(unk_education)
for(i in 1:length(x$education))
  if(x$education[i] == "unknown")
    unk_education[i]=1
table(unk_education)

table(x$contact)

cellular <- rep(0,length(x$contact))
table(cellular)
for(i in 1:length(x$contact))
  if(x$contact[i] == "cellular")
    cellular[i]=1
table(cellular)

telephone <- rep(0,length(x$contact))
table(telephone)
for(i in 1:length(x$contact))
  if(x$contact[i] == "telephone")
    telephone[i]=1
table(telephone)

unk_contact <- rep(0,length(x$contact))
table(unk_contact)
for(i in 1:length(x$contact))
  if(x$contact[i] == "unknown")
    unk_contact[i]=1
table(unk_contact)

data<-data.frame(x$age,admin,blue_collar,entrepreneur,
                 housemaid,management,retired,self_employed,
                 student,services,technician,unemployed,unk_job,
                 single,married,divorced,primary,secondary,
                 tertiary,unk_education,default,x$balance,housing,
                 loan,cellular,telephone,unk_contact)
View(data)
head(data)

temp <- data
head(temp)
b = rep(0,10)
for(i in 1:10)
{
  v <- kmeans(temp,i)
  b[i] = v$withinss
}
plot(seq(1,10,1),b,"l")

v <- kmeans(temp,4)
v$centers
v$size

table(x$day)
table(x$month)

month <- rep(0,length(x$month))
for(i in 1:length(x$month))
{
  if(x$month[i] == "jan")
    month[i] = 1
  else if(x$month[i] == "feb")
    month[i] = 2
  else if(x$month[i] == "mar")
    month[i] = 3
  else if(x$month[i] == "apr")
    month[i] = 4
  else if(x$month[i] == "may")
    month[i] = 5
  else if(x$month[i] == "jun")
    month[i] = 6
  else if(x$month[i] == "jul")
    month[i] = 7
  else if(x$month[i] == "aug")
    month[i] = 8
  else if(x$month[i] == "sep")
    month[i] = 9
  else if(x$month[i] == "oct")
    month[i] = 10
  else if(x$month[i] == "nov")
    month[i] = 11
  else if(x$month[i] == "dec")
    month[i] = 12
}
table(month)
table(x$pdays)
table(x$poutcome)

failure <- rep(0,length(x$poutcome))
table(failure)
for(i in 1:length(x$poutcome))
  if(x$poutcome[i] == "failure")
    failure[i]=1
table(failure)

other <- rep(0,length(x$poutcome))
table(other)
for(i in 1:length(x$poutcome))
  if(x$poutcome[i] == "other")
    other[i]=1
table(other)

success <- rep(0,length(x$poutcome))
table(success)
for(i in 1:length(x$poutcome))
  if(x$poutcome[i] == "success")
    success[i]=1
table(success)

unk_poutcome <- rep(0,length(x$poutcome))
table(unk_poutcome)
for(i in 1:length(x$poutcome))
  if(x$poutcome[i] == "unknown")
    unk_poutcome[i]=1
table(unk_poutcome)

data<-data.frame(x$age,admin,blue_collar,entrepreneur,
                 housemaid,management,retired,self_employed,
                 student,services,technician,unemployed,unk_job,
                 single,married,divorced,primary,secondary,
                 tertiary,unk_education,default,x$balance,housing,
                 loan,cellular,telephone,unk_contact,x$day,month,
                 x$duration,x$campaign,x$pdays,x$previous,failure,
                 other,success,unk_poutcome)

temp <- data
head(temp)
b = rep(0,10)
for(i in 1:10)
{
  v <- kmeans(temp,i)
  b[i] = v$withinss
}
plot(seq(1,10,1),b,"l")

v <- kmeans(temp,4)
v$centers
v$size
#,xlim=c(1,200),ylim=c(-1,1000)
hist(temp[v$cluster==1,1])
table(temp[v$cluster==4,1])

table(x$previous)
table(x$pdays)

table(x$job)
table(x$marital)

final <- data.frame(data,deposit)
View(final)

f <- kmeans(temp[1:37],4)
f$size
f$centers

c1_outcome <- final[f$cluster==1,38]
sum(c1_outcome)/length(c1_outcome)

c2_outcome <- final[f$cluster==2,38]
sum(c2_outcome)/length(c2_outcome)

c3_outcome <- final[f$cluster==3,38]
sum(c3_outcome)/length(c3_outcome)

c4_outcome <- final[f$cluster==4,38]
sum(c4_outcome)/length(c4_outcome)

?kmeans

sum(c4_outcome)
f$size

v$centers

sample <- data
head(sample)
b_vec = rep(0,10)
for(i in 1:10)
{
  vec <- kmeans(sample,i)
  b_vec[i] = vec$tot.withinss
}
plot(seq(1,10,1),b_vec,"l")









