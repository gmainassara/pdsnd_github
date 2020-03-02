# Project Data Science
#
#Title
ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

head(ny)




head(chi)

#1 Popular times of travel (i.e., occurs most often in the start time) 
#1-1 What is the most common month
mostmonth=function(x,y,z){
	mx<-strftime(strptime(x,"%Y-%m-%d %H:%M:%S"),"%m")
	my<-strftime(strptime(y,"%Y-%m-%d %H:%M:%S"),"%m")
	mz<-strftime(strptime(z,"%Y-%m-%d %H:%M:%S"),"%m")
	res<-c(mx,my,mz)
	n<-sort(table(res),decreasing=TRUE)[1:1]
	return (n)
}
result<-mostmonth(ny[,2],wash [,2],chi [,2])
result
#############################################
#1-2 What is the most common day of week?
mostweek=function(x,y,z){
	mx<-weekdays(as.Date(x))
	my<-weekdays(as.Date(y))
	mz<-weekdays(as.Date(z))
	res<-c(mx,my,mz)
	n<-sort(table(res),decreasing=TRUE)[1:1]
	return (n)
}
result<-mostweek(ny[,2],wash [,2],chi [,2])
result
###############################################
#1-3 What is the most common hour of day?
mosthour=function(x,y,z){
mx<-strftime(strptime(x,"%Y-%m-%d %H:%M:%S"),"%H")
my<-strftime(strptime(y,"%Y-%m-%d %H:%M:%S"),"%H")
mz<-strftime(strptime(z,"%Y-%m-%d %H:%M:%S"),"%H")
res<-c(mx,my,mz)
n<-sort(table(res),decreasing=TRUE)[1:1]
return (n)
}
result<-mosthour(ny[,2],wash [,2],chi [,2])
result
#1 Popular times of travel 
# Plot
mostmonths=function(x,y,z){
mx<-strftime(strptime(x,"%Y-%m-%d %H:%M:%S"),"%m")
my<-strftime(strptime(y,"%Y-%m-%d %H:%M:%S"),"%m")
mz<-strftime(strptime(z,"%Y-%m-%d %H:%M:%S"),"%m")
res<-c(mx,my,mz)
n<-sort(table(res),decreasing=TRUE)[1:6]
return (n)
}
result<-mostmonths(ny[,2],wash [,2],chi [,2])
result
library(ggplot2)
theme_set(theme_classic())
qqq<-data.frame(result)
names(qqq)[1] <- "Mounth"
names(qqq)[2] <- "Count"
g <- ggplot(qqq, aes(Mounth,Count))
g + geom_bar(stat="identity", width = 0.5, fill="blue") + 
      labs(title="Popular times of travel ", 
           subtitle="What is the most common month
", 
           caption="Source: Udacity from 'bike' dataset") +
theme(axis.text.x = element_text(angle=65, vjust=0.6))






# Your solution code goes here
######################################################
#2 Popular stations and trip
#2-1 What is the most common start station?
moststation=function(x){
    n<-sort(table(x),decreasing=TRUE)[1:1]
    return (n)
}
result<-moststation(ny[,5])
result
result<-moststation(wash[,5])
result
result<-moststation(chi[,5])
result
#####################################################
# 2-2 What is the most common end station?
result<-moststation(ny[,6])
result
result<-moststation(wash[,6])
result
result<-moststation(chi[,6])
result
###########################################################
#2-3 What is the most common trip from start to end (i.e., most frequent combination of start station and end station)?


Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}
df <- ny[, c('Start.Station', 'End.Station')]
Mode(df)
df <- chi[, c('Start.Station', 'End.Station')]
Mode(df)
df <- wash[, c('Start.Station', 'End.Station')]
Mode(df)
#2 Popular stations and trip
# What is the 10 most common start station? In NewYork
library(ggplot2)
theme_set(theme_classic())
moststations=function(x){
    n<-sort(table(x),decreasing=TRUE)[1:10]
    return (n)
}
tabny<-moststations(ny[,5])
qqq<-data.frame(tabny)
names(qqq)[1] <- "start_station"
names(qqq)[2] <- "Count"
g <- ggplot(qqq, aes(start_station,Count))
g + geom_bar(stat="identity", width = 0.5, fill="yellow") + 
      labs(title="Popular stations and trip ", 
           subtitle="What is the 10 most common start station? In NewYork
", 
           caption="Source: Udacity from 'bike' dataset") +
theme(axis.text.x = element_text(angle=65, vjust=0.6))


############################################################
#3 Trip duration
# 3-1 What is the total travel time for users in different cities?
n<-sum(ny[,4], na.rm = TRUE)
n<-sum(wash[,4], na.rm = TRUE)
n<-sum(chi[,4], na.rm = TRUE)
# 3-2 What is the average travel time for users in different cities?
m<-mean(ny[,4], na.rm = TRUE)
m1<-mean(wash[,4], na.rm = TRUE)
m2<-mean(chi[,4], na.rm = TRUE)
##############################################################
#4 User info
# 4-1 What are the counts of each user type?
occurresult<-table(factor(ny[,7], levels = 
c('Subscriber', 'Customer')))
occurresult1<-table(factor(wash[,7], levels = 
c('Subscriber', 'Customer')))
occurresult2<-table(factor(chi[,7], levels = 
c('Subscriber', 'Customer')))
#result<-c(occurresult,occurresult1,occurresult2)
for (i in 1:2){
occurresult[i]<-occurresult[i]+occurresult1[i]+occurresult2[i];
}
occurresult
##################################################################
#4 - 2 What are the counts of each gender (only available for NYC and Chicago)?

occurresult<-table(factor(ny[,8], levels = 
c('Male', 'Female')))
occurresult1<-table(factor(chi[,8], levels = 
c('Male', 'Female')))
for (i in 1:2){
occurresult[i]<-occurresult[i]+occurresult1[i];
}
occurresult
################################################################
#4-3 What are the earliest, most recent, most common year of birth (only available for NYC and Chicago)?
earliestyear=function(x,y){
res<-c(x,y)
occurresult<-min(res,na.rm = TRUE)
return (occurresult)
}
result<-earliestyear(ny[,9],chi[,9])
result
#################
recentyear=function(x,y){
res<-c(x,y)
occurresult<-max(res,na.rm = TRUE)
return (occurresult)
}
result<-recentyear(ny[,9],chi[,9])
result
##############
mostyear=function(x,y){
res<-c(x,y)
occurresult<-sort(table(res),decreasing=TRUE)[1:1]
return (occurresult)
}
result<-mostyear(ny[,9],chi[,9])
result


#3 Trip duration
# Plot
library(ggplot2)
theme_set(theme_classic())
qqq<-data.frame(city=c("NY","WASH","CHI"),duration=c(mean(ny[,4], na.rm = TRUE),mean(wash[,4], na.rm = TRUE),mean(chi[,4], na.rm = TRUE)))
g <- ggplot(qqq, aes(city,duration))
g + geom_bar(stat="identity", width = 0.5, fill="tomato2") + 
      labs(title="Trip duration ", 
           subtitle="What is the average travel time for users in different cities?
", 
           caption="Source: Udacity from 'bike' dataset") +
      theme(axis.text.x = element_text(angle=65, vjust=0.6))


#####################################################################
#4User info
occurresult<-table(factor(ny[,7], levels = 
c('Subscriber', 'Customer')))
occurresult1<-table(factor(wash[,7], levels = 
c('Subscriber', 'Customer')))
occurresult2<-table(factor(chi[,7], levels = 
c('Subscriber', 'Customer')))
#result<-c(occurresult,occurresult1,occurresult2)
for (i in 1:2){
occurresult[i]<-occurresult[i]+occurresult1[i]+occurresult2[i];
}
occurresult
qqq<-data.frame(occurresult)
names(qqq)[1] <- "Type"
names(qqq)[2] <- "Count"
g <- ggplot(qqq, aes(Type,Count/10000))
g + geom_bar(stat="identity", width = 0.5, fill="green") + 
      labs(title="User info", 
           subtitle="What are the counts of each user type?
", 
           caption="Source: Udacity from 'bike' dataset") +
      theme(axis.text.x = element_text(angle=65, vjust=0.6))


system('python -m nbconvert Explore_bikeshare_data.ipynb')
