library(foreign)
library(ggplot2)
library(data.table)
library(gmodels)

POPULATION.SIZE = 108
CURRENTYEAR <- 1900 + as.POSIXlt(Sys.Date())$year


data <- read.csv("~/Desktop/Methods Project/raportti-methods-openoff.csv")
View(data)

data <- data.frame(data)


######################################################################
# Preprocess
######################################################################

#Demographics
data$jobfunction <- factor(data$X1.1.Which.of.the.following.most.closely.matches.your.primary.job.function..,
                           levels = c(0:6),
                           labels = c("Developing software", "Testing software", "UX Design", "Management", "Operations", "Architecture", "Other"))
data$worktime <- data$X1.2.How.long.have.you.been.working.in.your.current.company.role.
data$birthyear <- data$X1.3.What.is.your.year.of.birth.
data$birthyear[31] <- data$birthyear[31] + 1900 # Fix data entry error
data$age <- CURRENTYEAR - data$birthyear

#2.1
data$useractivities.specifying.requirements <- data$Specifying.requirements
data$useractivities.designing.software <- data$Designing.software
data$useractivities.implementing.software <- data$Implementing.software
data$useractivities.testing <- data$Testing
data$useractivities.after.release <- data$The.activities.after.release
data$useractivities.other <- data$Other
data$useractivities.other.open <- data$If.other..please.specify..separate.with.commas.
useractivities.options <- c("Specifying requirements", "Designing software", "Implementing software", "Testing", "The activities after release", "Other")

#2.2

data$userinv.S1 <- data$X2.2..How.much.do.you.agree.with.the.following.statements...I.know.who.uses.the.software.I.contribute.to.in.my.work
data$userinv.S2 <- data$X2.2..How.much.do.you.agree.with.the.following.statements...I.need.to.ask.for.permission.to.contact.users
data$userinv.S3 <- data$X2.2..How.much.do.you.agree.with.the.following.statements...I.frequently.have.direct.contact.with.users
data$userinv.S4 <- data$X2.2..How.much.do.you.agree.with.the.following.statements...I.have.sufficient.information.about.users..needs
data$userinv.S5 <- data$X2.2..How.much.do.you.agree.with.the.following.statements...I.have.information.about.users.that.is.relevant.for.my.work
data$userinv.S6 <- data$X2.2..How.much.do.you.agree.with.the.following.statements...The.information.I.have.about.users.is.up.to.date
userinv.statements <- c(
  "I know who uses the software I contribute to in my work",
  "I need to ask for permission to contact users",
  "I frequently have direct contact with users",
  "I have sufficient information about users’ needs",
  "I have information about users that is relevant for my work",
  "The information I have about users is up to date"
)
userinv.options <- c("Completely disagree", "Disagree", "Neither disagree or agree", "Agree", "Completely agree", "I don't know")


######################################################################
# Analysis
######################################################################

attach(data)

# Job function
print("Primary job function")
summary(jobfunction)
ggplot(data, aes(x=jobfunction)) +
  geom_bar(fill="#FF9999", colour="#FF9999") +
  labs(x="Job function", y="Frequency")

# Work time
print("How long have you been working in your current role")
summary(worktime)
ggplot(data, aes(x=worktime)) +
  geom_histogram(binwidth=10) +
  labs(x="Work time", y="Frequency")

# Age
print("Age")
summary(age)
ggplot(data, aes(x=age)) +
  geom_histogram(binwidth=1) +
  labs(x="Age", y="Frequency")

#User activities
# 2.1 In which development activities are users involved in your company? (click all that apply)
useractivities.specifying.requirements.count <- sum(data$useractivities.specifying.requirements, na.rm=TRUE)
useractivities.designing.software.count <- sum(data$useractivities.designing.software, na.rm=TRUE)
useractivities.implementing.software <- sum(data$useractivities.implementing.software, na.rm=TRUE)
useractivities.testing <- sum(data$useractivities.testing, na.rm=TRUE)
useractivities.after.release <- sum(data$useractivities.after.release, na.rm=TRUE)
useractivities.other <- sum(data$useractivities.other, na.rm=TRUE)
useractivities <- data.frame(Activity=useractivities.options,
                             Frequency=c(
                               useractivities.specifying.requirements.count,
                               useractivities.designing.software.count,
                               useractivities.implementing.software,
                               useractivities.testing,
                               useractivities.after.release,
                               useractivities.other))

print("Frequencies of development activities that users are involved in")
summary(useractivities)
ggplot(data=useractivities, aes(x=Activity, y=Frequency)) +
  geom_bar(stat="identity", fill="#FF9999", colour="#FF9999")

#2.2
userinv <- data.frame(Statement=factor(rep(userinv.statements, each=length(userinv.S1))),
                      Rating=c(
                        userinv.S1,
                        userinv.S2,
                        userinv.S3,
                        userinv.S4,
                        userinv.S5,
                        userinv.S6))
ggplot(data=userinv, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot() + guides(fill=FALSE) + coord_flip()

########### Some cross-analysis #######
a<-table(jobfunction, age)
barplot(a, col=1:7, legend = rownames(a), main = "job function vs. age", las=1) 

ggplot(data,aes(x=jobfunction,y=age, fill=jobfunction))+geom_boxplot() + guides(fill=FALSE) + coord_flip() #"job function vs. age" 2

dd<-table(data$userinv.S2, jobfunction) # "I need permission" over job functions
ggplot(data,aes(x=jobfunction,y=data$userinv.S2, fill=jobfunction))+geom_boxplot() + guides(fill=FALSE) + coord_flip() + labs(x="Roles",y="'I need to ask permission to contact users'")

ggplot(data,aes(x=jobfunction,y=data$userinv.S3, fill=jobfunction))+geom_boxplot() + guides(fill=FALSE) + coord_flip() + labs(x="Roles",y="'I frequently have direct contact with users'") 
