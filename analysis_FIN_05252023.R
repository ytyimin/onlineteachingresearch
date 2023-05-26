# this is the program for the main anlaysis - will need to add interaction effect later

library(readxl)

# define working directory
workdir<-"D:/Dropbox (ASU)/research/reynold"
#workdir<-"C:/Users/ywang195/Documents/Dropbox (ASU)/research/bayesian lead time/r"

# set working directory
setwd(workdir)

#dat <- read_excel("DataConversionFile.xlsx",sheet = "Original Data")
#dat$QuantGradeC <- qgc
#dat_RAW$NumPrevCourse <- numPre

# define output file path and name
outputfile<-"dat_RAW.rds"

# save the raw data file
#saveRDS(dat, file=outputfile)

dat <- readRDS(file = "dat_RAW.rds")

# remove spaces
library(stringr)
names(dat)<-str_replace_all(names(dat), c(" " = "" , "," = "" ))

# make treatment a factor
dat$Treat <- ifelse(dat$Treatment=="Control", 1, ifelse(dat$Treatment=="camera", 3, ifelse(dat$Treatment=="chat-vote", 2, ifelse(dat$Treatment=="Full", 4,NA))))
dat$SchoolYearN <- ifelse(dat$SchoolYear=="Sophomore", 1, ifelse(dat$SchoolYear=="Junior", 2, 3))
dat$PrevCourse01 <- ifelse(dat$PrevCourseYN=="No", 0, ifelse(dat$PrevCourseYN=="Yes", 1,0))
dat$EngagedN <- ifelse(dat$Engaged=="Strongly Disagree", 1,ifelse(dat$Engaged=="Disagree", 2, ifelse(dat$Engaged=="Neither agree or disagree",3, ifelse(dat$Engaged=="Agree", 4, 5))))
dat$UnderstandingN <- ifelse(dat$Understanding=="Strongly Disagree", 1,ifelse(dat$Understanding=="Disagree", 2, ifelse(dat$Understanding=="Neither agree or disagree",3, ifelse(dat$Understanding=="Agree", 4, 5))))
dat$EnjoyedN <- ifelse(dat$Enjoyed=="Strongly Disagree", 1,ifelse(dat$Enjoyed=="Disagree", 2, ifelse(dat$Enjoyed=="Neither agree or disagree",3, ifelse(dat$Enjoyed=="Agree", 4, 5))))
dat$FeelQuizN <- ifelse(dat$FeelQuiz=="Strongly Disagree", 1,ifelse(dat$FeelQuiz=="Disagree", 2, ifelse(dat$FeelQuiz=="Neither agree or disagree",3, ifelse(dat$FeelQuiz=="Agree", 4, 5))))
dat$QuantGradeN <- ifelse(dat$QuantGradeC=="StrongQuant", 3, ifelse(dat$QuantGradeC=="MedQuant", 2, 1))

#dat$Treatment <- factor(dat$Treatment, levels=c("Control","camera","chat-vote","Full"))
#dat <- subset(dat, select = -c(Treatment))
#summary(dat)

##############################################################################
#  imputing missing values
##############################################################################


# imputation of missing values
library(VIM)
dat_IMPUTED <- kNN(dat)


##############################################################################
#  do coarsened matching
##############################################################################


# coarsened match
library(cem)
vars.mat <- c("ZoomLearn", "QuantGrade", "PrevCourseYN", "Gender","Age","SchoolYear")
imbalance(group = dat_IMPUTED$Treat, data=dat_IMPUTED[vars.mat])

vars.drop <- c("Treatment","RandomID","EOQQuiz","PrevCourse","Engaged","Understanding","Enjoyed","FeelQuiz","EngagedN","UnderstandingN","EnjoyedN","FeelQuizN","Course","SchoolYearN", "QuantGradeC", "QuantGradeN", "NumPrevCourse")
mat <- cem(treatment="Treat", data = dat_IMPUTED, drop = vars.drop)
mat

mat1 <- cem(treatment="Treat", data = dat_IMPUTED, drop = vars.drop, eval.imbalance = TRUE, verbose = 0)
mat1


dat_matched <- dat
dat_matched$weight <- mat[["w"]]
dat_dropped <- dat_matched[ which(dat_matched$weight<=0), ]
saveRDS(dat_dropped, file="dat_dropped")
#library(xlsx)
#write.xlsx(dat_dropped, "dat_dropped.xlsx", sheetName="Observations Dropped")
#write.xlsx(dat_matched, "dat_weight.xlsx", sheetName="Data with weights")


##############################################################################
#  look at correlations
##############################################################################

#dat_matched <- dat_IMPUTED
#dat_matched$weight <- mat[["w"]]
#dat_remain <- dat_matched[ which(dat_matched$weight>0), ]
#datCor <- dat_remain[,c(2,4,10,16,20,21,22,23)]
datCor <- dat[,c(2,4,10,16,20,21,22,23)]
tblCor <- cor(datCor, use = "pairwise.complete.obs")
write.csv(tblCor,"tblCor.csv")
tblCor

library("Hmisc")
res <- rcorr(as.matrix(datCor))
res


##############################################################################
#  look at general descriptive
##############################################################################

library(pastecs)
stat.desc(datCor)


##############################################################################
#  check average treatment effect
##############################################################################


est <- att(mat, EOQQuiz ~ Treat, data = as.data.frame(dat_IMPUTED), model="forest")
est

#est <- att(mat, EOQQuiz ~ Treat + ZoomLearn + QuantGrade + PrevCourseYN + Gender + Age + SchoolYear + Engaged + Understanding + Enjoyed + FeelQuiz, data = as.data.frame(dat_IMPUTED), model="forest")
#est


# factorize Treatment variable
dat$Treatment <- factor(dat$Treatment, levels=c("Control","camera","chat-vote","Full"))
dat_IMPUTED$Treatment <- factor(dat_IMPUTED$Treatment, levels=c("Control","camera","chat-vote","Full"))

# group quantGrade
#dat$QuantGrade <- ifelse(dat$QuantGrade %in% c("C-","C","C+"), "C", ifelse(dat$QuantGrade %in% c("B-","B","B+"), "B", ifelse(dat$QuantGrade %in% c("A-","A","A+"),"A","B")))
#dat_IMPUTED$QuantGrade <- ifelse(dat_IMPUTED$QuantGrade %in% c("C-","C","C+"), "C", ifelse(dat_IMPUTED$QuantGrade %in% c("B-","B","B+"), "B", ifelse(dat_IMPUTED$QuantGrade %in% c("A-","A","A+"),"A","B")))


##############################################################################
#  test which model is most parsimonious
##############################################################################



# full models
# raw
mod.raw.full <- lm(formula = EOQQuiz ~ Treatment + ZoomLearn + NumPrevCourse + Age + EngagedN + UnderstandingN + EnjoyedN + FeelQuizN, data = dat)
# imputed
mod.impute.full <- lm(formula = EOQQuiz ~ Treatment + ZoomLearn + NumPrevCourse + Age + EngagedN + UnderstandingN + EnjoyedN + FeelQuizN, data = dat_IMPUTED)
# weighted by coarsen match raw
dat$Treat2 <- dat$Treat^2
#mod.raw.weighted.full <- lm(formula = EOQQuiz ~ Treat + Treat2 + ZoomLearn + NumPrevCourse + Age + EngagedN + UnderstandingN + EnjoyedN + FeelQuizN, weights=mat[["w"]], data = dat)
mod.raw.weighted.full <- lm(formula = EOQQuiz ~ Treatment + ZoomLearn + NumPrevCourse + Age + EngagedN + UnderstandingN + EnjoyedN + FeelQuizN, weights=mat[["w"]], data = dat)
# weighted by coarsen match raw
mod.impute.weighted.full <- lm(formula = EOQQuiz ~ Treatment + ZoomLearn + NumPrevCourse + Age + EngagedN + UnderstandingN + EnjoyedN + FeelQuizN, weights=mat[["w"]], data = dat_IMPUTED)

# partial models
# raw
mod.raw.partial <- lm(formula = EOQQuiz ~ Treatment + ZoomLearn, data = dat)
# imputed
mod.impute.partial <- lm(formula = EOQQuiz ~ Treatment + ZoomLearn, data = dat_IMPUTED)
# weighted by coarsen match raw
mod.raw.weighted.partial <- lm(formula = EOQQuiz ~ Treatment + ZoomLearn, weights=mat[["w"]], data = dat)
# weighted by coarsen match raw
mod.impute.weighted.partial <- lm(formula = EOQQuiz ~ Treatment + ZoomLearn, weights=mat[["w"]], data = dat_IMPUTED)

# bare models
# raw
mod.raw.base <- lm(formula = EOQQuiz ~ Treatment, data = dat)
# imputed
mod.impute.base <- lm(formula = EOQQuiz ~ Treatment, data = dat_IMPUTED)
# weighted by coarsen match raw
mod.raw.weighted.base <- lm(formula = EOQQuiz ~ Treatment, weights=mat[["w"]], data = dat)
# weighted by coarsen match raw
mod.impute.weighted.base <- lm(formula = EOQQuiz ~ Treatment, weights=mat[["w"]], data = dat_IMPUTED)


library(AICcmodavg)
model.set <- list(mod.raw.full,mod.impute.full,mod.raw.weighted.full,mod.impute.weighted.full,mod.raw.partial,mod.impute.partial,mod.raw.weighted.partial,mod.impute.weighted.partial,mod.raw.base,mod.impute.base,mod.raw.weighted.base,mod.impute.weighted.base)
model.names <- c("mod.raw.full","mod.impute.full","mod.raw.weighted.full","mod.impute.weighted.full","mod.raw.partial","mod.impute.partial","mod.raw.weighted.partial","mod.impute.weighted.partial","mod.raw.base","mod.impute.base","mod.raw.weighted.base","mod.impute.weighted.base")
#anova(mod.raw.full,mod.impute.full,mod.raw.weighted.full,mod.impute.weighted.full,mod.raw.partial,mod.impute.partial,mod.raw.weighted.partial,mod.impute.weighted.partial)
aictab(model.set, modnames = model.names)
#aictab(model.set)


##############################################################################
#  select raw weighted full model and display its estimation results
##############################################################################


#summary(mod.raw.weighted.partial)
#summary(mod.impute.weighted.partial)
summary(mod.raw.weighted.full)
#summary(mod.raw.full)


##############################################################################
#  test whether the two effects are equivalent between camera on and chat-vote
##############################################################################



library(car)
linearHypothesis(mod.raw.weighted.full, "Treatmentcamera = Treatmentchat-vote")


##############################################################################
#  check mediation effect - overall
##############################################################################


# check mediation effect. To do so, we re-setup three sets of models 
# and use the R package to estimate the mediation effect

# first we do an overall mediation test

dat$PerceptionN <- (dat$EngagedN+dat$UnderstandingN+dat$EnjoyedN+dat$FeelQuizN)/4.0
dat_IMPUTED$PerceptionN <- (dat_IMPUTED$EngagedN+dat_IMPUTED$UnderstandingN+dat_IMPUTED$EnjoyedN+dat_IMPUTED$FeelQuizN)/4.0

# full models
# raw
mod.raw.full <- lm(formula = EOQQuiz ~ Treatment + ZoomLearn + NumPrevCourse + Age + PerceptionN, data = dat)
# imputed
mod.impute.full <- lm(formula = EOQQuiz ~ Treatment + ZoomLearn + NumPrevCourse + Age + PerceptionN, data = dat_IMPUTED)
# weighted by coarsen match raw
mod.raw.weighted.full <- lm(formula = EOQQuiz ~ Treatment + ZoomLearn + NumPrevCourse + Age + PerceptionN, weights=mat[["w"]], data = dat)
# weighted by coarsen match raw
mod.impute.weighted.full <- lm(formula = EOQQuiz ~ Treatment + ZoomLearn + NumPrevCourse + Age + PerceptionN, weights=mat[["w"]], data = dat_IMPUTED)

# direct models
# raw
mod.raw.dir <- lm(formula = EOQQuiz ~ Treatment + ZoomLearn + NumPrevCourse + Age, data = dat)
# imputed
mod.impute.dir <- lm(formula = EOQQuiz ~ Treatment + ZoomLearn + NumPrevCourse + Age, data = dat_IMPUTED)
# weighted by coarsen match raw
mod.raw.weighted.dir <- lm(formula = EOQQuiz ~ Treatment + ZoomLearn + NumPrevCourse + Age, weights=mat[["w"]], data = dat)
# weighted by coarsen match raw
mod.impute.weighted.dir <- lm(formula = EOQQuiz ~ Treatment + ZoomLearn + NumPrevCourse + Age, weights=mat[["w"]], data = dat_IMPUTED)

# raw
mod.raw.med <- lm(formula = PerceptionN ~ Treatment + ZoomLearn + NumPrevCourse + Age, data = dat)
# imputed
mod.impute.med <- lm(formula = PerceptionN ~ Treatment + ZoomLearn + NumPrevCourse + Age, data = dat_IMPUTED)
# weighted by coarsen match raw
mod.raw.weighted.med <- lm(formula = PerceptionN ~ Treatment + ZoomLearn + NumPrevCourse + Age, weights=mat[["w"]], data = dat)
# weighted by coarsen match raw
mod.impute.weighted.med <- lm(formula = PerceptionN ~ Treatment + ZoomLearn + NumPrevCourse + Age, weights=mat[["w"]], data = dat_IMPUTED)

library(mediation)
results = mediate(mod.raw.weighted.med, mod.raw.weighted.full, treat='Treatment', mediator='PerceptionN', boot=T)
summary(results)


##############################################################################
#  check mediation effect - EngagedN
##############################################################################


# full models
# raw
mod.raw.full <- lm(formula = EOQQuiz ~ Treatment + ZoomLearn + NumPrevCourse + Age + EngagedN + UnderstandingN + EnjoyedN + FeelQuizN, data = dat)
# imputed
mod.impute.full <- lm(formula = EOQQuiz ~ Treatment + ZoomLearn + NumPrevCourse + Age + EngagedN + UnderstandingN + EnjoyedN + FeelQuizN, data = dat_IMPUTED)
# weighted by coarsen match raw
mod.raw.weighted.full <- lm(formula = EOQQuiz ~ Treatment + ZoomLearn + NumPrevCourse + Age + EngagedN + UnderstandingN + EnjoyedN + FeelQuizN, weights=mat[["w"]], data = dat)
# weighted by coarsen match raw
mod.impute.weighted.full <- lm(formula = EOQQuiz ~ Treatment + ZoomLearn + NumPrevCourse + Age + EngagedN + UnderstandingN + EnjoyedN + FeelQuizN, weights=mat[["w"]], data = dat_IMPUTED)

# direct models
# raw
mod.raw.dir <- lm(formula = EOQQuiz ~ Treatment + ZoomLearn + NumPrevCourse + Age, data = dat)
# imputed
mod.impute.dir <- lm(formula = EOQQuiz ~ Treatment + ZoomLearn + NumPrevCourse + Age, data = dat_IMPUTED)
# weighted by coarsen match raw
mod.raw.weighted.dir <- lm(formula = EOQQuiz ~ Treatment + ZoomLearn + NumPrevCourse + Age, weights=mat[["w"]], data = dat)
# weighted by coarsen match raw
mod.impute.weighted.dir <- lm(formula = EOQQuiz ~ Treatment + ZoomLearn + NumPrevCourse + Age, weights=mat[["w"]], data = dat_IMPUTED)

# raw
mod.raw.med <- lm(formula = EngagedN ~ Treatment + ZoomLearn + NumPrevCourse + Age, data = dat)
# imputed
mod.impute.med <- lm(formula = EngagedN ~ Treatment + ZoomLearn + NumPrevCourse + Age, data = dat_IMPUTED)
# weighted by coarsen match raw
mod.raw.weighted.med <- lm(formula = EngagedN ~ Treatment + ZoomLearn + NumPrevCourse + Age, weights=mat[["w"]], data = dat)
# weighted by coarsen match raw
mod.impute.weighted.med <- lm(formula = EngagedN ~ Treatment + ZoomLearn + NumPrevCourse + Age, weights=mat[["w"]], data = dat_IMPUTED)

library(mediation)
results = mediate(mod.raw.weighted.med, mod.raw.weighted.full, treat='Treatment', mediator='EngagedN', boot=T)
summary(results)


##############################################################################
#  check mediation effect - UnderstandingN
##############################################################################


# full models
# raw
mod.raw.full <- lm(formula = EOQQuiz ~ Treatment + ZoomLearn + NumPrevCourse + Age + EngagedN + UnderstandingN + EnjoyedN + FeelQuizN, data = dat)
# imputed
mod.impute.full <- lm(formula = EOQQuiz ~ Treatment + ZoomLearn + NumPrevCourse + Age + EngagedN + UnderstandingN + EnjoyedN + FeelQuizN, data = dat_IMPUTED)
# weighted by coarsen match raw
mod.raw.weighted.full <- lm(formula = EOQQuiz ~ Treatment + ZoomLearn + NumPrevCourse + Age + EngagedN + UnderstandingN + EnjoyedN + FeelQuizN, weights=mat[["w"]], data = dat)
# weighted by coarsen match raw
mod.impute.weighted.full <- lm(formula = EOQQuiz ~ Treatment + ZoomLearn + NumPrevCourse + Age + EngagedN + UnderstandingN + EnjoyedN + FeelQuizN, weights=mat[["w"]], data = dat_IMPUTED)

# direct models
# raw
mod.raw.dir <- lm(formula = EOQQuiz ~ Treatment + ZoomLearn + NumPrevCourse + Age, data = dat)
# imputed
mod.impute.dir <- lm(formula = EOQQuiz ~ Treatment + ZoomLearn + NumPrevCourse + Age, data = dat_IMPUTED)
# weighted by coarsen match raw
mod.raw.weighted.dir <- lm(formula = EOQQuiz ~ Treatment + ZoomLearn + NumPrevCourse + Age, weights=mat[["w"]], data = dat)
# weighted by coarsen match raw
mod.impute.weighted.dir <- lm(formula = EOQQuiz ~ Treatment + ZoomLearn + NumPrevCourse + Age, weights=mat[["w"]], data = dat_IMPUTED)

# raw
mod.raw.med <- lm(formula = UnderstandingN ~ Treatment + ZoomLearn + NumPrevCourse + Age, data = dat)
# imputed
mod.impute.med <- lm(formula = UnderstandingN ~ Treatment + ZoomLearn + NumPrevCourse + Age, data = dat_IMPUTED)
# weighted by coarsen match raw
mod.raw.weighted.med <- lm(formula = UnderstandingN ~ Treatment + ZoomLearn + NumPrevCourse + Age, weights=mat[["w"]], data = dat)
# weighted by coarsen match raw
mod.impute.weighted.med <- lm(formula = UnderstandingN ~ Treatment + ZoomLearn + NumPrevCourse + Age, weights=mat[["w"]], data = dat_IMPUTED)

library(mediation)
results = mediate(mod.raw.weighted.med, mod.raw.weighted.full, treat='Treatment', mediator='UnderstandingN', boot=T)
summary(results)



##############################################################################
#  check mediation effect - EnjoyedN
##############################################################################

# full models
# raw
mod.raw.full <- lm(formula = EOQQuiz ~ Treatment + ZoomLearn + NumPrevCourse + Age + EngagedN + UnderstandingN + EnjoyedN + FeelQuizN, data = dat)
# imputed
mod.impute.full <- lm(formula = EOQQuiz ~ Treatment + ZoomLearn + NumPrevCourse + Age + EngagedN + UnderstandingN + EnjoyedN + FeelQuizN, data = dat_IMPUTED)
# weighted by coarsen match raw
mod.raw.weighted.full <- lm(formula = EOQQuiz ~ Treatment + ZoomLearn + NumPrevCourse + Age + EngagedN + UnderstandingN + EnjoyedN + FeelQuizN, weights=mat[["w"]], data = dat)
# weighted by coarsen match raw
mod.impute.weighted.full <- lm(formula = EOQQuiz ~ Treatment + ZoomLearn + NumPrevCourse + Age + EngagedN + UnderstandingN + EnjoyedN + FeelQuizN, weights=mat[["w"]], data = dat_IMPUTED)

# direct models
# raw
mod.raw.dir <- lm(formula = EOQQuiz ~ Treatment + ZoomLearn + NumPrevCourse + Age, data = dat)
# imputed
mod.impute.dir <- lm(formula = EOQQuiz ~ Treatment + ZoomLearn + NumPrevCourse + Age, data = dat_IMPUTED)
# weighted by coarsen match raw
mod.raw.weighted.dir <- lm(formula = EOQQuiz ~ Treatment + ZoomLearn + NumPrevCourse + Age, weights=mat[["w"]], data = dat)
# weighted by coarsen match raw
mod.impute.weighted.dir <- lm(formula = EOQQuiz ~ Treatment + ZoomLearn + NumPrevCourse + Age, weights=mat[["w"]], data = dat_IMPUTED)

# raw
mod.raw.med <- lm(formula = EnjoyedN ~ Treatment + ZoomLearn + NumPrevCourse + Age, data = dat)
# imputed
mod.impute.med <- lm(formula = EnjoyedN ~ Treatment + ZoomLearn + NumPrevCourse + Age, data = dat_IMPUTED)
# weighted by coarsen match raw
mod.raw.weighted.med <- lm(formula = EnjoyedN ~ Treatment + ZoomLearn + NumPrevCourse + Age, weights=mat[["w"]], data = dat)
# weighted by coarsen match raw
mod.impute.weighted.med <- lm(formula = EnjoyedN ~ Treatment + ZoomLearn + NumPrevCourse + Age, weights=mat[["w"]], data = dat_IMPUTED)

library(mediation)
results = mediate(mod.raw.weighted.med, mod.raw.weighted.full, treat='Treatment', mediator='EngagedN', boot=T)
summary(results)



##############################################################################
#  check mediation effect - FeelQuizN
##############################################################################


# full models
# raw
mod.raw.full <- lm(formula = EOQQuiz ~ Treatment + ZoomLearn + NumPrevCourse + Age + EngagedN + UnderstandingN + EnjoyedN + FeelQuizN, data = dat)
# imputed
mod.impute.full <- lm(formula = EOQQuiz ~ Treatment + ZoomLearn + NumPrevCourse + Age + EngagedN + UnderstandingN + EnjoyedN + FeelQuizN, data = dat_IMPUTED)
# weighted by coarsen match raw
mod.raw.weighted.full <- lm(formula = EOQQuiz ~ Treatment + ZoomLearn + NumPrevCourse + Age + EngagedN + UnderstandingN + EnjoyedN + FeelQuizN, weights=mat[["w"]], data = dat)
# weighted by coarsen match raw
mod.impute.weighted.full <- lm(formula = EOQQuiz ~ Treatment + ZoomLearn + NumPrevCourse + Age + EngagedN + UnderstandingN + EnjoyedN + FeelQuizN, weights=mat[["w"]], data = dat_IMPUTED)

# direct models
# raw
mod.raw.dir <- lm(formula = EOQQuiz ~ Treatment + ZoomLearn + NumPrevCourse + Age, data = dat)
# imputed
mod.impute.dir <- lm(formula = EOQQuiz ~ Treatment + ZoomLearn + NumPrevCourse + Age, data = dat_IMPUTED)
# weighted by coarsen match raw
mod.raw.weighted.dir <- lm(formula = EOQQuiz ~ Treatment + ZoomLearn + NumPrevCourse + Age, weights=mat[["w"]], data = dat)
# weighted by coarsen match raw
mod.impute.weighted.dir <- lm(formula = EOQQuiz ~ Treatment + ZoomLearn + NumPrevCourse + Age, weights=mat[["w"]], data = dat_IMPUTED)

# raw
mod.raw.med <- lm(formula = FeelQuizN ~ Treatment + ZoomLearn + NumPrevCourse + Age, data = dat)
# imputed
mod.impute.med <- lm(formula = FeelQuizN ~ Treatment + ZoomLearn + NumPrevCourse + Age, data = dat_IMPUTED)
# weighted by coarsen match raw
mod.raw.weighted.med <- lm(formula = FeelQuizN ~ Treatment + ZoomLearn + NumPrevCourse + Age, weights=mat[["w"]], data = dat)
# weighted by coarsen match raw
mod.impute.weighted.med <- lm(formula = FeelQuizN ~ Treatment + ZoomLearn + NumPrevCourse + Age, weights=mat[["w"]], data = dat_IMPUTED)

library(mediation)
results = mediate(mod.raw.weighted.med, mod.raw.weighted.full, treat='Treatment', mediator='FeelQuizN', boot=T)
summary(results)



##############################################################################
#  Now assume treatment is coded as 1,2,3,4 and test which model is most parsimonious
##############################################################################


# full models
# raw
mod.raw.full <- lm(formula = EOQQuiz ~ Treat + ZoomLearn + NumPrevCourse + Age + EngagedN + UnderstandingN + EnjoyedN + FeelQuizN, data = dat)
# imputed
mod.impute.full <- lm(formula = EOQQuiz ~ Treat + ZoomLearn + NumPrevCourse + Age + EngagedN + UnderstandingN + EnjoyedN + FeelQuizN, data = dat_IMPUTED)
# weighted by coarsen match raw
dat$Treat2 <- dat$Treat^2
#mod.raw.weighted.full <- lm(formula = EOQQuiz ~ Treat + Treat2 + ZoomLearn + NumPrevCourse + Age + EngagedN + UnderstandingN + EnjoyedN + FeelQuizN, weights=mat[["w"]], data = dat)
mod.raw.weighted.full <- lm(formula = EOQQuiz ~ Treat + ZoomLearn + NumPrevCourse + Age + EngagedN + UnderstandingN + EnjoyedN + FeelQuizN, weights=mat[["w"]], data = dat)
# weighted by coarsen match raw
mod.impute.weighted.full <- lm(formula = EOQQuiz ~ Treat + ZoomLearn + NumPrevCourse + Age + EngagedN + UnderstandingN + EnjoyedN + FeelQuizN, weights=mat[["w"]], data = dat_IMPUTED)

# partial models
# raw
mod.raw.partial <- lm(formula = EOQQuiz ~ Treat + ZoomLearn, data = dat)
# imputed
mod.impute.partial <- lm(formula = EOQQuiz ~ Treat + ZoomLearn, data = dat_IMPUTED)
# weighted by coarsen match raw
mod.raw.weighted.partial <- lm(formula = EOQQuiz ~ Treat + ZoomLearn, weights=mat[["w"]], data = dat)
# weighted by coarsen match raw
mod.impute.weighted.partial <- lm(formula = EOQQuiz ~ Treat + ZoomLearn, weights=mat[["w"]], data = dat_IMPUTED)

# bare models
# raw
mod.raw.base <- lm(formula = EOQQuiz ~ Treat, data = dat)
# imputed
mod.impute.base <- lm(formula = EOQQuiz ~ Treat, data = dat_IMPUTED)
# weighted by coarsen match raw
mod.raw.weighted.base <- lm(formula = EOQQuiz ~ Treat, weights=mat[["w"]], data = dat)
# weighted by coarsen match raw
mod.impute.weighted.base <- lm(formula = EOQQuiz ~ Treat, weights=mat[["w"]], data = dat_IMPUTED)


library(AICcmodavg)
model.set <- list(mod.raw.full,mod.impute.full,mod.raw.weighted.full,mod.impute.weighted.full,mod.raw.partial,mod.impute.partial,mod.raw.weighted.partial,mod.impute.weighted.partial,mod.raw.base,mod.impute.base,mod.raw.weighted.base,mod.impute.weighted.base)
model.names <- c("mod.raw.full","mod.impute.full","mod.raw.weighted.full","mod.impute.weighted.full","mod.raw.partial","mod.impute.partial","mod.raw.weighted.partial","mod.impute.weighted.partial","mod.raw.base","mod.impute.base","mod.raw.weighted.base","mod.impute.weighted.base")
#anova(mod.raw.full,mod.impute.full,mod.raw.weighted.full,mod.impute.weighted.full,mod.raw.partial,mod.impute.partial,mod.raw.weighted.partial,mod.impute.weighted.partial)
aictab(model.set, modnames = model.names)
#aictab(model.set)


##############################################################################
#  select raw weighted full model and display its estimation results
##############################################################################


#summary(mod.raw.weighted.partial)
#summary(mod.impute.weighted.partial)
summary(mod.raw.weighted.full)
#summary(mod.raw.full)



##############################################################################
#  Now add quadratic effect while assume treatment is coded as 1,2,3,4 and test which model is most parsimonious
##############################################################################

dat$Treat2 <- dat$Treat^2
dat_IMPUTED$Treat2 <- dat_IMPUTED$Treat^2

# full models
# raw
mod.raw.full <- lm(formula = EOQQuiz ~ Treat + Treat2 + ZoomLearn + NumPrevCourse + Age + EngagedN + UnderstandingN + EnjoyedN + FeelQuizN, data = dat)
# imputed
mod.impute.full <- lm(formula = EOQQuiz ~ Treat + Treat2 + ZoomLearn + NumPrevCourse + Age + EngagedN + UnderstandingN + EnjoyedN + FeelQuizN, data = dat_IMPUTED)
# weighted by coarsen match raw
dat$Treat2 <- dat$Treat^2
#mod.raw.weighted.full <- lm(formula = EOQQuiz ~ Treat + Treat2 + ZoomLearn + NumPrevCourse + Age + EngagedN + UnderstandingN + EnjoyedN + FeelQuizN, weights=mat[["w"]], data = dat)
mod.raw.weighted.full <- lm(formula = EOQQuiz ~ Treat + Treat2 + ZoomLearn + NumPrevCourse + Age + EngagedN + UnderstandingN + EnjoyedN + FeelQuizN, weights=mat[["w"]], data = dat)
# weighted by coarsen match raw
mod.impute.weighted.full <- lm(formula = EOQQuiz ~ Treat + Treat2 + ZoomLearn + NumPrevCourse + Age + EngagedN + UnderstandingN + EnjoyedN + FeelQuizN, weights=mat[["w"]], data = dat_IMPUTED)

# partial models
# raw
mod.raw.partial <- lm(formula = EOQQuiz ~ Treat + Treat2 + ZoomLearn, data = dat)
# imputed
mod.impute.partial <- lm(formula = EOQQuiz ~ Treat + Treat2 + ZoomLearn, data = dat_IMPUTED)
# weighted by coarsen match raw
mod.raw.weighted.partial <- lm(formula = EOQQuiz ~ Treat + Treat2 + ZoomLearn, weights=mat[["w"]], data = dat)
# weighted by coarsen match raw
mod.impute.weighted.partial <- lm(formula = EOQQuiz ~ Treat + Treat2 + ZoomLearn, weights=mat[["w"]], data = dat_IMPUTED)

# bare models
# raw
mod.raw.base <- lm(formula = EOQQuiz ~ Treat + Treat2, data = dat)
# imputed
mod.impute.base <- lm(formula = EOQQuiz ~ Treat + Treat2, data = dat_IMPUTED)
# weighted by coarsen match raw
mod.raw.weighted.base <- lm(formula = EOQQuiz ~ Treat + Treat2, weights=mat[["w"]], data = dat)
# weighted by coarsen match raw
mod.impute.weighted.base <- lm(formula = EOQQuiz ~ Treat + Treat2, weights=mat[["w"]], data = dat_IMPUTED)


library(AICcmodavg)
model.set <- list(mod.raw.full,mod.impute.full,mod.raw.weighted.full,mod.impute.weighted.full,mod.raw.partial,mod.impute.partial,mod.raw.weighted.partial,mod.impute.weighted.partial,mod.raw.base,mod.impute.base,mod.raw.weighted.base,mod.impute.weighted.base)
model.names <- c("mod.raw.full","mod.impute.full","mod.raw.weighted.full","mod.impute.weighted.full","mod.raw.partial","mod.impute.partial","mod.raw.weighted.partial","mod.impute.weighted.partial","mod.raw.base","mod.impute.base","mod.raw.weighted.base","mod.impute.weighted.base")
#anova(mod.raw.full,mod.impute.full,mod.raw.weighted.full,mod.impute.weighted.full,mod.raw.partial,mod.impute.partial,mod.raw.weighted.partial,mod.impute.weighted.partial)
aictab(model.set, modnames = model.names)
#aictab(model.set)


##############################################################################
#  select raw weighted full model and display its estimation results
##############################################################################


#summary(mod.raw.weighted.partial)
#summary(mod.impute.weighted.partial)
summary(mod.raw.weighted.full)
#summary(mod.raw.full)


























summary(mod.raw.weighted.full)
summary(mod.raw.weighted.dir)
summary(mod.raw.weighted.med)

















# check manova for self assesment
library(DescTools)
# anova analysis without grouping


library(dplyr)
dat$ENG <- as.numeric(recode(dat$Engaged, "Strongly Disagree" = "1", "Disagree" = "2", "Neither agree or disagree" = "3", "Agree" = "4", "Strongly Agree" = "5"))
dat$STD <- as.numeric(recode(dat$Understanding, "Strongly Disagree" = "1", "Disagree" = "2", "Neither agree or disagree" = "3", "Agree" = "4", "Strongly Agree" = "5"))
dat$ENJ <- as.numeric(recode(dat$Enjoyed, "Strongly Disagree" = "1", "Disagree" = "2", "Neither agree or disagree" = "3", "Agree" = "4", "Strongly Agree" = "5"))
dat$FEL <- as.numeric(recode(dat$FeelQuiz, "Strongly Disagree" = "1", "Disagree" = "2", "Neither agree or disagree" = "3", "Agree" = "4", "Strongly Agree" = "5"))

# check manova analysis

dependent.vars <- cbind(dat$ENG,dat$STD,dat$ENJ,dat$FEL)
res.manova <- manova(dependent.vars ~ dat$Treatment)
summary(res.manova)


dependent.vars <- cbind(dat$ENG,dat$ENJ)
manova(dependent.vars ~ dat$Treatment)
res.manova <- manova(dependent.vars ~ dat$Treatment)
summary(res.manova)

summary(aov(dat$ENG ~ dat$Treat))

par(mfrow=c(2,2))
hist(dat$ENG)
hist(dat$STD)
hist(dat$ENJ)
hist(dat$FEL)

summary(one.engaged)
EtaSq(one.engaged)  

summary(one.understanding)
EtaSq(one.understanding)  

summary(one.enjoyed)
EtaSq(one.enjoyed)  

summary(one.feel)
EtaSq(one.feel)  

# Taking four dependent variable
engaged <- as.numeric(recode(dat$Engaged, "Strongly Disagree" = "1", "Disagree" = "2", "Neither agree or disagree" = "3", "Agree" = "4", "Strongly Agree" = "5"))
understand <- as.numeric(recode(dat$Understanding, "Strongly Disagree" = "1", "Disagree" = "2", "Neither agree or disagree" = "3", "Agree" = "4", "Strongly Agree" = "5"))
enjoyed <- as.numeric(recode(dat$Enjoyed, "Strongly Disagree" = "1", "Disagree" = "2", "Neither agree or disagree" = "3", "Agree" = "4", "Strongly Agree" = "5"))
feel <- as.numeric(recode(dat$FeelQuiz, "Strongly Disagree" = "1", "Disagree" = "2", "Neither agree or disagree" = "3", "Agree" = "4", "Strongly Agree" = "5"))

# MANOVA test
result = manova(cbind(engaged, understand, enjoyed, feel) ~ Treatment, data = dat)
summary(result)

# ANOVA test
result1 = aov(EOQQuiz ~ understand, data = dat)
result2 = aov(EOQQuiz ~ understand + feel, data = dat)
result3 = aov(EOQQuiz ~ understand + feel + engaged, data = dat)
result4 = aov(EOQQuiz ~ understand + feel + engaged + enjoyed , data = dat)

summary(result2)

library(AICcmodavg)
model.set <- list(result1,result2,result3,result4)
#model.names <- c("one.way", "blocking", "all")
#aictab(model.set, modnames = model.names)
aictab(model.set)

# check correlation/interaction
CramerV(dat$SchoolYear,dat$Course)
Phi(dat$SchoolYear,dat$Course)
CramerV(dat$QuantGrade,dat$PrevCourseYN)
