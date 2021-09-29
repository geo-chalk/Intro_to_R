library(foreign)
library(sjPlot)
library(sjmisc)
library(psych)
library(GeneNet)
list.files(".")

# 1. Load the data from the SPSS file.
df <- read.spss("./salary.sav", to.data.frame=TRUE)
describe(df)
summary(df)

# 2. Run a frequency table for "gender". Report how many men are in the sample. 
# Which type of graph would best display data for a variable like gender?
frq(df$sex, title = "Gender", out = "v")
sprintf("The total number of men is: %d", sum(df$sex == 'MALES'))
gender_freq <- table(df$sex)
png("Plots/2_gender_barplot.png")
barplot(gender_freq, main = "Gender Frequency",col = c(4,2), axes = TRUE, border = TRUE)
dev.off()

# 3. Run a frequency table for "jobcat". 
# Report the most common job category and its percentage.
frq(df$jobcat,title = "Job Category", out = "v")
sprintf("The total number of men is: %d", sum(df$sex == 'MALES'))

# 4. Make a pie chart for "jobcat" 
png("Plots/rplot.png")
x <- 100*table(df$jobcat)/length(df$jobcat) 
pie(x, label=paste( round(x,1), '%', sep='') , col=2:8) 
legend(0.8, 1, names(jobcat_freq), cex = 0.7, fill = 2:8)
dev.off()

# 5. report the percentage of people who are either clerical workers or security officers
cler_sec <- sum((df$jobcat == 'CLERICAL')|(df$jobcat == 'SECURITY OFFICER'))/length(df$jobcat)
sprintf("The percetage of people who are wither clerical workers or security officers is: %05.2f%%", cler_sec*100)

# 6. Run a frequency distribution for “salnow”
frq(df$salnow, title="salary", out = "v", auto.grp = 10)
hist(df$salnow, breaks=nclass.Sturges(df$salnow), xlab = "Salary", main= "Histogram of Salary Now")

# 7. Report the highest salary and how many people make that salary
max(df$salnow)
sum(df$salnow == max(df$salnow))
df[df$salnow == max(df$salnow),]

# 8. Report the mean and median salary
mean(df$salnow)
median(df$salnow)

# 9. What is the salary of the 10% highest paid employees in this bank?
quantile(df$salnow, 0.9)

# 10. Create a histogram of education level (edlevel). Describe the shape of the distribution. 
# Run descriptives for education level (edlevel). Write a short description/report.
ed_lvl <- df$edlevel
png("Plots/10_edlvl.png")
hist(ed_lvl, breaks=nclass.Sturges(ed_lvl), xlab = "Education Level", main= NULL, ylim = c(0,200))
dev.off()
describe(df$edlevel)


# 11. Report the mean, standard deviation and range of all quantitative variables.
nums <- unlist(lapply(df, is.numeric)) 
nums
describe(df[ , nums])

# 12. Perform a Z-transformation on all quantitative variables. 
# Report the mean, standard deviation and range of the transformed scores
qual <- df[ , nums]
qual
describe((qual-apply(qual,2,mean))/apply(qual,2,sd))
cbind(mean(qual),sd(qual))
colMeans(qual)

# 13. Using z-transformed variables, select females only. 
# What is their average standardized score for education level?
  ?apply
  
  