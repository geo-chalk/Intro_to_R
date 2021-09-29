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
# png("Plots/2_gender_barplot.png")
barplot(gender_freq, main = "Gender Frequency",col = c(4,2), axes = TRUE, border = TRUE)
# dev.off()

# 3. Run a frequency table for "jobcat". 
# Report the most common job category and its percentage.
frq(df$jobcat,title = "Job Category", out = "v")
sprintf("The most common category is Clerical with 227 samples")

# 4. Make a pie chart for "jobcat" 
# png("Plots/rplot.png")
x <- 100*table(df$jobcat)/length(df$jobcat) 
pie(x, label=paste( round(x,1), '%', sep='') , col=2:8) 
legend(0.8, 1, levels(df$jobcat), cex = 0.7, fill = 2:8)
# dev.off()

# 5. report the percentage of people who are either clerical workers or security officers
cler_sec <- sum((df$jobcat == 'CLERICAL')|(df$jobcat == 'SECURITY OFFICER'))/nrow(df)
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
# png("Plots/10_edlvl.png")
hist(ed_lvl, breaks=nclass.Sturges(ed_lvl), xlab = "Education Level", main= NULL, ylim = c(0,200))
# dev.off()
describe(df$edlevel)


# 11. Report the mean, standard deviation and range of all quantitative variables.
nums <- unlist(lapply(df, is.numeric)) 
nums
quan <- df[ , nums][-c(1)] #ignore the id
describe(quan)[c('mean', 'sd', 'range')] 



# 12. Perform a Z-transformation on all quantitative variables. 
# Report the mean, standard deviation and range of the transformed scores
ztran <- function(x, na.rm = TRUE) {
  mns <- colMeans(x, na.rm = na.rm)
  sds <- apply(x, 2, sd, na.rm = na.rm)
  x <- sweep(x, 2, mns, "-")
  x <- sweep(x, 2, sds, "/")
  x
}
z_quan <- ztran(quan)
describe(z_quan)[c('mean', 'sd', 'range')]


# 13. Using z-transformed variables, select females only. 
# What is their average standardized score for education level?
nor_gender <- df$sex
zdf <- cbind(z_quan,df$sex)
zdf
aggregate(zdf[, c('edlevel')], list(zdf$nor_gender), mean)


# 14. Reselect all cases. Calculate a new variable called “raise” which is the difference 
# between current salary and beginning salary. Report the mean, median, and standard deviation for “raise”.
df$raise = df$salnow - df$salbeg
head(df[c('id', 'salbeg', 'salnow','raise' )])
describe(df$raise)[c('mean', 'median', 'sd')]

# 15. Which person (report case ID) had the greatest increase in salary from beginning until now? 
# Report the value of the increase.
df[df$raise == max(df$raise),]

# 16. If you have many different values for a variable, it may be more meaningful to group scores together. 
# Recode “salbeg” into a new variable called “salbeg2”.
describe(df$salbeg)
mapping <- function(x) {
  if ((0 <= x) & (x <= 4999)) {
    f <- 0
  }
  else if ((5000 <= x) & (x <= 9999)){
    f <- 1
  }
  else if ((10000 <= x) & (x <= 14999)){
    f <- 2
  }
  else if ((15000 <= x) & (x <= 19999)){
    f <- 3
  }
  else if ((20000 <= x) & (x <= 24999)){
    f <- 4
  }
  else{
    f <- 5
  }
  return(f)
}
df$salbeg2 <- lapply(df$salbeg, mapping)
head(df)

# 17. How many people (and their %) have beginning salaries between $10,000 and $14,999?
sprintf("There are %d people with salaries between $10,000 and $14,999 corresponding to %05.2f%%"
        , sum(df$salbeg2 == '2'), sum(df$salbeg2 == '2')/nrow(df)*100)

# 18. Prepare a scatterplot showing the relationship between education level (edlevel) and current salary (salnow). 
# Put education on the x-axis and current salary on the y-axis.
# png("Plots/2_scatterplot.png")
plot(df$edlevel, df$salnow, 
     xlab = "Education Level", ylab = "Current Salary")
# dev.off()

# 19. How does the relationship between education and current salary appear overall? Linear or non-linear?
print("The relashionship appears Linear")

# 20. Add the regression line to the graph.
# png("Plots/2_scatterplot_LR.png")

plot(scale(df$edlevel), scale(df$salnow),  xlab = "Education Level", ylab = "Current Salary")
lm(formula = scale(df$edlevel)~scale(df$salnow))
abline(lm(formula = scale(df$edlevel)~scale(df$salnow)), col=2, lw=2)
# dev.off()
          