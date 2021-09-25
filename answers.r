library(foreign)
list.files(".")

# 1. Load the data from the SPSS file.
df <- read.spss("./salary.sav", to.data.frame=TRUE)
head(df)
summary(df)

# 2. Run a frequency table for "gender". Report how many men are in the sample. 
# Which type of graph would best display data for a variable like gender?
gender_freq <- table(df$sex)
max_y <- max(gender_freq)
png("Plots/2_gender_freq.png")
barplot(gender_freq, main = "Gender Frequency",col = c(4,2), axes = TRUE, border = TRUE)
dev.off()     

# 3. Run a frequency table for "jobcat". 
# Report the most common job category and its percentage.
jobcat_freq <- table(df$jobcat)
cbind(Count = jobcat_freq, Perc = sprintf("%05.2f%%",round(prop.table(jobcat_freq)*100, 2)))

# 4. Make a pie chart for "jobcat" 
png("Plots/rplot.png")
pie(jobcat_freq, col = 2:8)
legend(0.9, 1, names(jobcat_freq), cex = 0.7, fill = 2:8)
dev.off()

# 5. report the percentage of people who are either clerical workers or security officers
cler_sec <- sum(df$jobcat == c('CLERICAL', 'SECURITY OFFICER'))/length(df$jobcat)
sprintf("The percetage of people who are wither clerical workers or security officers is: %05.2f%%", cler_sec*100)

# 6. Run a frequency distribution for “salnow”
salnow_freq <- table(df$salnow)