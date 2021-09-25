library(foreign)
setwd("D:/My New documents/MscDataScience/Elements of Statistics/assignment")
list.files(".")

# 1. Load the data from the SPSS file.
df <- read.spss("./salary.sav", to.data.frame=TRUE)
head(df)

# 2. Run a frequency table for "gender". Report how many men are in the sample. 
# Which type of graph would best display data for a variable like gender?
gender_freq <- table(df$sex)
max_y <- max(gender_freq)
# png("rplot.png")
barplot(gender_freq, main = "Gender Frequency",col = c(4,2), axes = TRUE, border = TRUE)
# dev.off()     

# 3. Run a frequency table for "jobcat". 
# Report the most common job category and its percentage.
jobcat <- table(df$jobcat)
cbind(jobcat, round(prop.table(jobcat)*100, 2))

# 4. Make a pie chart for "jobcat"   
pie(jobcat, col = 2:8)
legend(0.9, 1, names(jobcat), cex = 0.7, fill = 2:8)

