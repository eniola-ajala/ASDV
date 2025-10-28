# Installing packages
install.packages("datarium")
install.packages("RVAideMemoire")
install.packages("car")
install.packages("carData")


# Calling the libraries
library(datarium)
library(RVAideMemoire)
library(car)
library(car)
 
# One-way ANOVA using Heart Attach Data in Datarium
head(as.data.frame(heartattack))

# Box plot to see if there are any outliers
boxplot(cholesterol ~ drug, data = heartattack, names = c("A", "B", "C"),
        xlab = "Types of Drug", ylab = "Cholesterol Level",
        main = "Cholesterol level for each drug type")

#Performing Shapiro-wilk test to check for normality
byf.shapiro(cholesterol ~ drug, data = heartattack)
# since the p-values are greater than 0.05, we accept the null hypothesis and can confirm that the data is nornally distributed


# to check homogeniety of variances we use bartlett test
bartlett.test(cholesterol ~ drug, data = heartattack)

find("bartlett.test")

#ANOVA Test
oneway.test(cholesterol ~ drug, data = heartattack, var.equal = TRUE)
# Since p-value is 0.07 (greater than 0.05), then we will accept the null hypothesis. ie there is no significant differnce between the three drugs

?oneway.test

# Two-way ANOVA
summary(heartattack)
byf.shapiro(cholesterol ~ drug*gender, data = heartattack)
bartlett.test(cholesterol ~ interaction(drug, gender), data = heartattack)

#the data is normal
result_aov1 <- aov(cholesterol ~ drug * gender, data = heartattack)
summary(result_aov1)

#post-hoc analysis
TukeyHSD(result_aov1, which = "gender")

result_aov2 <- aov(cholesterol ~ drug + gender, data = heartattack)
summary(result_aov2)

#Two-way ANOVA (unbalanced data)
summary(jobsatisfaction)
head(jobsatisfaction)

unb_anova <- aov(score ~ education_level * gender, data = jobsatisfaction)
Anova(unb_anova, type = "III")

?kruskal.test
kruskal.test(cholesterol ~ drug, data = heartattack)


#challenge
plant <- PlantGrowth
head(plant)
summary(plant)

byf.shapiro(weight ~ group, data = plant)
oneway.test(weight ~ group, data = plant, var.equal = TRUE)
