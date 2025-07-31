# ANOVA IN R
# The key steps include the following:
# 1) Entering or importing data to be analyzed.
# 2) Creating a single vector that holds all numerical values from all categories 
# under study.
# 3) Creating a second vector of the same length that holds the names ofall 
# categories under study (either nominal or ordinal data).
# 4) Combining the above two vectors into a two---column data frame, with the 
# second column containing the category names (this column is “a factor”).
# 5) Graphical investigation of the data (exploratory data analysis).
# 6) Checking assumptions.
# 7) Fitting an ANOVA model, if assumptions are met.
# 8) Producing an ANOVA table.
# 9) Interpretation of ANOVA table.
# 10) Implementation of a multiple comparison test, if the null hypothesisof
# equal means (H0 = μ1 = μ2 = …μk) is rejected.
# 11) Interpretation of multiple comparison test results.


# Step 1 --- Entering or importing data to be analyzed
Rat_Chow = read.table(file.choose(),header=T)
Rat_Chow

# Step 2 --- Creating a single vector that holds all numerical values
Rat_Wts = c(Rat_Chow$Chow_Only,Rat_Chow$Chow_1caf,Rat_Chow$Chow_24caf)
Rat_Wts

# Step 3 --- Creating a vector of that holds all of the category names
Access = c(rep("Caf_No",15),rep("Caf_1hr",15),rep("Caf_24hr",15))
Access

# Step 4 --- Combining the above two vectors into a two---column data frame
Rat_Study = data.frame(Rat_Wts,Access)
Rat_Study

# Step 5 --- Graphical investigation ofthe data (EDA)
boxplot(Rat_Wts ~ Access, data=Rat_Study, col=c(2,3,4), main="Rat Study")
boxplot(Rat_Wts ~ Access, data=Rat_Study,col=c("dark green","dark red","dark blue"),main="Comparison of Rat Weights")

# Step 6 --- Checking assumptions
sd(Rat_Chow$Chow_Only)
sd(Rat_Chow$Chow_1caf)
sd(Rat_Chow$Chow_24caf) 

# Step 7 --- Fitting an ANOVA model
my_results = aov(Rat_Wts ~ Access, data=Rat_Study)

# Step 8 --- Producing an ANOVA table
anova(my_results)

# Step 9 --- Interpretation of ANOVA table
###################################
# The p-value (Pr(>F)) is 3.794e-06, which is highly significant (***), so we 
# reject the null hypothesis of equal means (H0 = µ1 = µ2 = …µk) and infer that the 
# three samples were not drawn from a common population.

# Step 10 --- Implementation of a multiple comparison test
# Tukey multiple comparisons of means - 95% family-wise confidence level
TukeyHSD(my_results,conf.level = 0.95)

# Step 11 --- Interpretation of multiple comparison test results
###############################################
# The results of Tukey’s HSD multiple comparison test reveal that “Caf_No” 
# category (or sample) has a mean that is significantly different from those of 
# the other two categories, “Caf_1hr” (p = 0.0021763) and “Caf_24hr” 
# (p = 0.0000025).
# In contrast, the means of “Caf_1hr” and “Caf_24hr” are not significantly 
# different (p = 0.0921396)
