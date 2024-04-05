###############################################################################
##                                                                           ##
##  This R practice script shows how to:                                     ##   
##  - Make a design matrix                                                   ##
##  - Make a multiple linear Regression                                      ##        
##  - Apply t-test for significance                                          ##
##                                                                           ##
##  Here it could be examined two examples:                                  ##
##  - A design matrix for a complex regression example of one batch of data  ##
##  - A design matrix for data of two different batches                      ##
##                                                                           ##
##                                                                           ##
##  The design matrix is the basic data object on which ML algorithms operate##
##  The design matrix is a fundamental mathematical object in linear models  ##
##  In a linear model, the dependent variable is linearly related to         ##
##  features.                                                                ##
##  A regression model may be represented as  Y= X * B + e                   ##
##  X --- the design matrix, the matrix of the descriptors x_j,i (features)  ##
##  e --- vector of residuals, vector of random errors with mean             ##
##  B --- vector of coefficients of each variable                            ##
##  Y --- vector of predicted outputs                                        ##
##                                                                           ##
##                                                                           ##
##  To know if there is a statistically significant relationship between     ##
##  the predictor variable and the response variable, a t-test is performed. ##                    
##  A t-test (also known as Student's t-test) is a tool for evaluating       ##
##  the means of one or two populations using hypothesis testing.            ##
##                                                                           ##
##                                                                           ##
##  For more information of the concepts:                                    ##
##  http://www.statistics4u.com/fundstat_eng/ee_designmatrix.html            ##
##  https://towardsdatascience.com/generalized-linear-models-9cbf848bb8ab    ##                                                           
##  https://www.statology.org/t-test-linear-regression/                      ##
##  https://medium.com/@albane.colmenares/the-statistical-foundation-of-     ##
##     linear-regression-t-tests-anova-and-chi-square-tests-46ab3114a411     ##
##                                                                           ##
##  Info about desing matrix in gene expression:                             ##
##  https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7873980/                    ##
##                                                                           ##
###############################################################################

### LOAD LIBRARIES
library(ggplot2) # for visualization



### EXAMPLE 1: COMPLEX REGRESSION

# Experiment consists in 2 groups, for example, of mice. 
# One group (4 mice) is used as control, 
# The other (4 mice) have altered mutant genes
# Could be predicted if the mouse has a mutant gene by measuring the weight?


# First, create labels for data
Type <- factor(c(rep("Control", times = 4),rep("Mutant", times = 4)))


# Make vectors for de dependent and independent variables 
# with values for the control (first 4) and the mutant mice (last 4)
# variables are weight in g (dependent) and size in cm (independent) 
Weight <- c(67.2, 98.0, 123.2, 137.2, 47.6, 78.4, 89.6, 109.2)
Size <- c(4.75, 7.50, 7.25, 9.25, 7.00, 8.25, 9.75, 12.00)


# Construct the design matrix
# Type should be our vector of residuals
# Documentation: https://rdrr.io/r/stats/model.matrix.html
model.matrix(~Type+Weight)


# Fit the linear regression model 
# Documentation: 
# https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/lm
model <- lm(Size~Type+Weight)
model


# Check the results of the model
# Documentation:
# https://www.statology.org/interpret-regression-output-in-r/
# In the results we can check the p-value of the t-test for alpha=0.5
# p-value < 0.5 is considered statistically significant
summary(model)



# Check results on a graph
# Plotting the data points in a scatter plot
# Documentation:https://r-coder.com/plot-r/
plot(Weight, Size, col = ifelse(Type == "Control",  "#D95F02", "#1B9E77"), 
     pch = ifelse(Type == "Control", 16, 17), 
     xlab = "Weight (g)", ylab = "Size (cm)", 
     main = "Example using design matrix for plotting regression lines", 
     cex.main = 0.8)


# Adding the linear model lines for each group using the DESING MATRIX
# Documentation:
# https://www.rdocumentation.org/packages/graphics/versions/3.6.2/topics/abline
abline(model$coefficients["(Intercept)"], 
       model$coefficients["Weight"], col = "#D95F02" )  # Control group

abline(model$coefficients["(Intercept)"] + model$coefficients["TypeMutant"], 
       model$coefficients["Weight"], col = "#1B9E77")  # Mutant group


# Adding a legend
legend("topleft", legend = levels(Type),  pch = c(16, 17), 
       col = c( "#D95F02", "#1B9E77"), title = "Type", cex = 0.65)





### EXAMPLE 2:Batch effect

# Experiment consists in 2 batches of mice with and without mutant genes  
# Batch means experiment was reproduced again for other group, mice  and/or time
# In the following experiment there are two replicates (two batches), A and B
# For each batch one group (3 mice) is used as control, 
# The other in the same batch (3 mice) have altered mutant genes
# Are there a significant difference between batches?


# First, create labels for data
Lab <- factor(c(rep("A", times = 6), 
                rep("B", times = 6))) 

Type <- factor(c(rep("Control", times = 3), 
                 rep("Mutant", times = 3),
                 rep("Control", times = 3), 
                 rep("Mutant", times = 3)))

# The last experiment between the weight and the mutant gene
# Thus, here only is measured the amount of gene expression instead of weight
Gene_Expression <- c(1.7,2,2.2,
               3.1,3.6, 3.9,
               0.9,1.2, 1.9,
               1.8,2.2,2.9)


# Construct the design matrix
# Type and LAB should be our vector of residuals for the mean
model.matrix(~Lab+Type)


# Fit the linear regression model 
batch.lm <- lm(Gene_Expression ~ Lab + Type)
batch.lm


# Check the results of the model
summary(batch.lm)


# VISUALIZATION
# Average values of each group to graph the mean 
mean_data <- data.frame(
  Average_expresion = c(
    mean(Gene_Expression[Lab == "A" & Type == "Control"]),
    mean(Gene_Expression[Lab == "A" & Type == "Mutant"]),
    mean(Gene_Expression[Lab == "B" & Type == "Control"]),
    mean(Gene_Expression[Lab == "B" & Type == "Mutant"])
  )
)


# Graph the data
# Adding the linear model lines for each group using the DESING MATRIX
# Documentation: https://ggplot2.tidyverse.org/reference/geom_point.html
# Documentation: https://ggplot2.tidyverse.org/reference/geom_segment.html
ggplot() +
  geom_point(aes(x = Lab, y = Gene_Expression, color = Type, shape = Type), 
             size = 2) +
 
  # to graph the means of each group
  geom_segment(aes(x = 0.75, y = mean_data[1,1], 
                   xend = 1.25, yend = mean_data[1,1]),
               color = '#D95F02') +
  geom_segment(aes(x = 1.75, y = mean_data[3,1], 
                   xend = 2.25, yend = mean_data[3,1]),
               color = '#D95F02') +
  geom_segment(aes(x = 0.75, y = mean_data[2,1], 
                   xend = 1.25, yend = mean_data[2,1]),
               color = '#1B9E77') +
  geom_segment(aes(x = 1.75, y = mean_data[4,1], 
                   xend = 2.25, yend = mean_data[4,1]),
               color = '#1B9E77') +
  
  # to graph the linear model
  
  
  labs(title = "Gene Expression by Lab and Type", 
       x = "Lab", y = "Gene Expression") +
  theme_minimal() +
  theme(legend.position = "top")



