
# ---------------------------CamoEvo Demo Data Analysis-----------------------------------------------------------------------------------
#========================================================================================================================================

# ---------------0) Introduction----------------------------------
# We  evolved triangles against tree bark backgrounds and wished to test
# the following hypotheses:
# 1) survival time of the targets should increase with generation.
# 2) Luminance, Contrast and Colour difference should decrease with survial time.
# 3) Edge disruption (GabRat) should increase with survival time.
# 4) The camouflage measures (Luminance, Contrast, Colour, Edge Disruption) should 
#     correlate with surival time regardless of generation.

# The code here will allow the user to run LMM models on the data provided.


# ---------------1) Import and Handle Data------------------------------------------------------------------------------------

# Set the working directory:

# ^ once Set

# Load the data created by CamoEvo
triangle <-read.table("Data_Output_TriangleDemo.txt", header=TRUE, fill=TRUE)


# Check the structure of the dataframe to see how each variable was coded:
str(triangle)
head(triangle)


# We actually want the time variable to be coded as numeric rather than whole numbers.


triangle$Survival_Time <- as.numeric(triangle$Survival_Time)

triangle$Response_Time <- as.numeric(triangle$Response_Time)

triangle$Capture_Time <- as.numeric(triangle$Capture_Time)



# Assign the triangle data as your main dataframe.


data <-triangle




# ---------------2) Test Data Distribution------------------------------------------------------------------------------------

#Time
#........
hist(log(data$Survival_Time), breaks=10)

shapiro.test(log(data$Survival_Time))

#Significant (2.126e-14), 95% confidence interval



#Luminance
#........
hist(log(data$Luminance_Difference), breaks=10)

shapiro.test(log(data$Luminance_Difference))

#Significant ( 2.2e-16), 95% confidence interval


#Contrast
#........
hist(log(data$Contrast_Difference), breaks=10)

shapiro.test(log(data$Contrast_Difference))

#Significant different from normal (7.503e-13), 95% confidence interval


#Colour
#........
hist(log(data$Colour_Difference), breaks=10)
shapiro.test(log(data$Colour_Difference))

#Significant (1.05e-13), 95% confidence interval


#GabRat
#........
hist(log(data$GabRat_Edge_Disruption), breaks=10)
shapiro.test(log(data$GabRat_Edge_Disruption))

#Significant (1.05e-13), 95% confidence interval





# ---------------3) Models (Generation ~ X)------------------------------------------------------------------------------------
# Use ggplots
library(ggplot2)

# Polynomial Analysis as values are expected to curtail with time.

#..................................................................................................................................
# generation  ~ log(Survival_Time)
#..................................................................................................................................

# Does survival time improve with generation?


#Explore
#=======
# Look at the Data

plot(data$Generation, data$Survival_Time)



# Stats
#=======

m1 <- lm(Generation ~ poly(log(Survival_Time),2), data=data )

# Then look at the model summary:
summary(m1)


# Poly 1, p< 0.01, negative estimate

# Poly 2, p< 0.01, positive estimate



# Before trusting the model we should check whether the data fit the model's assumptions using
# diagnostic plots:
plot(m1)
 

# Plot
#======

# As both poly 1 and 2 were significant line was fitted with a polynomial

p1 <- ggplot(data, aes(x=Generation, y=log(Survival_Time))) +  geom_point( color="blue", fill="blue", alpha = 0.2, size=3.5) +
  geom_smooth(method=glm,formula=y~poly(x,2), size=3)

p1 + theme_classic()  + theme(text = element_text(size = 24), line = element_line(size=1.4)) +  ylab("Log Survival Time (milliseconds)") + xlab("Generation") + theme(
  axis.title.x = element_text(family = "sans", size = 24, margin=margin(5,0,0,0)), 
  axis.title.y = element_text(family = "sans", size = 24, margin=margin(0,20,0,0)), 
  panel.border = element_rect(colour = "black", fill=NA, size=2)
)   


# Interp
#======

#Survival Time significantly increases with generation following a polynomial relationship.

# This output is to be expected as fitness should improve with each generation 
# if the genetic algorithm is able to output successively more effective camouflage.




#..................................................................................................................................
# generation  ~ Luminance_Difference
#..................................................................................................................................

# Does luminance difference decrease with generation?

#Explore
#=======
# Look at the Data

plot(data$Generation, data$Luminance_Difference)



# Stats
#=======

m1 <- lm(Generation ~ poly(Luminance_Difference,2), data=data )

# Then look at the model summary:
summary(m1)


# Poly 1, p< 0.01, negative estimate

# Poly 2, p > 0.01, negative estimate


# Before trusting the model we should check whether the data fit the model's assumptions using
# diagnostic plots:
plot(m1)


# Plot
#======

# As poly 2 wasn't significant line was fitted with a linear line.

p1 <- ggplot(data, aes(x=Generation, y=Luminance_Difference)) +  geom_point( color="blue", fill="blue", alpha = 0.2, size=3.5) +
  geom_smooth(method=lm, size=3)

p1 + theme_classic()  + theme(text = element_text(size = 30), line = element_line(size=1.4)) +  ylab("Luminance Difference") + xlab("Generation") + theme(
  axis.title.x = element_text(family = "sans", size = 30, margin=margin(5,0,0,0)), 
  axis.title.y = element_text(family = "sans", size = 30, margin=margin(0,20,0,0)), 
  panel.border = element_rect(colour = "black", fill=NA, size=2.5)
)   


# Interp
#======

#Luminance difference significantly decreases with generation following a linear relationship.
# This output suggests that having a luminance closer to the background is being selected for.



#..................................................................................................................................
# generation  ~ Contrast_Difference
#..................................................................................................................................

# Does contrast difference decrease with generation?

#Explore
#=======
# Look at the Data

plot(data$Generation, data$Contrast_Difference)


# Stats
#=======

m1 <- lm(Generation ~ poly(Contrast_Difference,2), data=data )

# Then look at the model summary:
summary(m1)


# Poly 1, p< 0.01, negative estimate

# Poly 2, p < 0.01, negative estimate



# Before trusting the model we should check whether the data fit the model's assumptions using
# diagnostic plots:
plot(m1)


# Plot
#======

# As both poly 1 and 2 were significant line was fitted with a polynomial

p1 <- ggplot(data, aes(x=Generation, y=(Contrast_Difference))) +  geom_point( color="blue", fill="blue", alpha = 0.2, size=3.5) +
  geom_smooth(method=glm,formula=y~poly(x,2), size=3)

p1 + theme_classic()  + theme(text = element_text(size = 30), line = element_line(size=1.4)) +  ylab("Contrast Difference") + xlab("Generation") + theme(
  axis.title.x = element_text(family = "sans", size = 30, margin=margin(5,0,0,0)), 
  axis.title.y = element_text(family = "sans", size = 30, margin=margin(0,20,0,0)), 
  panel.border = element_rect(colour = "black", fill=NA, size=2.5)
)   



# Interp
#======

#Contrast difference significantly decreases with generation following a polynomial relationship.
# This output suggests that having a contrast closer to the background is being selected for.
# However, towards the end contrast begins to increase.

#..................................................................................................................................
# generation  ~ Colour_Difference
#..................................................................................................................................

# Does colour difference decrease with generation?

#Explore
#=======
# Look at the Data

plot(data$Generation, data$Colour_Difference)


# Stats
#======

m1 <- lm(Generation ~ poly(Colour_Difference,2), data=data )

# Then look at the model summary:
summary(m1)


# Poly 1, p< 0.01, negative estimate

# Poly 2, p < 0.01, positive estimate



# Before trusting the model we should check whether the data fit the model's assumptions using
# diagnostic plots:
plot(m1)


# Plot
#======

# As both poly 1 and 2 were significant line was fitted with a polynomial

p1 <- ggplot(data, aes(x=Generation, y=(Colour_Difference))) +  geom_point( color="blue", fill="blue", alpha = 0.2, size=3.5) +
  geom_smooth(method=glm,formula=y~poly(x,2), size=3)

p1 + theme_classic()  + theme(text = element_text(size = 30), line = element_line(size=1.4)) +  ylab("Colour Difference") + xlab("Generation") + theme(
  axis.title.x = element_text(family = "sans", size = 30, margin=margin(5,0,0,0)), 
  axis.title.y = element_text(family = "sans", size = 30, margin=margin(0,20,0,0)), 
  panel.border = element_rect(colour = "black", fill=NA, size=2.5)
)   


# Interp
#======

#Colour difference significantly decreases with generation following a polynomial relationship.
# This output suggests that having a colour closer to the background is being selected for.



#..................................................................................................................................
# generation  ~ GabRat
#..................................................................................................................................

# Does edge disruption increase with generation?

#Explore
#=======
# Look at the Data

plot(data$Generation, data$GabRat_Edge_Disruption)


# Stats
#======

m1 <- lm(Generation ~ poly(GabRat_Edge_Disruption,2), data=data )

# Then look at the model summary:
summary(m1)


# Poly 1, p< 0.01, positive estimate

# Poly 2, > 0.01, negative estimate


# Before trusting the model we should check whether the data fit the model's assumptions using
# diagnostic plots:
plot(m1)


# Plot
#======


# As poly 2 wasn't significant line was fitted with a linear line.

p1 <- ggplot(data, aes(x=Generation, y=(GabRat_Edge_Disruption))) +  geom_point( color="blue", fill="blue", alpha = 0.2, size=3.5) +
  geom_smooth(method=lm, size=3)

p1 + theme_classic()  + theme(text = element_text(size = 30), line = element_line(size=1.4)) +  ylab("Edge Disruption (GabRat)") + xlab("Generation") + theme(
  axis.title.x = element_text(family = "sans", size = 30, margin=margin(5,0,0,0)), 
  axis.title.y = element_text(family = "sans", size = 30, margin=margin(0,20,0,0)), 
  panel.border = element_rect(colour = "black", fill=NA, size=2.5)
)   



# Interp
#======

#Edge Disruption significantly increases with generation following a linear relationship.
# This output suggests that having more disruptive edges is being selected for.




# ---------------4) Models (log(Survival_Time) ~ CamouflageFactor)------------------------------------------------------------------------------------

library(lme4)
library(lmerTest)

#..................................................................................................................................
# Luminance Difference & log(Survival_Time)
#..................................................................................................................................
# Does Luminance Difference correlate with capture time, when generation is controlled for?

#Explore
#=======
# Look at the Data

plot(data$Luminance_Difference, log(data$Survival_Time))

# Stats
#=======

m1 <- lmer(log(Survival_Time) ~ Luminance_Difference + (1|Generation), data=data, REML=TRUE )

# Then look at the model summary:
summary(m1)
anova(m1)
rand(m1)


#  p< 0.01, negative estimate


# Before trusting the model we should check whether the data fit the model's assumptions using
# diagnostic plots:
plot(m1)


# Plot
#======

p1 <- ggplot(data, aes(x=Luminance_Difference, y= log(Survival_Time))) +  geom_point( color="blue", fill="blue", alpha = 0.2, size=3.5) +
  geom_smooth(method=lm, size=3)

p1 + theme_classic()  + theme(text = element_text(size = 24), line = element_line(size=1.4)) +  ylab("log(Survival Time)") + xlab("Luminance Difference") + theme(
  axis.title.x = element_text(family = "sans", size = 24, margin=margin(5,0,0,0)), 
  axis.title.y = element_text(family = "sans", size = 24, margin=margin(0,20,0,0)), 
  panel.border = element_rect(colour = "black", fill=NA, size=2)
)   


# Interp
#======


# Survival Time significantly decreases with Luminance Difference following a linear relationship.
# This output suggests that, as expected, luminance difference decreases the camouflage effectiveness of the targets. 


#..................................................................................................................................
# Contrast Difference & log(Survival_Time)
#..................................................................................................................................
# Does Contrast Difference correlate with capture time, when generation is controlled for?

#Explore
#=======
# Look at the Data

plot(data$Contrast_Difference, log(data$Survival_Time))


# Stats
#=======

m1 <- lmer(log(Survival_Time) ~ Contrast_Difference + (1|Generation), data=data, REML=TRUE )

# Then look at the model summary:
summary(m1)
anova(m1)
rand(m1)


#  p< 0.01, negative estimate


# Before trusting the model we should check whether the data fit the model's assumptions using
# diagnostic plots:
plot(m1)


# Plot
#======

p1 <- ggplot(data, aes(x=Contrast_Difference, y= log(Survival_Time))) +  geom_point( color="blue", fill="blue", alpha = 0.2, size=3.5) +
  geom_smooth(method=lm, size=3)

p1 + theme_classic()  + theme(text = element_text(size = 24), line = element_line(size=1.4)) +  ylab("log(Survival Time)") + xlab("Contrast Difference") + theme(
  axis.title.x = element_text(family = "sans", size = 24, margin=margin(5,0,0,0)), 
  axis.title.y = element_text(family = "sans", size = 24, margin=margin(0,20,0,0)), 
  panel.border = element_rect(colour = "black", fill=NA, size=2)
)   


# Interp
#======

# Survival Time significantly decreases with Contrast Difference following a linear relationship.
# This output suggests that, as expected, contrast difference decreases the camouflage effectiveness of the targets. 




#..................................................................................................................................
# Colour Difference & log(Survival_Time)
#..................................................................................................................................
# Does Colour Difference correlate with capture time, when generation is controlled for?

#Explore
#=======
# Look at the Data

plot(data$Colour_Difference, log(data$Survival_Time))


# Stats
#=======

m1 <- lmer(log(Survival_Time) ~ Colour_Difference + (1|Generation), data=data, REML=TRUE )

# Then look at the model summary:
summary(m1)
anova(m1)
rand(m1)


#  p< 0.01, negative estimate


# Before trusting the model we should check whether the data fit the model's assumptions using
# diagnostic plots:
plot(m1)


# Plot
#======

p1 <- ggplot(data, aes(x=Colour_Difference, y= log(Survival_Time))) +  geom_point( color="blue", fill="blue", alpha = 0.2, size=3.5) +
  geom_smooth(method=lm, size=3)

p1 + theme_classic()  + theme(text = element_text(size = 24), line = element_line(size=1.4)) +  ylab("log(Survival Time)") + xlab("Colour Difference") + theme(
  axis.title.x = element_text(family = "sans", size = 24, margin=margin(5,0,0,0)), 
  axis.title.y = element_text(family = "sans", size = 24, margin=margin(0,20,0,0)), 
  panel.border = element_rect(colour = "black", fill=NA, size=2)
)   


# Interp
#======

# Survival Time significantly decreases with Colour Difference following a linear relationship.
# This output suggests that, as expected, colour difference decreases the camouflage effectiveness of the targets. 


#..................................................................................................................................
# Edge Disruption & log(Survival_Time)
#..................................................................................................................................
# Does Edge Disruption correlate with capture time, when generation is controlled for?

#Explore
#=======
# Look at the Data

plot(data$GabRat_Edge_Disruption, log(data$Survival_Time))


# Stats
#=======

m1 <- lmer(log(Survival_Time) ~ GabRat_Edge_Disruption + (1|Generation), data=data, REML=TRUE )

# Then look at the model summary:
summary(m1)
anova(m1)
rand(m1)


#  p< 0.01, positive estimate


# Before trusting the model we should check whether the data fit the model's assumptions using
# diagnostic plots:
plot(m1)


# Plot
#======

p1 <- ggplot(data, aes(x=GabRat_Edge_Disruption, y= log(Survival_Time))) +  geom_point( color="blue", fill="blue", alpha = 0.2, size=3.5) +
  geom_smooth(method=lm, size=3)

p1 + theme_classic()  + theme(text = element_text(size = 24), line = element_line(size=1.4)) +  ylab("log(Survival Time)") + xlab("Edge Disruption (GabRat)") + theme(
  axis.title.x = element_text(family = "sans", size = 24, margin=margin(5,0,0,0)), 
  axis.title.y = element_text(family = "sans", size = 24, margin=margin(0,20,0,0)), 
  panel.border = element_rect(colour = "black", fill=NA, size=2)
)   


# Interp
#======

# Survival Time significantly increases with Edge Disruption following a linear relationship.
# This output suggests that, as expected, more disruptive edges increase the camouflage effectiveness of the targets. 


