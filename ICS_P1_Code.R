library(tidyverse)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(reshape2)

demographic <- read.csv(file.choose(), sep =",")

head(demographic)

demographic <- demographic %>%
  group_by(Region, Subregion) %>%
  mutate(across(starts_with("Life.Expectancy"), ~ ifelse(is.na(.), mean(.[!is.na(.)]), .)),
         across(starts_with("Under.Age.5.Mortality"), ~ ifelse(is.na(.), mean(.[!is.na(.)]), .))) %>%
  ungroup()


demographic

# Check for any remaining missing values
sum(is.na(demographic))

head(demographic)

colSums(is.na(demographic))





### Part1

head(demographic)





# Frequency distribution of Mortality rate in 2022

### Question1


Census_2002 <- demographic[demographic$Year == 2002, ]
Census_2022 <- demographic[demographic$Year == 2022, ] 


# Frequency distribution of Mortality rate of both sexes in 2022

Histogram_infant_mortality <-ggplot(Census_2022, aes(x =Under.Age.5.Mortality..Both.Sexes)) + 
  geom_histogram(aes(y=..density..), color="Black", fill="light green") +
  scale_x_continuous(name = "U5 mortality rates of both sexes") +
  scale_y_continuous(name = "density") 
Histogram_infant_mortality

# Frequency distribution of life expectancy of both sexes in 2022

Histogram_both_sexes <-ggplot(Census_2022, aes(x = Life.Expectancy.at.Birth..Both.Sexes)) +
  geom_histogram(aes(y=..density..), color="Black", fill="#00BFC4") +
  scale_x_continuous(name = "LE at birth of both sexes") +
  scale_y_continuous(name = "density") 
Histogram_both_sexes

# Frequency distribution of Mortality rate of males in 2022

Histogram_males_mortality <-ggplot(Census_2022, aes(x =Under.Age.5.Mortality..Males)) +
  geom_histogram(aes(y=..density..), color="Black", fill="light green") +
  scale_x_continuous(name = "U5 mortality rates of males") +
  scale_y_continuous(name = "density") 
Histogram_males_mortality


# Frequency distribution of life expectancy of males in 2022


Histogram_males <-ggplot(Census_2022, aes(x =Life.Expectancy.at.Birth..Males)) +
  geom_histogram(aes(y=..density..), color="Black", fill="#00BFC4") +
  scale_x_continuous(name = "LE at birth of males")+  
  scale_y_continuous(name = "density") + ylim(0, 0.08)
Histogram_males


# Frequency distribution of Mortality rate of females in 2022

Histogram_females_mortality <-ggplot(Census_2022, aes(x =Under.Age.5.Mortality..Females)) +
  geom_histogram(aes(y=..density..), color="Black", fill="light green") +
  scale_x_continuous(name = "U5 mortality rates of females") +
  scale_y_continuous(name = "density") 

Histogram_females_mortality


# Frequency distribution of life expectancy of females in 2022

Histogram_females <-ggplot(Census_2022, aes(x =Life.Expectancy.at.Birth..Females)) +
  geom_histogram(aes(y=..density..), color="Black", fill="#00BFC4") +
  scale_x_continuous(name = "LE at birth of females") +
  scale_y_continuous(name = "density") 

Histogram_females


# All combined histogram

histogram <- grid.arrange(Histogram_infant_mortality, Histogram_both_sexes, Histogram_males_mortality, Histogram_males,Histogram_females_mortality , Histogram_females, nrow=3, ncol=2)
histogram


# finding mean, median, minimum, maximum and standard deviation for life expectancy and 
# Under 5 mortality rate and life expectancy of male, female and both sexes. 

mortality_rate_both_22 <- filter(demographic, Year == 2022)$Under.Age.5.Mortality..Both.Sexes
summary(mortality_rate_both_22)
sd(mortality_rate_both_22) 



mortality_rate_m_22 <-  filter(demographic, Year == 2022)$Under.Age.5.Mortality..Males
summary(mortality_rate_m_22)
sd(mortality_rate_m_22)



mortality_rate_f_22 <-  filter(demographic, Year == 2022)$Under.Age.5.Mortality..Females
summary(mortality_rate_f_22)
sd(mortality_rate_f_22)


life_expec_both_22 <- filter(demographic, Year == 2022)$Life.Expectancy.at.Birth..Both.Sexes
summary(life_expec_both_22)
sd(life_expec_both_22) 



life_expect_f_22 <- filter(demographic, Year == 2022)$Life.Expectancy.at.Birth..Females
summary(life_expect_f_22)
sd(life_expect_f_22) 



life_expec_m_22 <- filter(demographic, Year == 2022)$Life.Expectancy.at.Birth..Males
summary(life_expec_m_22)
sd(life_expec_m_22) 



# part 2 of question 1


LE_both_sexes <- ggplot(data = Census_2022, aes(x = Life.Expectancy.at.Birth..Both.Sexes, y = Region)) +
  geom_point(aes(x = Life.Expectancy.at.Birth..Both.Sexes,y= Region, color = Region), alpha=0.5) +
  geom_boxplot(alpha=0.6, size=0.7, outlier.color = "Black",  aes(fill = Region)) +
  xlab("LE at birth of both sexes") +
  ylab("Regions") +
  xlim(50,90)+ 
  
  theme(axis.title.x = element_text(colour="Black", size=14, face = "bold"),
        axis.title.y = element_text(colour="Black", size=12, face = "bold"),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=12),
        legend.title = element_text(size=12, face = "bold"),
        legend.text = element_text(size=10),
        )

LE_both_sexes


#Life.Expectancy..Males
LE_males <- ggplot(data = Census_2022, aes(x =  Life.Expectancy.at.Birth..Males, y = Region))+
  geom_point(aes(y= Region, x =  Life.Expectancy.at.Birth..Males, colour = Region), alpha=0.5) +
  geom_boxplot(alpha=0.6, size=0.7,outlier.color = "Black",  aes(fill = Region)) +
  xlab("LE at birth of males") + 
  ylab("Regions") +
  xlim(50,100)+ 
  #theme(legend.position = "none")+
  theme(axis.title.x = element_text(colour="Black", size=14, face = "bold"),
        axis.title.y = element_text(colour="Black", size=12, face = "bold"),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        legend.title = element_text(size=12, face = "bold"),
        legend.text = element_text(size=10))
  

LE_males

#Life.Expectancy..Females
LE_females <- ggplot(data = Census_2022, aes(x = Life.Expectancy.at.Birth..Females, y = Region)) +
  geom_point(aes(y= Region, x = Life.Expectancy.at.Birth..Females, colour = Region), alpha=0.5) +
  geom_boxplot(alpha=0.6, size=0.7, outlier.color = "Black",  aes(fill = Region)) +
  xlab("LE at birth of females") + 
  ylab("Regions") +
  xlim(50,100)+ 
  theme(axis.title.x = element_text(colour="Black", size=14, face = "bold"),
        axis.title.y = element_text(colour="Black", size=12, face = "bold"),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        legend.title = element_text(size=12, face = "bold"),
        legend.text = element_text(size=10)
        )
LE_females

#Mortality.Rate..Both.Sexes


U5_Mortality <- ggplot(data = Census_2022, aes(x = Under.Age.5.Mortality..Both.Sexes, y = Region)) +
  geom_point(aes(y= Region, x = Under.Age.5.Mortality..Both.Sexes, color = Region), alpha=0.5) +
  geom_boxplot(alpha=0.6, size=0.7, outlier.color = "Black",  aes(fill = Region)) +
  xlab("U5 mortality rates of both sexes") + 
  ylab("Regions") +
  xlim(0,155)+ 
  theme(axis.title.x = element_text(colour="Black", size=14, face = "bold"),
        axis.title.y = element_text(colour="Black", size=12, face = "bold"),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        legend.title = element_text(size=12, face = "bold"),
        legend.text = element_text(size=10)
        )


U5_Mortality

U5_Mortality_male <- ggplot(data = Census_2022, aes(x = Under.Age.5.Mortality..Males, y = Region)) +
  geom_point(aes(y= Region, x = Under.Age.5.Mortality..Males, color = Region), alpha=0.5) +
  geom_boxplot(alpha=0.6, size=0.7, outlier.color = "Black",  aes(fill = Region)) +
  xlab("U5 mortality rates of males") + 
  ylab("Regions") +
  xlim(0,155)+  
  
  theme(axis.title.x = element_text(colour="Black", size=14, face = "bold"),
        axis.title.y = element_text(colour="Black", size=12, face = "bold"),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        legend.title = element_text(size=12, face = "bold"),
        legend.text = element_text(size=8)
       )
U5_Mortality_male



U5_Mortality_female <- ggplot(data = Census_2022, aes(x = Under.Age.5.Mortality..Females, y = Region)) +
  geom_point(aes(y= Region, x = Under.Age.5.Mortality..Females, color = Region), alpha=0.5) +
  geom_boxplot(alpha=0.6, size=0.7, outlier.color = "Black",  aes(fill = Region)) +
  xlab("U5 mortality rates of females") + 
  ylab("Regions") +
  xlim(0,155)+ 
  theme(axis.title.x = element_text(colour="Black", size=14, face = "bold"),
        axis.title.y = element_text(colour="Black", size=12, face = "bold"),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        legend.title = element_text(size=12, face = "bold"),
        legend.text = element_text(size=10)
        )

U5_Mortality_female



grid.arrange(U5_Mortality_male, LE_males, nrow = 2, ncol = 1)

grid.arrange(U5_Mortality_female,LE_females, nrow = 2, ncol = 1)

grid.arrange(U5_Mortality,LE_both_sexes, nrow = 2, ncol = 1)



# grid.arrange(U5_Mortality_male, LE_males, 
#              nrow = 2, ncol = 1)
# 
# grid.arrange(U5_Mortality_female, LE_females, 
#              nrow = 1, ncol = 2)
# 


# # arrange the four boxplots horizontally
# combined_plots_new <- grid.arrange(U5_Mortality_male, LE_males, U5_Mortality_female, LE_females, 
#                                nrow = 2, ncol = 2)
# 
# # display the combined plot
# print(combined_plots_new)
# 



# --------------------- 2 Homogeneous\ Heterogeneous behavior among and within the sub regions --------------------------
#----------------------------------------------------------------------------------------------------
Census_2022_africa <- subset(Census_2022, Region == "Africa")

Census_2022_africa$Subregion <- factor(Census_2022_africa$Subregion, levels = unique(Census_2022_africa$Subregion[order(Census_2022_africa$Region)]))


#Life.Expectancy..Both.Sexes
box_both_sexes <- ggplot(data = Census_2022_africa, aes(x = Life.Expectancy.at.Birth..Both.Sexes, y = Subregion)) +
  geom_point(aes(y= Subregion, x = Life.Expectancy.at.Birth..Both.Sexes, color = Region), alpha=0.5) +
  geom_boxplot(alpha=0.6, size=0.7, outlier.color = "Black",  aes(fill = Region)) +
  xlab("LE at birth of both sexes") + 
  ylab("Subregions") +
  xlim(50,90)+ 
  theme(axis.title.x = element_text(colour="Black", size=14, face = "bold"),
        axis.title.y = element_text(colour="Black", size=12, face = "bold"),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        legend.title = element_text(size=12, face = "bold"),
        legend.text = element_text(size=10))
box_both_sexes
#Life.Expectancy..Males
box_males <- ggplot(data = Census_2022_africa, aes(x = Life.Expectancy.at.Birth..Males, y = Subregion)) +
  geom_point(aes(y= Subregion, x = Life.Expectancy.at.Birth..Males, colour = Region), alpha=0.5) +
  geom_boxplot(alpha=0.6, size=0.7, outlier.colour = "Black",  aes(fill = Region)) +
  xlab("LE at birth of males") + 
  ylab("Subregions") +
  xlim(50,90)+ 
  theme(axis.title.x = element_text(colour="Black", size=14, face = "bold"),
        axis.title.y = element_text(colour="Black", size=12, face = "bold"),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        legend.title = element_text(size=12, face = "bold"),
        legend.text = element_text(size=10),
        legend.position = "none")
box_males
#Life.Expectancy..Females
box_females <- ggplot(data = Census_2022_africa, aes(x = Life.Expectancy.at.Birth..Females, y = Subregion)) +
  geom_point(aes(y= Subregion, x = Life.Expectancy.at.Birth..Females, colour = Region), alpha=0.5) +
  geom_boxplot(alpha=0.6, size=0.7, outlier.colour = "Black",  aes(fill = Region)) +
  xlab("LE at birth of females") + 
  ylab("Subregions") +
  xlim(50,90)+ 
  theme(axis.title.x = element_text(colour="Black", size=14, face = "bold"),
        axis.title.y = element_text(colour="Black", size=12, face = "bold"),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        legend.title = element_text(size=12, face = "bold"),
        legend.text = element_text(size=10),
        legend.position = "none")

#Mortality.Rate..Both.Sexes

box_females

#u5 
box_males_u5 <- ggplot(data = Census_2022_africa, aes(x = Under.Age.5.Mortality..Males
                                                      , y = Subregion)) +
  geom_point(aes(y= Subregion, x = Under.Age.5.Mortality..Males
                 , colour = Region), alpha=0.5) +
  geom_boxplot(alpha=0.6, size=0.7, outlier.colour = "Black",  aes(fill = Region)) +
  xlab("U5 mortality rates of males") + 
  ylab("Subregions") +
  xlim(10,160)+ 
  theme(axis.title.x = element_text(colour="Black", size=14, face = "bold"),
        axis.title.y = element_text(colour="Black", size=12, face = "bold"),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        legend.title = element_text(size=12, face = "bold"),
        legend.text = element_text(size=10),
        legend.position = "none")

#u5 
box_females_u5 <- ggplot(data = Census_2022_africa, aes(x = Under.Age.5.Mortality..Females
                                                        , y = Subregion)) +
  geom_point(aes(y= Subregion, x = Under.Age.5.Mortality..Females
                 , colour = Region), alpha=0.5) +
  geom_boxplot(alpha=0.6, size=0.7, outlier.colour = "Black",  aes(fill = Region)) +
  xlab("U5 mortality rates of females") + 
  ylab("Subregions") +
  xlim(10,160)+ 
  theme(axis.title.x = element_text(colour="Black", size=14, face = "bold"),
        axis.title.y = element_text(colour="Black", size=12, face = "bold"),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        legend.title = element_text(size=12, face = "bold"),
        legend.text = element_text(size=10),
        legend.position = "none")

box_females_u5

box_Mortality <- ggplot(data = Census_2022_africa, aes(x = Under.Age.5.Mortality..Both.Sexes, y = Subregion)) +
  geom_point(aes(y= Subregion, x = Under.Age.5.Mortality..Both.Sexes, color = Region), alpha=0.5) +
  geom_boxplot(alpha=0.6, size=0.7, outlier.colour = "Black",  aes(fill = Region)) +
  xlab("U5 mortality rates of both sexes") + 
  ylab("Subregions") +
  xlim(10,160)+ 
  theme(axis.title.x = element_text(colour="Black", size=14, face = "bold"),
        axis.title.y = element_text(colour="Black", size=12, face = "bold"),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        legend.title = element_text(size=12, face = "bold"),
        legend.text = element_text(size=10))


grid.arrange(box_males_u5, box_males,  nrow = 1, ncol = 2)

grid.arrange(box_females_u5, box_females,  nrow = 1, ncol = 2)

grid.arrange(box_Mortality, box_both_sexes,  nrow = 2, ncol = 1)


# Define the legend separately for each plot
legend_box_Mortality <- get_legend(box_Mortality)
legend_box_both_sexes <- get_legend(box_both_sexes)

# Create the final plot with a common legend
final_plot_bothsex <- plot_grid(box_Mortality + theme(legend.position = "top"),
                                box_both_sexes + theme(legend.position = "none"),
                                align = "h",
                                axis = "tb",
                                ncol = 1)

# Add the common legend to the final plot
final_plot_bothsex <- ggdraw() +
  draw_plot(final_plot_bothsex) +
  draw_plot(legend_box_Mortality, x = 0.78, y = 0.15) +
  draw_plot(legend_box_both_sexes, x = 0.9, y = 0.15)

# Show the final plot
final_plot_bothsex



# Define the legend separately for each plot
legend_box_males_u5 <- get_legend(box_males_u5)
legend_box_males <- get_legend(box_males)

# Create the final plot with a common legend
final_plot_males <- plot_grid(box_males_u5 + theme(legend.position = "none"),
                              box_males + theme(legend.position = "top"),
                              align = "h",
                              axis = "tb",
                              ncol = 2)

# Add the common legend to the final plot
final_plot_males <- ggdraw() +
  draw_plot(final_plot_males) +
  draw_plot(legend_box_males_u5, x = 0.78, y = 0.15) +
  draw_plot(legend_box_males, x = 0.9, y = 0.15)

# Show the final plot
final_plot_males




# Define the legend separately for each plot
legend_box_females_u5 <- get_legend(box_females_u5)
legend_box_females <- get_legend(box_females)

# Create the final plot with a common legend
final_plot_females <- plot_grid(box_females_u5 + theme(legend.position = "none"),
                                box_females + theme(legend.position = "none"),
                                align = "h",
                                axis = "tb",
                                ncol = 2)

# Add the common legend to the final plot
final_plot_females <- ggdraw() +
  draw_plot(final_plot_females) +
  draw_plot(legend_box_females_u5, x = 0.78, y = 0.15) +
  draw_plot(legend_box_females, x = 0.9, y = 0.15)

# Show the final plot
final_plot_females




grid.arrange(final_plot_males, final_plot_females, nrow = 2, ncol = 1)




## Part 3
# Are there bi variate correlations between the variables?
# Correlations between life expectancy female and mortality rate.

gr12 <- filter(demographic, Year == 2022) %>% 
  ggplot(aes(y =`Under.Age.5.Mortality..Females`, x =`Life.Expectancy.at.Birth..Females`)) +
  xlab("LE at birth of females") + ylab("U5 mortality rates of females") +  
  theme(plot.title = element_text(size=16,hjust = 0.5,face = "bold"),
        text = element_text(size=17))+
  geom_point(position = position_jitter(w = 0.04, h = 0.02), size = 1.8)
gr12


                   
# Correlations between life expectancy male and mortality rate.

gr22 <- filter(demographic, Year == 2022) %>% 
  ggplot( aes(y = `Under.Age.5.Mortality..Males`, x =`Life.Expectancy.at.Birth..Males`)) +
  xlab("LE at birth of males") + ylab("U5 mortality rates of males") + 
  theme(plot.title = element_text(size=16,hjust = 0.5,face = "bold"),
        text = element_text(size=17))+
  geom_point(position = position_jitter(w = 0.04, h = 0.02), size = 1.8)
gr22

# Correlations between life expectancy both sexes and mortality rate.


gr32 <- filter(demographic, Year == 2022) %>% 
  ggplot( aes(y = `Under.Age.5.Mortality..Both.Sexes`, x =`Life.Expectancy.at.Birth..Both.Sexes`)) +
  xlab("LE at birth of both sexes") + ylab("U5 mortality rates of both sexes") +  
  theme(plot.title = element_text(size=16,hjust = 0.5,face = "bold"),
        text = element_text(size=17))+
  geom_point(position = position_jitter(w = 0.04, h = 0.02), size = 1.8)
gr32





# Correlations between life expectancy female and both sexes.

gr13 <- filter(demographic, Year == 2022) %>% 
  ggplot(aes(Life.Expectancy.at.Birth..Females, Life.Expectancy.at.Birth..Both.Sexes)) +
  ylab("LE at birth of both sexes") + xlab("LE at birth of females") +  
  theme(plot.title = element_text(size=16,hjust = 0.5,face = "bold"),
        text = element_text(size=17))+
  geom_point(position = position_jitter(w = 0.04, h = 0.02), size = 1.8)
gr13

# Correlations between life expectancy male and both sexes.

gr23 <- filter(demographic, Year == 2022) %>% 
  ggplot( aes(Life.Expectancy.at.Birth..Males, Life.Expectancy.at.Birth..Both.Sexes)) +
  ylab("LE at birth of both sexes") + xlab("LE at birth of males") +  
  theme(plot.title = element_text(size=16,hjust = 0.5,face = "bold"),
        text = element_text(size=17))+
  geom_point(position = position_jitter(w = 0.04, h = 0.02), size = 1.8)
gr23

# Correlations between life expectancy male and life expectancy female.

gr33 <- filter(demographic, Year == 2022) %>% 
  ggplot( aes(Life.Expectancy.at.Birth..Females, Life.Expectancy.at.Birth..Males)) +
  ylab("LE at birth of males") + xlab("LE at birth of females") +  
  theme(plot.title = element_text(size=16,hjust = 0.5,face = "bold"),
        text = element_text(size=17))+
  geom_point(position = position_jitter(w = 0.04, h = 0.02), size = 1.8)
gr33

combined <- gr13 + gr23 + gr33 & theme(legend.position = "bottom")
comb_3 <- combined + plot_layout(guides = "collect")
comb_3 #positive relations

combined <- gr12 + gr22 + gr32 & theme(legend.position = "bottom")
comb_2 <- combined + plot_layout(guides = "collect")
comb_2 #negative relationship



comb_2_with_LE <- gr12 + gr22 + gr33 + plot_layout(guides = "collect")
comb_2_with_LE

round(cor(mortality_rate_both_22,life_expec_both_22,method = 'pearson'),2) 
round(cor(mortality_rate_both_22,life_expec_m_22,method = 'pearson' ),2) 
round(cor(mortality_rate_both_22,life_expect_f_22,method = 'pearson' ),2) 
round(cor(life_expec_both_22,life_expec_m_22,method = 'pearson' ),2) 
round(cor(life_expec_both_22,life_expect_f_22,method = 'pearson'),2) 
round(cor(life_expect_f_22,life_expec_m_22 ,method = 'pearson'),2)
mortfemale<- filter(demographic, Year == 2022)$Under.Age.5.Mortality..Females
mortmale<- filter(demographic, Year == 2022)$Under.Age.5.Mortality..Females
round(cor(mortfemale,mortality_rate_both_22 ,method = 'pearson'),2)
round(cor(mortmale,mortality_rate_both_22 ,method = 'pearson'),2)


#ends here bivariate


Census_2022_refined <- Census_2022[,6:11]
colnames(Census_2022_refined) <- c("LE at birth of both sexes", "LE at birth of males", 
                                   "LE at birth of females", "U5 mortality rates of both sexes", 
                                   "U5 mortality rates of males", "U5 mortality rates of females")


correlations <- round(cor(Census_2022_refined[,], use="pairwise", method="pearson"),2)

melted_cormat <- melt(correlations)

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

# Reorder the correlation matrix
cormat <- reorder_cormat(correlations)

##Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
upper_tri <- get_upper_tri(cormat)

# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)


ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "skyblue", high = "darkgray", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="\nPearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 35, vjust = 1, 
                                   size = 14, hjust = 1))+
  theme(axis.text.y = element_text(angle = 360, vjust = 1, 
                                   size = 14, hjust = 1))+
  coord_fixed()



# Print the heatmap

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 6) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.5, 0.75),
    legend.direction = "horizontal",
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 13)
  
    )+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))







###  Part 4


# Scatter plot to show the comparison of mortality rate in 2002 and 2022

#demographic <- read.csv(file = 'census2002_2022.csv')

data_2022 <- subset(demographic,demographic$Year == "2022")
data_2002 <- subset(demographic,demographic$Year == "2002")

plot17<-ggplot(data_2022, aes(x= data_2022$Under.Age.5.Mortality..Both.Sexes, y= data_2002$Under.Age.5.Mortality..Both.Sexes)) +
  geom_point(size=2,aes(color = Region, shape = Region))+
labs(x="U5 mortality of both sexes 2022", 
     y="U5 mortality of both sexes 2002")+
  xlim(0,250)+ 
  theme(axis.title.x = element_text(colour="Black", size=14, face = "bold"),
        axis.title.y = element_text(colour="Black", size=14, face = "bold"),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        legend.title = element_text(size=14, face = "bold"),
        legend.text = element_text(size=14))
k<-plot17+ geom_abline()
k

# Scatter plot to show the comparison of life expectancy both sexes in 2002 and 2022

plot18<-ggplot(data_2022, aes(x=data_2022$Life.Expectancy.at.Birth..Both.Sexes, y=data_2002$Life.Expectancy.at.Birth..Both.Sexes)) +
        geom_point(size=2,aes(color = Region, shape = Region))+ 
        labs(x="LE at birth of both sexes 2022", 
             y="LE at birth of both sexes 2002")+
  xlim(50,100)+ 
        theme(axis.title.x = element_text(colour="Black", size=14, face = "bold"),
            axis.title.y = element_text(colour="Black", size=14, face = "bold"),
            axis.text.x = element_text(size=14),
            axis.text.y = element_text(size=14),
            legend.title = element_text(size=14, face = "bold"),
            legend.text = element_text(size=14)
            )

p<-plot18+ geom_abline()
p


grid.arrange(k,p)


#required data given for the analysis
#To find mean, median, variance and IQR of subgroups
#We change the name of subgroup each time

data_2022 <- subset(demographic,demographic$Year == "2022")


all_the_subrigions <- c("Western Africa","Southern Africa", "Northern Africa", "Middle Africa","Eastern Africa","South America",
                        "Northern America","Central America","Caribbean","Western Asia","South-Eastern Asia",
                        "South-Central Asia","Eastern Asia","Western Europe","Southern Europe","Northern Europe",
                        "Eastern Europe","Polynesia","Micronesia","Melanesia","Australia/New Zealand")
noya<-filter(data_2022, data_2022$Subregion == "Middle Africa" )


#For Life exp.both sexes
mean(noya$Life.Expectancy.at.Birth..Both.Sexes)

median(noya$Life.Expectancy.at.Birth..Both.Sexes)

var(noya$Life.Expectancy.at.Birth..Both.Sexes)

IQR(noya$Life.Expectancy.at.Birth..Both.Sexes)


# Mortality rate.
mean(noya$Under.Age.5.Mortality..Both.Sexes)

median(noya$Under.Age.5.Mortality..Both.Sexes)

var(noya$Under.Age.5.Mortality..Both.Sexes)

IQR(noya$Under.Age.5.Mortality..Both.Sexes)



#data for question 2 for the africa region
# Under.Age.5.Mortality..Both.Sexes
# Create an empty data frame to store the results
results_df <- data.frame()
# Subset the data for the Africa region
africa_df <- subset(Census_2022, Region == "Africa")

# Get unique subregions in Africa
africa_subregions <- unique(africa_df$Subregion)

# Loop through each subregion and calculate the descriptive statistics
for (subregion in africa_subregions) {
  subregion_df <- subset(africa_df, subregion == Subregion)
  subregion_mean <- mean(subregion_df$Under.Age.5.Mortality..Both.Sexes)
  subregion_median <- median(subregion_df$Under.Age.5.Mortality..Both.Sexes)
  subregion_var <- var(subregion_df$Under.Age.5.Mortality..Both.Sexes)
  subregion_iqr <- IQR(subregion_df$Under.Age.5.Mortality..Both.Sexes)
  subregion_min <- min(subregion_df$Under.Age.5.Mortality..Both.Sexes)
  subregion_max <- max(subregion_df$Under.Age.5.Mortality..Both.Sexes)
  
  
  # Save the results in individual variables for each subregion
  # Create a data frame to store the results of this subregion
  subregion_results <- data.frame(
    subregion = subregion,
    mean = round(subregion_mean,2),
    median = round(subregion_median,2),
    var = round(subregion_var,2),
    iqr = round(subregion_iqr,2),
    min = round(subregion_min,2),
    max = round(subregion_max,2)
    
  )
  
  # Append the results to the overall results data frame
  results_df <- rbind(results_df, subregion_results)
}

results_df



# Under.Age.5.Mortality..Males
# Create an empty data frame to store the results
results_df_male <- data.frame()
# Subset the data for the Africa region
africa_df <- subset(Census_2022, Region == "Africa")
# Get unique subregions in Africa
africa_subregions <- unique(africa_df$Subregion)

# Loop through each subregion and calculate the descriptive statistics
for (subregion in africa_subregions) {
  subregion_df <- subset(africa_df, subregion == Subregion)
  subregion_mean <- mean(subregion_df$Under.Age.5.Mortality..Males)
  subregion_median <- median(subregion_df$Under.Age.5.Mortality..Males)
  subregion_var <- var(subregion_df$Under.Age.5.Mortality..Males)
  subregion_iqr <- IQR(subregion_df$Under.Age.5.Mortality..Males)
  subregion_min <- min(subregion_df$Under.Age.5.Mortality..Males)
  subregion_max <- max(subregion_df$Under.Age.5.Mortality..Males)
  
  # Save the results in individual variables for each subregion
  # Create a data frame to store the results of this subregion
  subregion_results <- data.frame(
    subregion = subregion,
    mean = round(subregion_mean,2),
    median = round(subregion_median,2),
    var = round(subregion_var,2),
    iqr = round(subregion_iqr,2),
    min = round(subregion_min,2),
    max = round(subregion_max,2)
    
  )
  # Append the results to the overall results data frame
  results_df_male <- rbind(results_df_male, subregion_results)
}

results_df_male




# Under.Age.5.Mortality..Females
# Create an empty data frame to store the results
results_df_female <- data.frame()
# Subset the data for the Africa region
africa_df <- subset(Census_2022, Region == "Africa")

# Get unique subregions in Africa
africa_subregions <- unique(africa_df$Subregion)

# Loop through each subregion and calculate the descriptive statistics
for (subregion in africa_subregions) {
  subregion_df <- subset(africa_df, subregion == Subregion)
  subregion_mean <- mean(subregion_df$Under.Age.5.Mortality..Females)
  subregion_median <- median(subregion_df$Under.Age.5.Mortality..Females)
  subregion_var <- var(subregion_df$Under.Age.5.Mortality..Females)
  subregion_iqr <- IQR(subregion_df$Under.Age.5.Mortality..Females)
  subregion_min <- min(subregion_df$Under.Age.5.Mortality..Females)
  subregion_max <- max(subregion_df$Under.Age.5.Mortality..Females)
  
  # Save the results in individual variables for each subregion
  # Create a data frame to store the results of this subregion
  subregion_results <- data.frame(
    subregion = subregion,
    mean = round(subregion_mean,2),
    median = round(subregion_median,2),
    var = round(subregion_var,2),
    iqr = round(subregion_iqr,2),
    min = round(subregion_min,2),
    max = round(subregion_max,2)
  )
  
  # Append the results to the overall results data frame
  results_df_female <- rbind(results_df_female, subregion_results)
}

results_df_female



# Life.Expectancy.at.Birth..Both.Sexes
# Create an empty data frame to store the results
results_df_le <- data.frame()
# Subset the data for the Africa region
africa_df <- subset(Census_2022, Region == "Africa")
# Get unique subregions in Africa
africa_subregions <- unique(africa_df$Subregion)

# Loop through each subregion and calculate the descriptive statistics
for (subregion in africa_subregions) {
  subregion_df <- subset(africa_df, subregion == Subregion)
  subregion_mean <- mean(subregion_df$Life.Expectancy.at.Birth..Both.Sexes)
  subregion_median <- median(subregion_df$Life.Expectancy.at.Birth..Both.Sexes)
  subregion_var <- var(subregion_df$Life.Expectancy.at.Birth..Both.Sexes)
  subregion_iqr <- IQR(subregion_df$Life.Expectancy.at.Birth..Both.Sexes)
  subregion_min <- min(subregion_df$Life.Expectancy.at.Birth..Both.Sexes)
  subregion_max <- max(subregion_df$Life.Expectancy.at.Birth..Both.Sexes)
  
  # Save the results in individual variables for each subregion
  # Create a data frame to store the results of this subregion
  subregion_results <- data.frame(
    subregion = subregion,
    mean = round(subregion_mean,2),
    median = round(subregion_median,2),
    var = round(subregion_var,2),
    iqr = round(subregion_iqr,2),
    min = round(subregion_min,2),
    max = round(subregion_max,2)
    
  )
  
  # Append the results to the overall results data frame
  results_df_le <- rbind(results_df_le, subregion_results)
}

results_df_le


# Life.Expectancy.at.Birth..Males
# Create an empty data frame to store the results
results_df_le_male <- data.frame()
# Subset the data for the Africa region
africa_df <- subset(Census_2022, Region == "Africa")

# Get unique subregions in Africa
africa_subregions <- unique(africa_df$Subregion)

# Loop through each subregion and calculate the descriptive statistics
for (subregion in africa_subregions) {
  subregion_df <- subset(africa_df, subregion == Subregion)
  subregion_mean <- mean(subregion_df$Life.Expectancy.at.Birth..Males)
  subregion_median <- median(subregion_df$Life.Expectancy.at.Birth..Males)
  subregion_var <- var(subregion_df$Life.Expectancy.at.Birth..Males)
  subregion_iqr <- IQR(subregion_df$Life.Expectancy.at.Birth..Males)
  subregion_min <- min(subregion_df$Life.Expectancy.at.Birth..Males)
  subregion_max <- max(subregion_df$Life.Expectancy.at.Birth..Males)
  
  # Save the results in individual variables for each subregion
  # Create a data frame to store the results of this subregion
  subregion_results <- data.frame(
    subregion = subregion,
    mean = round(subregion_mean,2),
    median = round(subregion_median,2),
    var = round(subregion_var,2),
    iqr = round(subregion_iqr,2),
    min = round(subregion_min,2),
    max = round(subregion_max,2)
    
    
  )
  
  # Append the results to the overall results data frame
  results_df_le_male <- rbind(results_df_le_male, subregion_results)
}

results_df_le_male



# Life.Expectancy.at.Birth..Female
# Create an empty data frame to store the results
results_df_le_female <- data.frame()
# Subset the data for the Africa region
africa_df <- subset(Census_2022, Region == "Africa")

# Get unique subregions in Africa
africa_subregions <- unique(africa_df$Subregion)

# Loop through each subregion and calculate the descriptive statistics
for (subregion in africa_subregions) {
  subregion_df <- subset(africa_df, subregion == Subregion)
  subregion_mean <- mean(subregion_df$Life.Expectancy.at.Birth..Females)
  subregion_median <- median(subregion_df$Life.Expectancy.at.Birth..Females)
  subregion_var <- var(subregion_df$Life.Expectancy.at.Birth..Females)
  subregion_iqr <- IQR(subregion_df$Life.Expectancy.at.Birth..Females)
  subregion_min <- min(subregion_df$Life.Expectancy.at.Birth..Females)
  subregion_max <- max(subregion_df$Life.Expectancy.at.Birth..Females)
  
  # Save the results in individual variables for each subregion
  # Create a data frame to store the results of this subregion
  subregion_results <- data.frame(
    subregion = subregion,
    mean = round(subregion_mean,2),
    median = round(subregion_median,2),
    var = round(subregion_var,2),
    iqr = round(subregion_iqr,2),
    min = round(subregion_min,2),
    max = round(subregion_max,2)
    
    
  )
  
  # Append the results to the overall results data frame
  results_df_le_female <- rbind(results_df_le_female, subregion_results)
}

results_df_le_female



#data for question 4 for 2002 and 2022
# Create a vector of regions to loop through
regions <- c("Africa", "Americas", "Asia", "Europe", "Oceania")

# Create an empty data frame to store the results
results_both <- data.frame(Region = character(),
                           LifeExpectancyMean = numeric(),
                           LifeExpectancyMedian = numeric(),
                           LifeExpectancyMax = numeric(),
                           LifeExpectancyMin = numeric(),
                           LifeExpectancySD = numeric(),
                           MortalityMean = numeric(),
                           MortalityMedian = numeric(),
                           MortalityMax = numeric(),
                           MortalityMin = numeric(),
                           MortalitySD = numeric())

# Loop through each region, calculate statistics, and add to the results data frame
for (i in 1:length(regions)) {
  region <- regions[i]
  life_expect_both_sex <- filter(demographic, Year == 2022, Region == region)$Life.Expectancy.at.Birth..Both.Sexes
  mort_both_sex <- filter(demographic, Year == 2022, Region == region)$Under.Age.5.Mortality..Both.Sexes
  
  results_both[i, "Region"] <- region
  results_both[i, "LifeExpectancyMean"] <- round(mean(life_expect_both_sex), 2)
  results_both[i, "LifeExpectancyMedian"] <- round(median(life_expect_both_sex), 2)
  results_both[i, "LifeExpectancyMax"] <- round(max(life_expect_both_sex), 2)
  results_both[i, "LifeExpectancyMin"] <- round(min(life_expect_both_sex), 2)
  results_both[i, "LifeExpectancySD"] <- round(sd(life_expect_both_sex), 2)
  results_both[i, "MortalityMean"] <- round(mean(mort_both_sex), 2)
  results_both[i, "MortalityMedian"] <- round(median(mort_both_sex), 2)
  results_both[i, "MortalityMax"] <- round(max(mort_both_sex), 2)
  results_both[i, "MortalityMin"] <- round(min(mort_both_sex), 2)
  results_both[i, "MortalitySD"] <- round(sd(mort_both_sex), 2)
}

# Display the results table
print(results_both)

#for question 4
# Create an empty data frame to store the results
results_both_2002 <- data.frame(Region = character(),
                                LifeExpectancyMean = numeric(),
                                LifeExpectancyMedian = numeric(),
                                LifeExpectancyMax = numeric(),
                                LifeExpectancyMin = numeric(),
                                LifeExpectancySD = numeric(),
                                MortalityMean = numeric(),
                                MortalityMedian = numeric(),
                                MortalityMax = numeric(),
                                MortalityMin = numeric(),
                                MortalitySD = numeric())

# Loop through each region, calculate statistics, and add to the results data frame
for (i in 1:length(regions)) {
  region <- regions[i]
  life_expect_both_sex <- filter(demographic, Year == 2002, Region == region)$Life.Expectancy.at.Birth..Both.Sexes
  mort_both_sex <- filter(demographic, Year == 2002, Region == region)$Under.Age.5.Mortality..Both.Sexes
  
  results_both_2002[i, "Region"] <- region
  results_both_2002[i, "LifeExpectancyMean"] <- round(mean(life_expect_both_sex), 2)
  results_both_2002[i, "LifeExpectancyMedian"] <- round(median(life_expect_both_sex), 2)
  results_both_2002[i, "LifeExpectancyMax"] <- round(max(life_expect_both_sex), 2)
  results_both_2002[i, "LifeExpectancyMin"] <- round(min(life_expect_both_sex), 2)
  results_both_2002[i, "LifeExpectancySD"] <- round(sd(life_expect_both_sex), 2)
  results_both_2002[i, "MortalityMean"] <- round(mean(mort_both_sex), 2)
  results_both_2002[i, "MortalityMedian"] <- round(median(mort_both_sex), 2)
  results_both_2002[i, "MortalityMax"] <- round(max(mort_both_sex), 2)
  results_both_2002[i, "MortalityMin"] <- round(min(mort_both_sex), 2)
  results_both_2002[i, "MortalitySD"] <- round(sd(mort_both_sex), 2)
}

# Display the results table
print(results_both_2002)



# Create an empty data frame to store the results
results_males <- data.frame(Region = character(),
                            LifeExpectancyMean = numeric(),
                            LifeExpectancyMedian = numeric(),
                            LifeExpectancyMax = numeric(),
                            LifeExpectancyMin = numeric(),
                            LifeExpectancySD = numeric(),
                            MortalityMean = numeric(),
                            MortalityMedian = numeric(),
                            MortalityMax = numeric(),
                            MortalityMin = numeric(),
                            MortalitySD = numeric())

# Loop through each region, calculate statistics, and add to the results data frame
for (i in 1:length(regions)) {
  region <- regions[i]
  life_expect_males <- filter(demographic, Year == 2022, Region == region)$Life.Expectancy.at.Birth..Males
  mort_males <- filter(demographic, Year == 2022, Region == region)$Under.Age.5.Mortality..Males
  
  
  results_males[i, "Region"] <- region
  results_males[i, "LifeExpectancyMean"] <- round(mean(life_expect_males), 2)
  results_males[i, "LifeExpectancyMedian"] <- round(median(life_expect_males), 2)
  results_males[i, "LifeExpectancyMax"] <- round(max(life_expect_males), 2)
  results_males[i, "LifeExpectancyMin"] <- round(min(life_expect_males), 2)
  results_males[i, "LifeExpectancySD"] <- round(sd(life_expect_males), 2)
  results_males[i, "MortalityMean"] <- round(mean(mort_males), 2)
  results_males[i, "MortalityMedian"] <- round(median(mort_males), 2)
  results_males[i, "MortalityMax"] <- round(max(mort_males), 2)
  results_males[i, "MortalityMin"] <- round(min(mort_males), 2)
  results_males[i, "MortalitySD"] <- round(sd(mort_males), 2)
}

# Display the results_males table
print(results_males)




# Create an empty data frame to store the results
results_males_2002 <- data.frame(Region = character(),
                            LifeExpectancyMean = numeric(),
                            LifeExpectancyMedian = numeric(),
                            LifeExpectancyMax = numeric(),
                            LifeExpectancyMin = numeric(),
                            LifeExpectancySD = numeric(),
                            MortalityMean = numeric(),
                            MortalityMedian = numeric(),
                            MortalityMax = numeric(),
                            MortalityMin = numeric(),
                            MortalitySD = numeric())

# Loop through each region, calculate statistics, and add to the results data frame
for (i in 1:length(regions)) {
  region <- regions[i]
  life_expect_males <- filter(demographic, Year == 2002, Region == region)$Life.Expectancy.at.Birth..Males
  mort_males <- filter(demographic, Year == 2002, Region == region)$Under.Age.5.Mortality..Males
  
  
  results_males_2002[i, "Region"] <- region
  results_males_2002[i, "LifeExpectancyMean"] <- round(mean(life_expect_males), 2)
  results_males_2002[i, "LifeExpectancyMedian"] <- round(median(life_expect_males), 2)
  results_males_2002[i, "LifeExpectancyMax"] <- round(max(life_expect_males), 2)
  results_males_2002[i, "LifeExpectancyMin"] <- round(min(life_expect_males), 2)
  results_males_2002[i, "LifeExpectancySD"] <- round(sd(life_expect_males), 2)
  results_males_2002[i, "MortalityMean"] <- round(mean(mort_males), 2)
  results_males_2002[i, "MortalityMedian"] <- round(median(mort_males), 2)
  results_males_2002[i, "MortalityMax"] <- round(max(mort_males), 2)
  results_males_2002[i, "MortalityMin"] <- round(min(mort_males), 2)
  results_males_2002[i, "MortalitySD"] <- round(sd(mort_males), 2)
}

# Display the results_males table
print(results_males_2002)



# Create an empty data frame to store the results
#2022
results_females <- data.frame(Region = character(),
                              LifeExpectancyMean = numeric(),
                              LifeExpectancyMedian = numeric(),
                              LifeExpectancyMax = numeric(),
                              LifeExpectancyMin = numeric(),
                              LifeExpectancySD = numeric(),
                              MortalityMean = numeric(),
                              MortalityMedian = numeric(),
                              MortalityMax = numeric(),
                              MortalityMin = numeric(),
                              MortalitySD = numeric())

# Loop through each region, calculate statistics, and add to the results data frame
for (i in 1:length(regions)) {
  region <- regions[i]
  life_expect_females <- filter(demographic, Year == 2022, Region == region)$Life.Expectancy.at.Birth..Females
  mort_females <- filter(demographic, Year == 2022, Region == region)$Under.Age.5.Mortality..Females
  
  
  results_females[i, "Region"] <- region
  results_females[i, "LifeExpectancyMean"] <- round(mean(life_expect_females), 2)
  results_females[i, "LifeExpectancyMedian"] <- round(median(life_expect_females), 2)
  results_females[i, "LifeExpectancyMax"] <- round(max(life_expect_females), 2)
  results_females[i, "LifeExpectancyMin"] <- round(min(life_expect_females), 2)
  results_females[i, "LifeExpectancySD"] <- round(sd(life_expect_females), 2)
  results_females[i, "MortalityMean"] <- round(mean(mort_females), 2)
  results_females[i, "MortalityMedian"] <- round(median(mort_females), 2)
  results_females[i, "MortalityMax"] <- round(max(mort_females), 2)
  results_females[i, "MortalityMin"] <- round(min(mort_females), 2)
  results_females[i, "MortalitySD"] <- round(sd(mort_females), 2)
}

# Display the results_males table
print(results_females)



# Create an empty data frame to store the results
results_females_2002 <- data.frame(Region = character(),
                              LifeExpectancyMean = numeric(),
                              LifeExpectancyMedian = numeric(),
                              LifeExpectancyMax = numeric(),
                              LifeExpectancyMin = numeric(),
                              LifeExpectancySD = numeric(),
                              MortalityMean = numeric(),
                              MortalityMedian = numeric(),
                              MortalityMax = numeric(),
                              MortalityMin = numeric(),
                              MortalitySD = numeric())

# Loop through each region, calculate statistics, and add to the results data frame
for (i in 1:length(regions)) {
  region <- regions[i]
  life_expect_females <- filter(demographic, Year == 2002, Region == region)$Life.Expectancy.at.Birth..Females
  mort_females <- filter(demographic, Year == 2002, Region == region)$Under.Age.5.Mortality..Females
  
  
  results_females_2002[i, "Region"] <- region
  results_females_2002[i, "LifeExpectancyMean"] <- round(mean(life_expect_females), 2)
  results_females_2002[i, "LifeExpectancyMedian"] <- round(median(life_expect_females), 2)
  results_females_2002[i, "LifeExpectancyMax"] <- round(max(life_expect_females), 2)
  results_females_2002[i, "LifeExpectancyMin"] <- round(min(life_expect_females), 2)
  results_females_2002[i, "LifeExpectancySD"] <- round(sd(life_expect_females), 2)
  results_females_2002[i, "MortalityMean"] <- round(mean(mort_females), 2)
  results_females_2002[i, "MortalityMedian"] <- round(median(mort_females), 2)
  results_females_2002[i, "MortalityMax"] <- round(max(mort_females), 2)
  results_females_2002[i, "MortalityMin"] <- round(min(mort_females), 2)
  results_females_2002[i, "MortalitySD"] <- round(sd(mort_females), 2)
}

# Display the results_males table
print(results_females_2002)









