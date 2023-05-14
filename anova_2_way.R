#Dataset preparation
data_naive_per<-new_final_crime_subset_new
data_naive_per

#one way anova
res.aov <- aov( Count_of_crime~ crime_type, data = data_naive_per)
# Summary of the analysis
summary(res.aov)

#plot of anova
plot(res.aov, 1)

#Dataset preparation
data_anova_two<-new_final_crime_subset_new 
library(dplyr)
data_anova_two = subset(data_anova_two, select = -c(crime_year) )
data_anova_two

#Sampling and converting columns as factor
set.seed(123)
dplyr::sample_n(data_anova_two, 5)
data_anova_two$city_name <- as.factor(data_anova_two$city_name)
data_anova_two$crime_type <- as.factor(data_anova_two$crime_type)


str(data_anova_two)

table(data_anova_two$city_name,data_anova_two$crime_type)
data_anova_two
install.packages("ggpubr")

#Plots
library("ggpubr")
anova_two<-ggboxplot(data_anova_two, x = "city_name", y = "Count_of_crime", color = "crime_type")
anova_two + theme(axis.text.x = element_text(angle = 45, hjust = 1))

library("ggpubr")
anova_line<-ggline(data_anova_two, x = "city_name", y = "Count_of_crime", color = "crime_type",
       add = c("mean_se", "dotplot"))
anova_line + theme(axis.text.x = element_text(angle = 45, hjust = 1))

boxplot(Count_of_crime ~ crime_type *city_name, data = data_anova_two, frame = FALSE,
        col = c("#00AFBB", "#E7B800"), ylab="Tooth Length")

#Two way anova model fit
res.aov2 <- aov(Count_of_crime ~ crime_type *city_name, data = data_anova_two)
summary(res.aov2)

