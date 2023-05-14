#ScatterPlot

pairs(~Homicide+Sexual+Burglary+Theft+Robbery,data=means_df_2014,
      main="Scatterplot Matrix")

#correlation matrix

corr_df <-df3_new %>% select(-c('Town'))
corr_df <-corr_df %>% select(-c('crime_month'))
corr_df <-corr_df %>% select(-c('crime_year'))
corr_df <-corr_df %>% select(-c('Number of Admin Finalised Unsuccessful'))
corr_df = corr_df[,!grepl("_us$",names(corr_df))]

corr <- round(cor(corr_df[, unlist(lapply(corr_df, is.numeric))]),1)
head(corr[, 1:5])
corr

install.packages('ggcorrplot')
library(ggcorrplot)
p.mat <- cor_pmat(corr)
head(p.mat[, 1:4])
#Correlation Plot
ggcorrplot(corr, hc.order = TRUE, type = "upper",
           outline.col = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"))

library(sqldf)
#Creating the dataset
total_count <- sqldf("select sum(Count_of_crime)'total_count',crime_type,crime_year from 
                   new_final_crime_subset group by crime_year,crime_type order by crime_year")

#Converting character to factor
total_count$crime_type <- as.factor(total_count$crime_type)

#Linear model fit
fit <- lm(crime_year ~  total_count+crime_type, data = total_count)
summary(fit)


lm(formula = total_count ~ , data = total_count)

summary(lm)

new <- data.frame(crime_type=as.factor(c('Homicide')))

predict(fit, newdata=new)

#Plot of residual
plot(fit$residuals, pch = 16, col = "red")

par(mfrow=c(1,1))

plot(fit)


