dfSurvey <- read.csv("C:/Users/Administrator/Desktop/ITC255/Najiba_Haqdost_itc255.csv")
dfSurvey <- read.csv("C:/Users/Administrator/Desktop/ITC255/Najiba_Haqdost_itc255.csv")

View(Najiba_Haqdost_itc255)
dfSurvey <- (Najiba_Haqdost_itc255)

install.packages("ggplot2")
library(ggplot2)
install.packages("plotly")

#GGPLOT ASSIGNMENT 

# (1)Select a qualitative variable and plot its bar chart (gender)
gender_data <- table(dfSurvey$gender)
gender_data <- as.data.frame(gender_data)
colnames(gender_data) <- c("Gender", "Count")

ggplot(gender_data, aes(x=Gender, y=Count, fill = Gender))+
  geom_bar(stat = "identity")+
  theme_classic()+
  theme(legend.position = "")+
  theme(axis.title.x = element_text(),
        axis.title.y = element_text(face="bold", hjust= .5))+
  ggtitle("Gender Distribution")+
  geom_text(aes(label = Count), vjust=2)+
  scale_fill_manual(values = c('#ff9933', '#0000CC'))


#(2)Select a quantitative variable and plot its histogram/density and its box plot (income)


#Histogram of Income
ggplot(dfSurvey, aes(x=income))+
  geom_histogram(bins = 10, fill='#99FFFF', colour=4)+
  theme_classic()+
  theme(plot.title = element_text(face= 'bold', hjust= .5),
        axis.title.x = element_text(),
        axis.title.y = element_text())+
  ggtitle('Income Distribution')+
  xlab('Income')+
  ylab('Frequency')+
  geom_vline(xintercept = mean(dfSurvey$income),
             linetype='dashed',
             color = 'red', size=1)+
  geom_vline(xintercept = median(dfSurvey$income),
             linetype ='dashed',
             color='blue', size=1)

#Density plot of income
ggplot(dfSurvey, aes(x=income))+
  geom_density(fill="lightgreen", color="black", alpha=0.7, size= 1)+
  theme_classic()+
  theme(plot.title = element_text(face = 'bold', hjust = .5),
        axis.title = element_text(),
        axis.title.y = element_text())+
  ggtitle('Income Distribution')+
  xlab('Income')+
  ylab('Density')+
  geom_vline(xintercept = 40000,
             linetype='dashed',
             color='blue', size= 1)

#Box Plot of income 
ggplot(dfSurvey, aes(y="", x= income))+
  geom_boxplot(fill=5, color=6, alpha=0.3, outlier.colour = 'blue', linetype= 2, lwd= .6)+
  theme_classic()+
  theme(axis.title.x = element_text(),
        plot.title = element_text(face='bold', hjust = .5, color='darkgreen'))+
  ggtitle('Box Plot of Income')+
  xlab('Income')

#(3)Select a quantitative and a qualitative variable and plot their joint box plot
#QNT = INCOME, QL= GENDER

ggplot(dfSurvey, aes(x=gender, y=income, fill = gender))+
  geom_boxplot(color=2, alpha= 0.3, outlier.colour = 'blue', linetype=2, lwd= .6)+
  stat_boxplot(geom = 'errorbar', width= .5)+
  theme_gray()+
  theme(plot.title = element_text(face='bold', hjust = .5),
        axis.title.x = element_text(),
        axis.title.y = element_text(),
        legend.title = element_text(color = 'blue'),
        legend.position = "bottom")+
  ggtitle('Income by Gender')+
  xlab('Gender')+
  ylab('Income')


#(4)Select two quantitative variables and plot their scatter plot (income, age)

ggplot(dfSurvey, aes(x=age, y=income))+
  geom_point(color="blue")+
  ggtitle("Scatter Plot of Age vs Income")+
  labs(x="Age", y="Income")

#(5)Select two quantitative and one qualitative variable and create their interactive scatter plot
#QNT = INCOME, AGE, QL=GENDER

library(plotly)
g=ggplot(dfSurvey, aes(x=age, y=income, color=gender))+
  geom_point(size =3)+
  ggtitle('Income vs Age by Gender')+
  xlab('Age')+
  ylab('Income')
g

g_interactive = plotly::ggplotly(g)

g_interactive

htmlwidgets::saveWidget(g_interactive, file = 'interactiveScatterplot.html')
  
  
  


