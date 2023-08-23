## ------------------------------------------------------------ 
## Scraping 
## ------------------------------------------------------------ 
#Load up rvest library
library("rvest")

#Specifying the url for desired website to be scraped
url <- 'https://www.thecompleteuniversityguide.co.uk/league-tables/rankings?tabletype=full-table'
#Reading the HTML code of Complete University Guide website
webpage <- read_html(url)

#Using CSS selectors to scrape the rankings section
rank_data_html <- html_nodes(webpage, '.uninum')
#Converting the ranking data to text
rank_data <- html_text(rank_data_html)
#Data-Preprocessing: Converting rankings to numerical
rank_data <-as.numeric(rank_data)
# Have a look at the rankings
head(rank_data)
# Identify index of NA values and then replace them with the rank from the preceding one
indexOfNA <- which(is.na(rank_data)=="TRUE", arr.ind = TRUE)
for(i in indexOfNA){
  if(is.na(rank_data[i])=="TRUE"){
    rank_data[i]=rank_data[i-1]+1
    rank_data[i+1]=rank_data[i]
  }
}
# Double check value
which(is.na(rank_data)=="TRUE", arr.ind = TRUE)
rm(indexOfNA)
head(rank_data)

#Using CSS selectors to scrape the university name section
uni_data_html <- html_nodes(webpage, '.uni_lnk')
#Converting the data to text
uni_data <- html_text(uni_data_html)
# Have a look at the university name
head(uni_data)
#Identifying which university name got cut off
#grepl turns all values without ellipsis(...) to False, otherwise true
#which responds with the index of values that are TRUE
ellipsisIndex<-which(grepl('...', uni_data, fixed = TRUE)=="TRUE",arr.ind = TRUE)
#study each of the names with ellipsis
head(uni_data[ellipsisIndex])
rm(ellipsisIndex)
#replace them accordingly
uni_data[3] <- "London School of Economics and Political Science, University of London"
uni_data[21] <- "King's College London, University of London"
uni_data[32] <- "Royal Holloway, University of London"
uni_data[55] <- "Bristol, University of the West of England"
uni_data[88] <- "Queen Margaret University, Edinburgh"
uni_data[103] <- "University of Wales Trinity Saint David"
#double check to see if everything is maintained properly
which(grepl('...', uni_data, fixed = TRUE)=="TRUE",arr.ind = TRUE)

#Using CSS selectors to scrape the overall score section
Overall_score_html <- html_nodes(webpage, '.swiper-slide-active .bgtxt')
#Converting the data to text
Overall_score <- html_text(Overall_score_html)
#Data-Preprocessing: removing '<span class="bgtxt">'
Overall_score <-gsub('<span class="bgtxt">','',Overall_score)
#Data-Preprocessing: Converting overall score to numerical
Overall_score <-as.numeric(Overall_score)
# Have a look at the overall score
head(Overall_score)

#Using CSS selectors to scrape the entry standards section
Entry_standards_html <- html_nodes(webpage, '.swiper-slide-next .bgtxt')
#Converting the data to text
Entry_standards <- html_text(Entry_standards_html)
#Data-Preprocessing: Converting entry standards to numerical
Entry_standards<-as.numeric(Entry_standards)
# Have a look at the entry standards
head(Entry_standards)

#Using CSS selectors to scrape the student satisfaction section
Student_satisfaction_html <- html_nodes(webpage, '.rt_list3 .bgtxt')
#Converting the data to text
Student_satisfaction <- html_text(Student_satisfaction_html)
#Data-Preprocessing: removing 'n/a'
Student_satisfaction <-gsub('n/a','',Student_satisfaction)
#Data-Preprocessing: Converting student satisfaction to numerical
Student_satisfaction <-as.numeric(Student_satisfaction)
# Have a look at the student satisfaction
head(Student_satisfaction)

#Using CSS selectors to scrape the research quality section
Research_quality_html <- html_nodes(webpage, '.rt_list4 .bgtxt')
#Converting the data to text
Research_quality <- html_text(Research_quality_html)
#Data-Preprocessing: removing 'n/a'
Research_quality <-gsub('n/a','',Research_quality)
#Data-Preprocessing: Converting research quality to numerical
Research_quality <-as.numeric(Research_quality)
# Have a look at the research quality
head(Research_quality)

#Using CSS selectors to scrape the research intensity section
Research_intensity_html <- html_nodes(webpage, '.rt_list5 .bgtxt')
#Converting the data to text
Research_intensity <- html_text(Research_intensity_html)
#Data-Preprocessing: removing 'n/a'
Research_intensity <-gsub('n/a','',Research_intensity)
#Data-Preprocessing: Converting research intensity to numerical
Research_intensity <-as.numeric(Research_intensity)
# Have a look at the research intensity
head(Research_intensity)

#Using CSS selectors to scrape the acedemic services spent section
Academic_services_spend_html <- html_nodes(webpage, '.rt_list7 .bgtxt')
#Converting the data to text
Academic_services_spend <- html_text(Academic_services_spend_html)
#Data-Preprocessing: removing 'n/a'
Academic_services_spend <-gsub('n/a','',Academic_services_spend)
#Data-Preprocessing: Converting acedemic services spent to numerical
Academic_services_spend <-as.numeric(Academic_services_spend)
# Have a look at the academic services spent
head(Academic_services_spend)

#Using CSS selectors to scrape the facilities spent section
Facilities_spend_html <- html_nodes(webpage, '.rt_list8 .bgtxt')
#Converting the data to text
Facilities_spend <- html_text(Facilities_spend_html)
#Data-Preprocessing: removing 'n/a'
Facilities_spend <-gsub('n/a','',Facilities_spend)
#Data-Preprocessing: Converting facilities spent to numerical
Facilities_spend <-as.numeric(Facilities_spend)
# Have a look at the facilities spent
head(Facilities_spend)

#Using CSS selectors to scrape the degree completion section
Degree_completion_html <- html_nodes(webpage, '.rt_list10 .bgtxt')
#Converting the data to text
Degree_completion <- html_text(Degree_completion_html)
#Data-Preprocessing: removing 'n/a'
Degree_completion <-gsub('n/a','',Degree_completion)
#Data-Preprocessing: Converting degree completion to numerical
Degree_completion <-as.numeric(Degree_completion)
# Have a look at the degree completion
head(Degree_completion)

#Using CSS selectors to scrape the student and staff ratio section
Student_staff_ratio_html <- html_nodes(webpage, '.rt_list11 .bgtxt')
#Converting the data to text
Student_staff_ratio <- html_text(Student_staff_ratio_html)
#Data-Preprocessing: removing 'n/a'
Student_staff_ratio <-gsub('n/a','',Student_staff_ratio)
#Data-Preprocessing: Converting student and staff ratio to numerical
Student_staff_ratio <-as.numeric(Student_staff_ratio)
# Have a look at the student and staff ratio
head(Student_staff_ratio)

#Using CSS selectors to scrape the graduate prospect outcome section
Graduate_prospect_outcome_html <- html_nodes(webpage, '.rt_list12 .bgtxt')
#Converting the data to text
Graduate_prospect_outcome <- html_text(Graduate_prospect_outcome_html)
#Data-Preprocessing: removing 'n/a'
Graduate_prospect_outcome <-gsub('n/a','',Graduate_prospect_outcome)
#Data-Preprocessing: Converting graduate prospect outcome to numerical
Graduate_prospect_outcome <-as.numeric(Graduate_prospect_outcome)
# Have a look at the graduate prospect outcome
head(Graduate_prospect_outcome)

#Using CSS selectors to scrape the graduate prospect ontrack section
Graduate_prospect_ontrack_html <- html_nodes(webpage, '.rt_list13 .bgtxt')
#Converting the data to text
Graduate_prospect_ontrack <- html_text(Graduate_prospect_ontrack_html)
#Data-Preprocessing: removing 'n/a'
Graduate_prospect_ontrack <-gsub('n/a','',Graduate_prospect_ontrack)
#Data-Preprocessing: Converting graduate prospect ontrack to numerical
Graduate_prospect_ontrack <-as.numeric(Graduate_prospect_ontrack)
# Have a look at the graduate propect ontrack
head(Graduate_prospect_ontrack)

#Combining all the lists to form a data frame
Uni_df <- data.frame(Rank = rank_data, University = uni_data,
                     Overall_score, Entry_standards,
                     Student_satisfaction, Research_quality, Research_intensity,
                     Academic_services_spend , Facilities_spend ,
                     Degree_completion, Student_staff_ratio, Graduate_prospect_outcome,
                     Graduate_prospect_ontrack)
#Structure of the data frame
str(Uni_df)

#Convert Dataframe into CSV file
write.csv(Uni_df,file="University_ranking.csv")

## ------------------------------------------------------------ 
## Descriptive Analysis 
## ------------------------------------------------------------ 

#Figure 1: Variables in uni_df dataframe
str(Uni_df)

#Figure 2: Number of missing value in each variable
#Load up naniar library
library("naniar")
miss_var_summary(Uni_df)

#Figure 3: Rows with missing value
sum(!complete.cases(Uni_df))
Uni_df[!complete.cases(Uni_df), ]

#Figure 4: Summary statistics for each variable
#Load up psych library
library("psych")
describe(Uni_df) 

#Table 2: Box and Whisker plot for all numeric variables 
boxplot(Uni_df$Rank,
        ylab = "Rank"
)

boxplot(Uni_df$Overall_score,
        ylab = "Overall_score"
)

boxplot(Uni_df$Entry_standards,
        ylab = "Entry_standards"
)

boxplot(Uni_df$Student_satisfaction,
        ylab = "Student_satisfaction"
)

boxplot(Uni_df$Research_quality,
        ylab = "Research_quality"
)

boxplot(Uni_df$Research_intensity,
        ylab = "Research_intensity"
)

boxplot(Uni_df$Academic_services_spend,
        ylab = "Academic_services_spend"
)

boxplot(Uni_df$Facilities_spend,
        ylab = "Facilities_spend"
)

boxplot(Uni_df$Degree_completion,
        ylab = "Degree_completion"
)

boxplot(Uni_df$Student_staff_ratio,
        ylab = "Student_staff_ratio"
)

boxplot(Uni_df$Graduate_prospect_outcome,
        ylab = "Graduate_prospect_outcome"
)

boxplot(Uni_df$Graduate_prospect_ontrack,
        ylab = "Graduate_prospect_ontrack"
)

## ------------------------------------------------------------ 
## Problem Statement 1
## ------------------------------------------------------------ 
#Which Universities spend the most in resources? (stacked bar-chart)

#Identify the top 10 Universities
#Which Universities spend the most in resources? (stacked bar-chart)

#Identify the top 10 Universities
library("ggplot2")
library("dplyr")


df2 <- data.frame(
  University = c(Uni_df$University),
  Facilities_spend = c(Uni_df$Facilities_spend),
  Academic_Services_spend = c(Uni_df$Academic_services_spend)
  
)


df2$Total_spend <- df2$Facilities_spend + df2$Academic_Services_spend

df_order <- df2 %>% 
arrange(desc(Total_spend))

df_order 
df_order <- top_n(df_order, n=10)

df4 <-df_order

keeps <- c("University","Total_spend")
df5 <- df4[keeps]

df5$University <- factor(df5$University,
                         levels = df5$University[order(df5$Total_spend,decreasing= TRUE)] )

#Plotting the top 10 unis who spend the most, in order.

ggplot(df5, aes(x=University, y=Total_spend)) +                                  
  geom_bar(stat = "identity")+ geom_text(aes(label=Total_spend), size=3.5, position = position_stack(vjust = 1.05)) +
  scale_x_discrete(guide = guide_axis(angle = 60))


# stacked bar chart to find the variation of spendings.


top_10_spendings <- data.frame(spending_on=rep(c("Facility","Academic"), each=10),
                               Universities=rep(c("Harper Adams University","London Metropolitan University",
                                                  "University of Cambridge", "Imperial College London",
                                                  "University of Oxford", "University of Essex",
                                                  "University of St Andrews","Lancaster University",
                                                  "Heriot-Watt University","Durham University"),2),
                               Total_spendings=c(1939,1210,1043,755,599,1469,746,1066,1491,1085,1891,2581,2718,2982,2842,1950,2650,2319,1795,2174),
                               SUM = c(3830,3791,3761,3737,3441,3419,3396,3385,3286,3259))

top_10_spendings


# Stacked barplot
ggplot(data=top_10_spendings, aes(x=Universities, y=Total_spendings, fill=spending_on)) +
  geom_bar(stat="identity") + geom_text(aes(label=Total_spendings), size=3.5, position = position_stack(vjust = 0.5)) +
  scale_x_discrete(guide = guide_axis(angle = 60))


## ------------------------------------------------------------ 
## Problem Statement 2
## ------------------------------------------------------------ 
# Use R build in function to plot histograms 
Satisfaction_Score <- Uni_df$Student_satisfaction
h <- hist(Satisfaction_Score, main= "Histogram of Student's Satisfaction Score",
          xlab="Student's Satisfaction Score", ylim=c(0,60))
text(h$mids,h$counts,labels=h$counts,adj=c(0.5, -0.5))


Academic_spend <- Uni_df$Academic_services_spend
h1 <- hist(Academic_spend , main= "Histogram of Academic Service Spend",
           xlab="Academic Service Spend", ylim=c(0,30), xlim=c(500,3000))
text(h1$mids,h1$counts,labels=h1$counts, adj=c(0.5, -0.5))

library('ggplot2')
# Use ggplot's geom_line and aes functions (which stands for aesthetic mappings) to create a linegraph 
# The academic_service_spend variable is assigned as the y axis 
# and the student_satisfaction variable is assigned as the x axis inside the aes function 
linegraph <- ggplot(data=Uni_df, aes(x=Academic_services_spend, y=Student_satisfaction)) +
  geom_line(color="blue")+ geom_point()
# write another 'linegraph' to visualize the plot
linegraph

# use the labs function to add a title, caption and change the name of the axes
linegraph + labs(title = "Relationship between Academic_service_spend variable and Student_satisfaction variable", 
                 caption = "Data: thecompleteuniversityguide.co.uk")



## ------------------------------------------------------------ 
## Problem Statement 3
## ------------------------------------------------------------ 
#Scatter plot 
library("ggplot2")

ggplot(Uni_df, aes(x=Overall_score, y=Entry_standards)) + 
  geom_point()+
  geom_smooth(method=lm)

#Pearson correlation coefficient
cor(Overall_score, Entry_standards) 

#Regression equation
lm(Overall_score ~ Entry_standards, data = Uni_df)

#Top 10 universities with highest Entry Standard
library("dplyr")
top_10_entry <-  top_n(Uni_df, 10, Entry_standards)

ggplot(top_10_entry, aes(x = Entry_standards, y = reorder(University, -Entry_standards))) +
  geom_bar(stat = "identity" ,fill = "#FF6666") +
  geom_text(aes(label=Entry_standards), hjust=0, size=3.5) + 
  labs(y= "University", x = "Entry Standard")


## ------------------------------------------------------------ 
## Problem Statement 4
## ------------------------------------------------------------ 
#Graduate Prospects Scatter Plot
PBS4<-cbind(Graduate_prospect_ontrack,Graduate_prospect_outcome)
ggplot(data=as.data.frame(PBS4), aes(x=Graduate_prospect_ontrack,y=Graduate_prospect_outcome)) + 
  geom_point()+
  geom_smooth(method=lm)+
  ggtitle("Plot of Ontrack Prospects \n by Prospects Outcome") +
  xlab("Prospects Ontrack") + ylab("Prospects Outcome")+
  theme(
    plot.title = element_text(size=14, face="bold.italic",hjust = 0.5),
  )
#pdf(file="ProbelmStatement4.pdf")

#Uni Rankings by Graduate_Prospects Grouped Bar Chart
cor(Graduate_prospect_ontrack,Graduate_prospect_outcome)
lm(Graduate_prospect_outcome~Graduate_prospect_ontrack, as.data.frame(uni_data))
summary(lm(Graduate_prospect_outcome~Graduate_prospect_ontrack, as.data.frame(uni_data)))
rm(PBS4)

PBS4<-rbind(c(1,"Ontrack",Graduate_prospect_ontrack[1]))
PBS4<-rbind(PBS4,c(1,"Outcome",Graduate_prospect_outcome[1]))
for(i in 2:3){
  PBS4<-rbind(PBS4,c(i,"Ontrack",Graduate_prospect_ontrack[i]))
  PBS4<-rbind(PBS4,c(i,"Outcome",Graduate_prospect_outcome[i]))
}
for(i in 64:66){
  PBS4<-rbind(PBS4,c(i,"Ontrack",Graduate_prospect_ontrack[i]))
  PBS4<-rbind(PBS4,c(i,"Outcome",Graduate_prospect_outcome[i]))
}
for(i in 127:130){
  PBS4<-rbind(PBS4,c(i,"Ontrack",Graduate_prospect_ontrack[i]))
  PBS4<-rbind(PBS4,c(i,"Outcome",Graduate_prospect_outcome[i]))
}
library(forcats)
PBS4<-as.data.frame(PBS4)
dimnames(PBS4)[[2]]<-c("Rank","Prospects_Type","Percentage")
PBS4$Percentage<-as.numeric(PBS4$Percentage)
ggplot(PBS4, aes(fill=Prospects_Type, y=Percentage, x=fct_inorder(Rank))) + 
  geom_bar(position="dodge", stat="identity")+
  ggtitle("Bar Chart of Prospects by Uni Rankings") +xlab("University Rank")+
  theme(
    plot.title = element_text(size=14, face="bold.italic",hjust = 0.5),
  )
rm(PBS4)
#ggplot(PBS4, aes(fill=Prospects_Type, y=Percentage, x=Rank)) + 
#  geom_bar(position="dodge", stat="identity")

#Top 10 Uni Rankings Grouped Bar Chart
PBS5<-rbind(c(1,"Ontrack",Graduate_prospect_ontrack[1]))
PBS5<-rbind(PBS5,c(1,"Outcome",Graduate_prospect_outcome[1]))
for(i in 2:10){
  PBS5<-rbind(PBS5,c(i,"Ontrack",Graduate_prospect_ontrack[i]))
  PBS5<-rbind(PBS5,c(i,"Outcome",Graduate_prospect_outcome[i]))
}
library("forcats")
PBS5<-as.data.frame(PBS5)
dimnames(PBS5)[[2]]<-c("Rank","Prospects_Type","Percentage")
PBS5$Percentage<-as.numeric(PBS5$Percentage)
ggplot(PBS5, aes(fill=Prospects_Type, y=Percentage, x=fct_inorder(Rank))) + 
  geom_bar(position="dodge", stat="identity")+
  ggtitle("Bar Chart of Prospects by Top 10 Uni Rankings") +xlab("University Rank")+
  theme(
    plot.title = element_text(size=14, face="bold.italic",hjust = 0.5),
  )
dev.off
rm(PBS5)

## ------------------------------------------------------------ 
## Problem Statement 5
## ------------------------------------------------------------ 
#Scatter Plot
ggplot(Uni_df, aes(x=Entry_standards, y=Degree_completion)) + 
  geom_point() +
  geom_smooth(method=lm) +
  ggtitle("Plot of Degree Completion by Entry Standards") +
  xlab("Entry Standards") +
  ylab("Degree Completion") +
  theme(
    plot.title = element_text(size=14, face="bold.italic",hjust = 0.5),
  )

#Regression Analysis
cor(x=Entry_standards, y=Degree_completion, use = "everything", method = c("pearson", "kendall", "spearman"))
summary(lm(Degree_completion ~ Entry_standards, data=Uni_df))

#QQ Plot
#Entry_standards
qqnorm(Uni_df$Entry_standards, main="QQ Plot of Entry_standards")
qqline(Uni_df$Entry_standards)

#Degree_Completion
qqnorm(Uni_df$Degree_completion, main="QQ Plot of Degree_Completion")
qqline(Uni_df$Degree_completion)

#Scatter Plot with Top 10 Highlighted
top_10_entry <-  top_n(Uni_df, 10, Entry_standards)
ggplot(Uni_df, aes(x=Entry_standards, y=Degree_completion)) + 
  geom_point() +
  geom_point(data=top_10_entry,
             aes(x=Entry_standards, y=Degree_completion), 
             color='red') +
  geom_smooth(method=lm) +
  ggtitle("Plot of Degree Completion by Entry Standards \n (Top 10 Highlighted)") +
  xlab("Entry Standards") +
  ylab("Degree Completion") +
  theme(
    plot.title = element_text(size=14, face="bold.italic",hjust = 0.5),
  )




