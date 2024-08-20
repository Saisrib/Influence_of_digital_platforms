# changing variables
library(readxl)
library(dplyr)

# Read the Excel file
excel_file_path <- "C:\\Users\\saisr\\OneDrive\\Desktop\\Influence of Digital platform usage among students for Academic Performance .xlsx"
rdd <- read_excel(excel_file_path)

# Display the original column names
print(names(rdd))

# Change the variable name
# Display the original column names
print(names(rdd))

# Rename columns
names(rdd) <- c("Timestamp", "Email_address", "Age", "Studying", "Gender","Internship", "Rural_or_Urban", "Regularity", "Reliability", 
                "Digital_literacy", "Gained_skills", "Quality_content", "Interaction_with_classmates", 
                "Personalised_learning_experience", "Academic_percentage_increase", "Learning_experience", 
                "Online_discussion_participation", "Time_management", "Distractions", "Productivity", 
                "Usage_for_study_schedule", "Collaboration_with_classmates")


#install.packages("dplyr")
library(dplyr)

# Define the mapping
response_mapping <- c("Strongly Disagree" = 1, "Disagree" = 2, "Neutral" = 3, "Agree" = 4, "Strongly Agree" = 5)

# Specify the columns you want to convert
columns_to_convert <- c("Regularity", "Reliability", 
                        "Digital_literacy", "Gained_skills", "Quality_content", "Interaction_with_classmates", 
                        "Personalised_learning_experience", "Academic_percentage_increase", "Learning_experience", 
                        "Online_discussion_participation", "Time_management", "Distractions", "Productivity", 
                        "Usage_for_study_schedule", "Collaboration_with_classmates")

# Apply the mapping to the specified columns using mutate_all
rdd <- rdd %>%
  mutate(across(all_of(columns_to_convert), ~response_mapping[.]))

# Summative score calculation
numeric_columns <- c("Regularity", "Reliability", 
                     "Digital_literacy", "Gained_skills", "Quality_content", "Interaction_with_classmates", 
                     "Personalised_learning_experience", "Academic_percentage_increase", "Learning_experience", 
                     "Online_discussion_participation", "Time_management", "Distractions", "Productivity", 
                     "Usage_for_study_schedule", "Collaboration_with_classmates")

# Create a new column 'SummativeScore' with the sum of the numeric values for each row
rdd$SummativeScore <- rowSums(rdd[, numeric_columns], na.rm = TRUE)

# Print the dataframe and its structure
rdd
str(rdd)
summary(rdd)


hist(rdd$Interaction_with_classmates,xlab= "scale from 1 to 5", ylab= "density",
     main = "Histogram with Interaction with classmates."
     , prob=T, col = c("skyblue","orange3","violet","red","purple"))          
lines(density(rdd$Interaction_with_classmates),col = "black",lwd = 2)  
legend("topleft",
       legend = c("Strongly Disagree","Disagree","Nuetral","Agree","Strongly Agree"),
       col = c("skyblue","orange3","red","skyblue3","violet"),
       pch = c(19,19),
       bty = 0,
       pt.cex = 0.8,
       text.col = "black",
       horiz = F,
       inset = c(0.1,0.1))


#  Interpretation

#  Students acknowledge an improvement in their interactions with classmates due 
#  to learning from digital platforms. This indicates that engaging with digital 
#  learning has not only boosted their confidence levels but has also positively 
#  influenced the overall quality of their interactions with fellow students.


#2. Personalized learning experience. 

p=rdd$Personalised_learning_experience
p.counts=table(p)
p.percentages <- (p.counts / sum(p.counts)) * 100
names(p.counts)= c("Strongly Disagree","Disagree","Nuetral","Agree","Strongly Agree")
pie(p.counts,labels =  sprintf("%.1f%%", p.percentages),col=c("yellow","red3","pink","orange","skyblue3"),main = "Personalized Learning Experience")
colors = c("yellow", "red3", "pink", "orange", "skyblue")
legend("bottomleft", legend = names(p.counts), fill = colors, title = "Responses")


#   Interpretation

#  Students agreeing that digital platforms offer a personalized learning 
#  experience indicates their active engagement in digital learning. This 
#  involvement is seen as beneficial for improving their performance, as 
#  personalized learning caters to their individual needs and preferences.
#  In essence, the students find value in the personalized approach provided
#  by digital platforms, leading to enhanced learning outcomes.



#3. Academic performance
color = c("black","lightblue3")
a= table(rdd$Internship,rdd$Academic_percentage_increase)
barplot(a,beside = T, main ="Bar Plot for academic performances")
legend("topleft", legend = c("Without Internship","With Internship"), fill = color, title = "Values")


#   Interpretation
#    Students who work part-time jobs or internships say that using digital tools 
#    and websites has helped them do better in academic performances. These 
#    digital platforms resources make it easier for them to understand their 
#    subjects, stay organized, and get extra learning materials. So, even
#    though they're working, these digital tools help them manage everything 
#   and improve their grades in school.





#4. Learning experience comparison with Traditional methods

library(vioplot)
vioplot(rdd$Learning_experience ~ rdd$Age, col = c("lightblue","green","violet","yellow2","pink2"),main= "Violin Plot for learning experince comparison", xlab = "Age", ylab = "Scale")



# Interpretation. 

#   Majority of students, from various age groups, have found the learning 
#   experience from digital platforms beneficial, mainly because it provides a 
#   personalized learning experience tailored to individual needs. However, it's 
#   interesting to note that students aged 16 to 25 still express a preference 
#   for traditional methods of learning. This could imply that while digital
#   platforms offer advantages, some students within this age range believe 
#   that the more conventional ways of learning, such as in-person classes or
#   traditional textbooks, are superior.


#5.   Online Discussion Participation.

boxplot(rdd$Online_discussion_participation ~ rdd$Studying, data = rdd,
        main = "Boxplot for online discussion participation",
        ylab = "Scale",
        xlab = "Currently Studying",
        col= c("violet","pink","green","blue","pink"), horizontal = FALSE)

    



#   Interpretation 
#   Many students from various grades and groups actively participate in online
#   discussions or forums on digital platforms for academic purposes. This 
#   involvement is proving to be beneficial, contributing to improved learning 
#   and better academic performance. Essentially, students find that engaging 
#   in these online discussions on educational platforms helps them understand 
#   their studies better and boosts their grades.



m= mean(rdd$Regularity[rdd$Gender == "Male"])
m





#           OVERALL INTERPRETATION


#  Students say that using digital platforms for learning helps them talk 
#  with classmates better. This makes them feel more sure of themselves and 
#  improves how well they get along with others. When students agree that 
#  learning online is personal, it means they are involved in it. This is good 
#  for doing better in academic performances because personal learning is about 
#  what each student likes and needs. Students who work part-time jobs 
#  or internships find that using digital platforms tools makes their learning 
#  easier. Most students of different ages think learning from these platforms
#  is good. However, some students aged between 16 to 25 still prefer the 
#  traditional methods of learning, like in-person classes or regular books.
#  Many students from different grades and groups take part in online 
#  discussions or forums on these platforms for academic purposes. This helps 
#  them to understand their studies better and get better grades. 
#  In general, students believe that using digital platforms helps them do
#  better in their performances.




table(rdd$Age, rdd$Academic_percentage_increase)
two = aov(SummativeScore ~ Academic_percentage_increase*Age,
          data = rdd
)
two
summary(two)


# interpretation


#Two-Way ANOVA

#H01:  There is no significant difference in the academic performance
#based on summative scores

#H11: There is a significant difference in the academic performance 
# based on summative scores

#H02:  There is no significant difference in the academic performance between 
#different age groups based on summative scores


#H12:  There is a significant difference in the academic performance between 
#different age groups based on summative scores

#Score (p value)
#Academic percentage = 0.0000000000000002
#Age = 0.0126
#Age and Academic Performance = 0.9835

#In the two-way ANOVA model context, a notable impact is observed on academic 
#percentage. Nevertheless, the factors "Age" and the interaction between "Age"
#and "academic percentage increase" do not exhibit a statistically significant
#influence in this analysis based on the Summative Score. That there is means
#no relationship that is specific to age that influence in increased academic
#performance. All age groups showed a significant increase in academic performance.


one= aov(SummativeScore ~ Learning_experience,
         data = rdd )
one
summary(one)

# Interpretation 
#ONE WAY ANOVA  
#H0: Overall there is no significant difference in the academic performance 
#among students based on summative score.

#H1: Overall there is a significant difference in the academic performance 
#among students based on summative score.

#Score (P value)
#Learning experience = 0.0000000000000002
#Analysis
#The one-way ANOVA reveals a clear difference in learning outcomes between 
#traditional and modern methods. The results strongly imply that digital 
#platforms play a more significant role in influencing these outcomes than 
#traditional methods based on summative score. In conclusion, students who learn 
#using digital platforms appears to have a more noticeable effect on the results 
#compared to learning through traditional means.






# CONCLUSION

#The findings suggest that students perceive digital platforms as valuable
#tools for enhancing their learning experiences and academic performances.
#Improved communication with classmates and increased self-assurance contribute
#to a positive learning environment. The recognition of online learning as a 
#personalized and engaging experience aligns with improved academic outcomes. 
#Notably, students juggling part-time jobs or internships find digital platforms
#beneficial in managing their studies. While a majority of students, spanning 
#different age groups, embrace digital learning, a subset of individuals aged
#16 to 25 still prefers traditional methods. Active participation in online
#discussions emerges as a common practice, leading to better understanding
#and higher grades. Statistical analyses, including two-way ANOVA and one-way 
#ANOVA, support the notion that digital platforms have a more substantial 
#impact on academic performance than traditional methods. Overall, the study
#underscores the widespread positive perception of digital platforms and their
#pivotal role in shaping contemporary learning outcomes.







