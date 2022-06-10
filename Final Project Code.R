library(tidyverse)

d1 <- read_csv("/Users/Courtney/Desktop/Programming in R/Final Project/d1.csv")
diabetes <- read.csv("/Users/Courtney/Desktop/Programming in R/Final Project/diabetes.csv")
head(d1)
summary(d1)
str(d1)


## changing outcome to Yes and No for some analysis ##
d1$Outcome <- as.character(d1$Outcome)


    ##Checking Correlations##
library(GGally)

d1_sub <- d1 %>%
  dplyr::select(Pregnancies,Glucose, BloodPressure, 
                SkinThickness, Insulin, BMI, Age)

##########
GGally::ggcorr(d1_sub, label = T) +
  ggtitle("Visualizing Correlation Matrix as a Heatmap") +
  theme(plot.title=element_text(hjust=0.5))

    ##Scatterplots##
GGally::ggpairs(d1_sub) +
  ggtitle("More Informative Scatterplot Matrix") + 
  theme(plot.title=element_text(hjust=0.5))


###Insert Scatterplot######


##############


####BAR PLOT for Outcome
ggplot(d1,aes(x=Outcome)) +
  geom_bar(fill = c("darkseagreen2", "darkseagreen")) +
  labs(x= "Outcome",
       y= "Count",
       title = "Bar Plot for Having Diabetes or Not") + 
  theme_light() +
  theme(plot.title=element_text(hjust=0.50))


########Histogram before and after imputation#########
ggplot(diabetes) +
  geom_histogram(mapping=aes(x=BloodPressure),fill="lightskyblue2")+
  theme_light()+
  labs(title = "Histogram for Blood Pressure Before Imputation to Median",
       x = "Blood Pressure (mmHG)",
       y= "Count") +
  theme(plot.title=element_text(hjust=0.50))


ggplot(d1) +
  geom_histogram(mapping=aes(x=BloodPressure),fill="lightskyblue2") +
  theme_light() +
labs(title = "Histogram for Blood Pressure After Imputation to Median",
     x = "Blood Pressure (mmHG)",
     y= "Count")+
  theme(plot.title=element_text(hjust=0.50))
###################################################


d1$Outcome <- as.numeric(d1$Outcome)
##Actual Logistic Regression
lmodel <- glm(Outcome ~ SkinThickness + Pregnancies + Glucose + BMI + Age + BloodPressure, data = d1, family = "binomial")

summary(lmodel)

#####ROC
my_roc <- ROSE::roc.curve(lmodel$y,fitted(lmodel),plotit = T)
my_roc 
##area under the curve .837

#####Pseduo r^2
DescTools::PseudoR2(lmodel,which="McKelveyZavoina")

lmodel %>% 
  broom::tidy(conf.int=T,exponentiate=T)

#interpreting Pregnancies: As Pregnancies increases by 1 point, the odds of having
##diabetes increases by ((1.118-1)*100 = 11.8% ) 11.8%


plot <- lmodel %>%
  broom::tidy(conf.int=T,exponentiate=T) %>%
  dplyr::mutate(Sig = if_else(p.value < 0.05,"Significant",
                              "Non-Significant")) %>%
  ggplot(aes(x=term,y=estimate)) +
  geom_errorbar(aes(ymin=conf.low,ymax=conf.high,color=factor(Sig)),width=0.1) +
  geom_point(aes(color=factor(Sig))) +
  geom_hline(aes(yintercept = 1),color="black",linetype="dashed") +
  labs(x = "Variable",
       y = "Estimated Exponentiated Coefficient",
       color = "Statistical \n Significance") +
  theme_classic() + coord_flip() +
  ggtitle("Confidence Intervals for Logistic Regression Analysis") +
  theme(plot.title = element_text(hjust=0.5))

plot

library(plotly)
plotly::ggplotly(plot)



car::vif(lmodel) ##VIF good


################
set.seed(12345)

d1$Outcome <- as.numeric(d1$Outcome)

##70% of data in training set: fit the model to training
##30% of data in test set: use training model to predict diabetes on my test set
samp <- sample(c("train","test"), nrow(d1), replace = T, prob = c(.7, .3))

d_train <- d1[samp == "train",] #only keep rows where samp = train

d_test <- d1[samp == "test",] 

testmodel <- glm(Outcome ~ SkinThickness + Pregnancies + Glucose + BMI + Age + BloodPressure, data = d_train, family = "binomial")



ROSE::roc.curve(testmodel$y,fitted(testmodel),plotit = T) 
##.840

d_test$pred_probs <- predict.glm(testmodel,newdata=d_test,type="response")

pt <- table(d_test$Outcome,d_test$pred_probs > 0.50)
pt1 <- addmargins(pt)

write.csv(pt1, "sstable.csv",row.names = F)



#####Specificity

pt[1,1]/sum(pt[1,])
#We are 88.05% accurate when predicting someone to not have diabetes

######Sensitivity
pt[2,2]/sum(pt[2,])
##57% accurate when predicting someone to have diabetes



      ### using different cut off points##
cp <- seq(0.20,0.95,by=0.01) #potential cutoff values
sn <- vector("double",length(cp)) #sensitivity
sp <- vector("double",length(cp)) #specificity

for(i in 1:length(cp)){
  
  tab <- table(d_test$Outcome,d_test$pred_probs > cp[i]) 
  
  sp[i] <- ifelse(dim(tab)[2] == 1,0,tab[1,1]/sum(tab[1,]))
  
  sn[i] <- ifelse(dim(tab)[2] == 1,0,tab[2,2]/sum(tab[2,]))
  
}

ssplot <- ggplot() + 
  geom_line(aes(x = cp, y = sn),color='blue') + #sensitivity in blue
  geom_line(aes(x = cp, y = sp),color='red') + #specificity in red
  theme_classic() + labs(x = "Cut Points",
                         y = "Probabilities") +
  scale_x_continuous(breaks = seq(0.20,0.90,by=0.05))

ssplot





#################predicted probabilty graph?


d2 <- d1 %>% 
  dplyr::mutate(BMIlevel = case_when(BMI <= 18.5 ~ '1',
                                    BMI <= 24.9 ~ '2',
                                    BMI <= 29.9 ~ '3',
                                    BMI >= 30 ~ '4'))
d3 <- d2 %>% 
  select("BMIlevel","Pregnancies","Glucose","Outcome")


d3$BMIlevel <- factor(d3$BMIlevel)

d3$BMIlevel1 <- relevel(d3$BMIlevel,ref="4")

d3$BMIlevel1

summary(d3)


lmodel <- glm(Outcome ~  Pregnancies + Glucose + BMIlevel1 , data = d3, family = "binomial")


new_d3 <- tibble(Pregnancies = rep(3,100), #repeats 3.5 100 times
                  BMIlevel1 = rep(1,100), #repeats 1 100 times
                   Glucose= seq(200,800,length.out = 100))
new_d32 <- tibble(Pregnancies = rep(3,100),
                  BMIlevel1 = rep(2,100), #rank is at rank 2
                  Glucose = seq(200,800,length.out = 100))

new_d33 <- tibble(Pregnancies = rep(3,100),
                  BMIlevel1 = rep(3,100),
                  Glucose = seq(200,800,length.out = 100))

new_d34 <- tibble(Pregnancies = rep(3,100),
                  BMIlevel1 = rep(4,100),
                  Glucose = seq(200,800,length.out = 100))

new_d <- dplyr::bind_rows(new_d3,new_d32,new_d33,new_d34)
new_d$BMIlevel1 <- factor(new_d$BMIlevel1) 
new_d$probs <- predict.glm(lmodel,newdata=new_d,type="response")

new_d %>%
  ggplot(aes(x=Glucose,y=probs)) + #specify x and y axes
  geom_line(aes(color=BMIlevel1)) + labs(x = "Glucose",
                                     y = "Predicted Probability of Diabetes",
                                     color = "BMI Level") +
  theme_classic() + ggtitle("Predicted Probability of Diabetes",
                            subtitle = "for fixed Pregancies of 3") +
  theme(plot.title=element_text(hjust=0.50),
        plot.subtitle=element_text(hjust=0.50))

