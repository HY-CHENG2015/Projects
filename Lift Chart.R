
mydata <- read.csv("mydata.csv")


####Part 1.Data extraction and manipulation
## change Date format
mydata$DATE <- as.Date(mydata$DATE, format="%m/%d/%y")

## Subset to two groups to identify customer's retention
mydata.cal <- mydata %>% filter(DATE <= "1997-09-30")
mydata.val <- mydata %>% filter(DATE > "1997-09-30")

## Summarise data
# calibration group : create new variables, frequency, recency, number of purchase and average spending
mydata.cal.summarise <- mydata.cal %>%
  group_by(ID) %>% 
  summarise(monetary.cal = round(mean(DOLLARS),2),
            last_purchas.cal = max(DATE),
            number_of_purcahse.cal = round(mean(CDS),2),
            frequency.cal = length(ID)) %>%
  data.frame() %>%
  mutate(recency.cal = max(last_purchas.cal) - last_purchas.cal)

# validation group: create new variables, frequency, recency, number of purchase and average spending
mydata.val.summarise <- mydata.val %>%
  group_by(ID) %>% 
  summarise(monetary.val = round(mean(DOLLARS),2),
            last_purchas.val = max(DATE),
            number_of_purcahse.val = round(mean(CDS),2),
            frequency.val = length(ID)) %>%
  data.frame() 

# merge two groups to one group so we can identify who stays. And create "retention" variable, 1 means retention.
mydata.all <- merge(mydata.cal.summarise, mydata.val.summarise, by="ID" , all = TRUE) %>%
  mutate( retention = ifelse(is.na(monetary.val),0,1))


#### Part 2 ntile
str(mydata.cal.summarise)

## Create columns to dcile customer to 10 groups
mydata.retention <- mydata.all[,c("ID","retention")]

mydata.cal.summarise <- mydata.cal.summarise %>%
  left_join(mydata.retention, by="ID") %>%
  mutate(monetary.rank.cal = min_rank(monetary.cal),
         monetary.ntile.cal = ntile(monetary.rank.cal,10),
         recency.rank.cal = min_rank(recency.cal),
         recency.ntile.cal = ntile(recency.rank.cal,10))


## Create a column to dcile customer to 10 groups based on monetary
mydata.cal.monetary <- mydata.cal.summarise %>% 
  group_by(monetary.ntile.cal) %>%
  summarise(percentage_of_retention_rate = round((sum(retention) / length(retention))*100, 2))

# Plot retention for 10 decile
ggplot(mydata.cal.monetary, aes(x = as.factor(monetary.ntile.cal), y = percentage_of_retention_rate)) +
  geom_bar(stat="identity", width=0.9,fill="lightblue", colour="black")+
  theme_bw()+
  scale_fill_brewer()+
  geom_text(aes(label = percentage_of_retention_rate),hjust = 0.6, vjust = 0, size = 5) + 
  xlab("monetary ntile") +
  ylab("Percentage of Retention Rate")


## Create a column to dcile customer to 10 groups based on recency
mydata.cal.recency <- mydata.cal.summarise %>%
  group_by(recency.ntile.cal) %>%
  summarise(percentage_of_retention_rate = round((sum(retention) / length(retention))*100, 2))

# Plot retention for 10 decile
ggplot(mydata.cal.recency, aes(x = as.factor(recency.ntile.cal), y = percentage_of_retention_rate)) +
  geom_bar(stat="identity", width=0.9,fill="lightblue", colour="black")+
  theme_bw()+
  scale_fill_brewer()+
  geom_text(aes(label=percentage_of_retention_rate),hjust = 0.6, vjust = 0, size = 5) + 
  xlab("Recency ntile") +
  ylab("Percentage of Retention Rate")
  
  

#### Part 3 Regression Analysis
## Linear regression
lm.cal <- lm(retention~ + monetary.cal + frequency.cal + recency.cal, mydata.cal.summarise)
summary(lm.cal)


## Retention Predict
mydata.cal.summarise$retention.predict <- predict(lm.cal, mydata.cal.summarise, type = "response")


# Plot result
plot(mydata.cal.summarise$recency.cal, mydata.cal.summarise$retention.predict)
ggplot(mydata.cal.summarise, aes(x = monetary.cal , y = retention.predict)) + geom_point()
ggplot(mydata.cal.summarise, aes(x = frequency.cal , y = retention.predict)) + geom_point()


# Logistic regression
glm.cal <- glm(retention~ + monetary.cal + frequency.cal + recency.cal, mydata.cal.summarise, family = binomial)
summary(glm.cal)

mydata.cal.summarise$retention.predict.glm <- predict(glm.cal, mydata.cal.summarise, type = "response")

ggplot(mydata.cal.summarise, aes(x = monetary.cal , y = retention.predict.glm)) + geom_point()
ggplot(mydata.cal.summarise, aes(x = frequency.cal , y = retention.predict.glm)) + geom_point()




#### Part 4 Lift chart to see which variable is more important

## Lift chart for prediction
mydata.cal.summarise <- mydata.cal.summarise %>%
  mutate(retention.predict.glm.rank.cal = min_rank(retention.predict.glm),
         retention.predict.glm.ntile.cal = ntile(retention.predict.glm.rank.cal,10))

lift.chart.prediction.glm <- mydata.cal.summarise %>%
  group_by(retention.predict.glm.ntile.cal) %>%
  filter(retention >0) %>%
  summarise(Respondents = length(retention)) %>%
  data.frame() %>%
  mutate(n = 2357,
         Decile = as.factor(rank(desc(Respondents))),
         Response_Rate = round((Respondents/n),2),
         Lift = round(((Response_Rate/sum(Response_Rate)))*10,2)) %>%
  arrange(Decile) %>%
  data.frame() %>%
  mutate(Cumulative_Respondents = cumsum(Respondents),
         Cumulative_n = cumsum(n),
         Cumulative_Lift = as.numeric(round((Cumulative_Respondents/Cumulative_n)/sum(Response_Rate)*10,2))) %>%
  data.frame()


## Lift chart for Recency
lift.chart.recency <- mydata.cal.summarise %>%
  group_by(recency.ntile.cal) %>%
  filter(retention >0) %>%
  summarise(Respondents = length(retention)) %>%
  data.frame() %>%
  mutate(n = 2357,
         Decile = as.factor(rank(desc(Respondents))),
         Response_Rate = round((Respondents/n), 2),
         Lift = round(((Response_Rate/sum(Response_Rate)))*10, 2)) %>%
  arrange(Decile) %>%
  data.frame() %>%
  mutate(Cumulative_Respondents = cumsum(Respondents),
         Cumulative_n = cumsum(n),
         Cumulative_Lift = as.numeric(round((Cumulative_Respondents/Cumulative_n)/sum(Response_Rate)*10,2))) %>%
  data.frame()


## Lift chart for Monetary
lift.chart.monetary <- mydata.cal.summarise %>%
  group_by(monetary.ntile.cal) %>%
  filter(retention >0) %>%
  summarise(Respondents = length(retention)) %>%
  data.frame() %>%
  mutate(n = 2357,
         Decile = as.factor(rank(desc(Respondents))),
         Response_Rate = round((Respondents/n),2),
         Lift = round(((Response_Rate/sum(Response_Rate)))*10,2)) %>%
  arrange(Decile) %>%
  data.frame() %>%
  mutate(Cumulative_Respondents = cumsum(Respondents),
         Cumulative_n = cumsum(n),
         Cumulative_Lift = as.numeric(round((Cumulative_Respondents/Cumulative_n)/sum(Response_Rate)*10,2))) %>%
  data.frame()


prediction <- lift.chart.prediction.glm[,c("Decile", "Cumulative_Lift")] %>%
  mutate( Variable = "prediction")

monetary <- lift.chart.monetary[,c("Decile", "Cumulative_Lift")] %>%
  mutate( Variable = "monetary")

Recency <- lift.chart.recency[,c("Decile", "Cumulative_Lift")] %>%
  mutate( Variable = "recency")


lift.chart <- rbind(prediction, monetary, Recency)


# Plot results
ggplot(lift.chart, aes(x = Decile, y = Cumulative_Lift, colour = Variable, group = Variable)) + 
  geom_line(size = 1) +
  geom_point(shape=21, size=3, fill="white") +
  theme_bw() +
  ggtitle("Lift Chart") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 16)) +
  scale_y_continuous(breaks=pretty_breaks(n=15)) +
  scale_colour_manual(values=c("#000000", "#CC3300", "#0066CC")) +
  annotate("text", x = 1, y = 1.4, label = "monetary") +
  annotate("text", x = 2.5, y = 2.6, label = "prediction") +
  annotate("text", x = 1.5, y = 2.1, label = "Recency") 
