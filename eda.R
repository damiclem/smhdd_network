library(dplyr)
library(reshape2)
library(ggplot2)
library(tidyverse)

# Import dataframe
users <- read.csv("data/users.csv", stringsAsFactors=FALSE)
positions <- read.csv("data/positions.csv", stringsAsFactors=FALSE)
friendship <- read.csv("data/friendship.csv", stringsAsFactors=FALSE)


# Number of Universities by country
####################################
country <- as.data.frame(table(users$country))
country <- country[country$Freq>2,]


country %>%
  arrange(desc(Freq)) %>%    
  mutate(name=factor(Var1, levels=Var1)) %>% 
  ggplot(aes(x=name, y=Freq)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=Freq), vjust=-0.3, size=3.5)+
  labs(x = "Countries")+
  labs(y = "Frequency")+
  labs(title = "Universities by Country")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

####################################


# Number of Universities by language
####################################
language <- as.data.frame(table(users$language))
language <- language[language$Freq>2,]


language %>%
  arrange(desc(Freq)) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(name=factor(Var1, levels=Var1)) %>% 
  ggplot(aes(x=name, y=Freq)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=Freq), vjust=-0.3, size=3.5)+
  labs(x = "Languages")+
  labs(y = "Frequency")+
  labs(title = "Universities by Language")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

####################################


# Cities with more universities
####################################
cities <- as.data.frame(table(positions$location))
cities <- cities[cities$Freq>2,]

cities %>%
  arrange(desc(Freq)) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(name=factor(Var1, levels=Var1)) %>% 
  ggplot(aes(x=name, y=Freq)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=Freq), vjust=-0.3, size=3.5)+
  labs(x = "Cities")+
  labs(y = "Frequency")+
  labs(title = "Universities by City")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

####################################


# COMPUTING IN-DEGREE AND OUT_DEGREE
####################################
# Filter friendship by known users
friendship <- friendship %>%
  inner_join(
    users %>% select(id),
    by=c('to' = 'id'),
    suffix=c('.from', '.to'), 
    rename(id.from = id, id.to = to))


# Universities out-degree
outdegree <- friendship %>%
  group_by(from) %>%
  summarise(unique = n())

# Universities in-degree
indegree <- friendship %>%
    group_by(to) %>%
    summarise(unique = n())

# Merge with users
users <- merge(x=users, y=outdegree, by.x="id",by.y="from", all.x = TRUE)
users <- merge(x=users, y=indegree, by.x="id",by.y="to", all.x = TRUE)
names(users)[names(users) == "unique.x"] <- "outdegree"
names(users)[names(users) == "unique.y"] <- "indegree"

####################################


# Universities with highest and lowest in/out -degree
####################################

# Remove Universities without account
users <- users[!is.na(users$id),]

# Create df with indegree and reorder it
indegree <- users %>% select(university, indegree)
indegree <- indegree %>% arrange(desc(indegree)) 

# Create df with outdegree and reorder it
outdegree <- users %>% select(university, outdegree)
outdegree <- outdegree %>% arrange(desc(outdegree))


# Universities with indegree 0
ind.0 <- indegree[is.na(indegree$indegree),]
length(ind.0$university)
ind.0


# Universities with outdegree 0
out.0 <- outdegree[is.na(outdegree$outdegree),]
length(out.0$university)
out.0

# Top 10 Universities by Followers
indegree <- indegree[1:10,]
indegree[7,1] <-"MIT"

indegree %>%
  arrange(desc(indegree)) %>%    
  mutate(name=factor(university, levels=university)) %>% 
  ggplot(aes(x=name, y=indegree)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=indegree), vjust=0.5, size=3.5, hjust=-0.3)+
  labs(x = "Univeristies")+
  labs(y = "Followers")+
  labs(title = "Top10 Universities by Followers")+
  coord_flip()


# Top 10 Universities by Friends
outdegree <- outdegree[1:10,]

outdegree %>%
  arrange(desc(outdegree)) %>%    
  mutate(name=factor(university, levels=university)) %>% 
  ggplot(aes(x=name, y=outdegree)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=outdegree), vjust=0.5, size=3.5, hjust=-0.3)+
  labs(x = "Univeristies")+
  labs(y = "Friends")+
  labs(title = "Top10 Universities by Friends")+
  coord_flip()


####################################


# In/Out -degree by country
####################################

# FIll NAs with 0
users$indegree[is.na(users$indegree)] <- 0
users$outdegree[is.na(users$outdegree)] <- 0

# Indegree and outdegree by country
a <- users %>% group_by(country) %>% mutate(country_in=sum(indegree)) %>% distinct(country, country_in)
b <- users %>% group_by(country) %>% mutate(country_out=sum(outdegree)) %>% distinct(country, country_out)


a <- a %>% inner_join(
            country,
            by=c('country' = 'Var1')) %>% inner_join(b,
                                                     by=c('country' = 'country'))

# taking average by country
a$avg_in <- a$country_in/a$Freq
a$avg_out <- a$country_out/a$Freq

# keep only interesting values
a <- a %>% select(country, avg_in, avg_out)

colnames(a) <- c("country","in", "out")

# reshape
a <- melt(a, id.vars="country")


#plot
ggplot(data=a, aes(x=country, y=value, fill=variable)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=variable), vjust=-0.3, color="black",
            position = position_dodge(0.9))+
  scale_fill_brewer(palette="Paired")+
  labs(x = "Countries")+
  labs(y = "Average Degree")+
  labs(title = "Degree by Country")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  





####################################


# In/Out -degree distribution
####################################
# indegree
ggplot(users, aes(x=indegree)) + 
  geom_histogram(color="black", fill="steelblue", bins=20)+
  geom_vline(aes(xintercept=mean(indegree)),
            color="red", linetype="dashed", size=1)+
  labs(x = "Indegree")+
  labs(y = "Count")+
  labs(title = "Indegree distributions")

# outdegree
ggplot(users, aes(x=outdegree)) + 
  geom_histogram(color="black", fill="steelblue", bins=20)+
  geom_vline(aes(xintercept=mean(indegree)),
             color="red", linetype="dashed", size=1)+
  labs(x = "Outdegree")+
  labs(y = "Count")+
  labs(title = "Outdegree distributions")



####################################




