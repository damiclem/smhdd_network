# smhdd_network

Network analysis for Statistical Methods for High Dimensional Data course

## Dataset

Dataset of users and their friends has been retrieved through Twitter APIs. Users are, in this case, 200 Universities all around the world, selected among the best ones in year 2019 arrocding to roundranking.com. Specifically, the dataset has been downloaded from [here](http://roundranking.com/ranking/world-university-rankings.html#world-2019).

Dataset is arranged in a relational manner in two tables:

1. Users
   1. id (character): it is the unique identifier of the urser
   2. screen_name (character): it is the unique name of the user, which appears on its Twitter page
   3. location (character): describes users location, if available
   4. url (character): url to user's web page, if available
   5. description (character): description of the user, if available
   6. protected (logical): does the user have a protected (i.e. not public) profile? yes/no
   7. verified (logical): does the user have a verified profile? yes/no
   8. followers_count (numeric): number of followers, at the moment when the user has been retrieved
   9. friends_count (numeric): number of friends (i.e. followerd users), at the moment when the user has been retrieved
2. Friendship
   1. from (character): id of the user which has the friend
   2. to (character): firend of the user
  
**Note** that for every user, all their friends have been downloaded, since the APIs mada available from Twitter do not allow to filter them a priori.This means that there are much more ids in friendship with respect to the ones in the user table. Hence, inner join operator must be used on the friendship and the users table, in order to obtain friendship among users of interest only.
