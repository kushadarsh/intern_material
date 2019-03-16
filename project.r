deliveries_data=read.csv('deliveries.csv')
summary(deliveries_data)
matches_data=read.csv('matches.csv')
summary(matches_data)
Dl_matches = matches_data[ matches_data$dl_applied == 1,]
deliveries_data_filter = deliveries_data %>%
  filter(is_super_over == 0 & !(match_id %in% Dl_matches$id)) 
#batsman level summary
Batsmen_Match_Level_Summary = deliveries_data_filter %>%
  group_by(match_id,inning,batsman) %>%
  summarize(Runs_Scored = sum(batsman_runs), Balls_Faced = length(ball[wide_runs ==0]), Fours = length(batsman_runs[batsman_runs ==4]), Sixes = length(batsman_runs[batsman_runs ==6])) %>%
  ungroup()
# Batsman dismissal mode
Batsmen_Out_Data = deliveries_data_filter %>%
  filter(player_dismissed != "") 

Batsmen_Out_Data = Batsmen_Out_Data[,c(1,2,19,20)]


colnames(Batsmen_Out_Data)[3] = c("batsman")
# Merging the 2 
Batsmen_Match_Level_Summary = merge(Batsmen_Match_Level_Summary,Batsmen_Out_Data,by=c("match_id","inning","batsman"),all.x=T)
Batsmen_Match_Level_Summary$dismissal_kind = ifelse(is.na(Batsmen_Match_Level_Summary$dismissal_kind),"NotOut",as.character(Batsmen_Match_Level_Summary$dismissal_kind))
# Filter by those who have played atleast 48 balls and create signals

Batsmen_Summary = Batsmen_Match_Level_Summary %>%
  group_by(batsman) %>%
  filter(sum(Balls_Faced) > 47) %>%
  summarize(Total_Innings = length(inning), Avg = sum(Runs_Scored)/Total_Innings, SR = 100*sum(Runs_Scored)/sum(Balls_Faced),  Avg_Fours = sum(Fours)/Total_Innings, Avg_Sixes = sum(Sixes)/Total_Innings, Thirty_Plus = length(Runs_Scored[ Runs_Scored > 29 ])/Total_Innings, Not_Out = length(dismissal_kind[ dismissal_kind %in% c('NotOut','retired hurt')])/Total_Innings) %>%
  ungroup()

summary(Batsmen_Summary)

#bowler level summary
#bowlwer credits
bowler_dismissal = c("caught","caught and bowled","bowled","lbw","stumped")
# Create match level summary

Bowler_Match_Level_Summary = deliveries_data_filter %>%
  group_by(match_id,inning,bowler) %>%
  summarize(Runs_Given = sum(total_runs), Balls = length(ball) ,  Dots = length(total_runs[total_runs ==0]), Bowler_Wickets = length(dismissal_kind[dismissal_kind %in% bowler_dismissal])) %>%
  ungroup()
# Filetr by those who have bowled atleast 8 overs and create overall bowler summary

Bowler_Summary = Bowler_Match_Level_Summary %>%
  group_by(bowler) %>%
  filter(sum(Balls) > 47) %>%
  summarize(Bowl_Total_Innings = length(inning), Bowl_Avg = sum(Runs_Given)/sum(Bowler_Wickets) , Bowl_SR = sum(Balls)/sum(Bowler_Wickets),  Bowl_Avg_Dots = sum(Dots)/Bowl_Total_Innings, Two_Plus_Wickets = length(Bowler_Wickets[ Bowler_Wickets>1])/Bowl_Total_Innings) %>%
  ungroup()
# Fielding data
Fielding_Data = deliveries_data_filter %>%
  mutate(fielder = ifelse(dismissal_kind == bowler_dismissal[2],as.character(bowler),as.character(fielder))) %>%
  filter(fielder != "") %>%
  group_by(fielder) %>%
  summarize(Catches_Taken = length(dismissal_kind[ dismissal_kind %in% bowler_dismissal[1:2]]), Run_Outs_Done = length(dismissal_kind[ dismissal_kind == c("run out")]), Stumped_Done = length(dismissal_kind[ dismissal_kind %in% bowler_dismissal[5]])) %>%
  ungroup()
# Removing Fielding data of substitue fielders as they actually didn;t play those matches

Fielding_Data = subset(Fielding_Data, !grepl("(sub)", Fielding_Data$fielder))

summary(Fielding_Data)
#merge all players to create player level summary
colnames(Batsmen_Summary)[1] = c("player")
colnames(Bowler_Summary)[1] = c("player")
colnames(Fielding_Data)[1] = c("player")
Player_Summary = merge(Batsmen_Summary,Bowler_Summary,by=c("player"),all.x=T,all.Y=T)
Player_Summary = merge(Player_Summary,Fielding_Data,by=c("player"),all.x=T)

summary(Player_Summary)
#dealing with NA's and infinite values
Player_Summary = Player_Summary %>%
  group_by(player) %>%
  mutate(Bowl_Avg = ifelse(is.infinite(Bowl_Avg) | is.na(Bowl_Avg),150,Bowl_Avg), Bowl_SR  = ifelse(is.infinite(Bowl_SR) | is.na(Bowl_SR),100,Bowl_SR))

Player_Summary[is.na(Player_Summary)]<-0

#creating final columns and final summary
Player_Summary = Player_Summary %>%
  group_by(player) %>%
  mutate(Matches_Played = max(Total_Innings,Bowl_Total_Innings), Avg_Boundaries = sum(Avg_Fours,Avg_Sixes),Avg_Catches_Taken = Catches_Taken/Matches_Played, Avg_RunOut_Done = Run_Outs_Done/Matches_Played, Avg_Stumping_Done = Stumped_Done/Matches_Played) %>%
  ungroup()


Player_Summary = Player_Summary[ ,c(1,17,3,4,18,7,8,10,11,12,13,19,20,21)]  

#CLUSTERING NOW
set.seed(1001)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}


Player_Summary_normal = as.data.frame(lapply(Player_Summary[,c(3:14)], normalize))


score_wss_normal <- (nrow(Player_Summary_normal)-1)*sum(apply(Player_Summary_normal,2,var))
for (i in 2:15) score_wss_normal[i] <- sum(kmeans(Player_Summary_normal,
                                                  centers=i)$withinss)

plot(1:15, score_wss_normal[1:15], type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Elbow method to look at optimal clusters for Normalized Data",
     pch=20, cex=2)

diff(score_wss_normal)
#By analysing the graph we take k=5 
#Normalising and denormalasing the data
minvec <- sapply(Player_Summary[,c(3:14)],min)
maxvec <- sapply(Player_Summary[,c(3:14)],max)
denormalize <- function(x,minval,maxval) {
  return(x*(maxval-minval) + minval)
}

set.seed(009)

#cluster of 5 groups
km_5_cluster_normal = kmeans(Player_Summary_normal,5,nstart = 100)  

km_5_cluster_actual = NULL
test1 = NULL

for(i in 1:length(minvec))
{
  test1 = (km_5_cluster_normal$centers[,i] * (maxvec[i] - minvec[i])) + minvec[i]
  km_5_cluster_actual = cbind(km_5_cluster_actual,test1)
}
#cluster with 4 groups
km_4_cluster_normal = kmeans(Player_Summary_normal,4,nstart = 100)  

km_4_cluster_actual = NULL
test1 = NULL

for(i in 1:length(minvec))
{
  test1 = (km_4_cluster_normal$centers[,i] * (maxvec[i] - minvec[i])) + minvec[i]
  km_4_cluster_actual = cbind(km_4_cluster_actual,test1)
}


colnames(km_4_cluster_actual) = colnames(Player_Summary[c(3:14)])
colnames(km_5_cluster_actual) = colnames(Player_Summary[c(3:14)])

print("Numbers of players in each cluster for 4 groups is given below")
km_4_cluster_normal$size
print("Numbers of players in each cluster for 5 groups is given below")
km_5_cluster_normal$size

# Adding cluster for each player bacl
Player_Summary_Mapped = cbind(Player_Summary,km_4_cluster_normal$cluster,km_5_cluster_normal$cluster)
Player_Summary_Mapped = Player_Summary_Mapped[,c(1,15,16,2:14)]

colnames(Player_Summary_Mapped)[c(2:3)] = c("Cluster_4_Group","Cluster_5_Group")


print("Distribution of players in the 2 clusters looks as follows")

table(Player_Summary_Mapped$Cluster_4_Group,Player_Summary_Mapped$Cluster_5_Group)

print("Below are the average value of all the fatures when broken into 4 cluster" )

round(km_4_cluster_actual,2)

#So the groups represent the following 

#1. Bowler: Bowl SR and Bowl Avg are good while SR and Avg are low. Along with that they ball a lot of dot balls and have high 2+ Wickets per match
#2. Weak Batsman + Wicketkeeper: SR and Avg are low but they have high Stumping and Catching percentage so Wicket keeper are part of this group
#3. Batsman + Wicketkeeper: These are best batsman group they have high Avg and SR along with that they score most boundaries and have many 30+ scores as well
#4. All Rounder: We can see they have good Avg and SR but worse that that in group 3. Also they have good Bowl SR and Bowl Avg which but again worst that group 1


print("Bowler" )
Player_Summary_Mapped$player[Player_Summary_Mapped$Cluster_5_Group ==1]

print("Batsman + Wicketkeeper" )
Player_Summary_Mapped$player[Player_Summary_Mapped$Cluster_5_Group ==5]

print("Bowling All Rounder" )
Player_Summary_Mapped$player[Player_Summary_Mapped$Cluster_5_Group ==3]

print("Batting All Rounder" )
Player_Summary_Mapped$player[Player_Summary_Mapped$Cluster_5_Group ==2]

print("Weak Batsman + Wicketkeeper" )
Player_Summary_Mapped$player[Player_Summary_Mapped$Cluster_5_Group ==4]


