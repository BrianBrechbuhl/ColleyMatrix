require("sqldf")
require("dplyr")
require("reshape2")
require("igraph")
require("ggplot2")
require("sna")

games <- read.csv("data/2015-games.csv")
teams_tournament <- read.csv("data/teams.csv")

games.tournament.teams <- sqldf('select Home, Away, hpts - 3, apts, (hpts - 3) - apts as MOV, 1 as gp   
                                from games g
                                join teams_tournament t1 on g.Home = t1.Team
                                join teams_tournament t2 on g.Away = t2.Team')

#Create Game Counts Diagonal
home <- games.tournament.teams %>%
  group_by(Home) %>% 
  summarize(home_games=n())

away <- games.tournament.teams %>%
  group_by(Away) %>%
  summarize(away_games=n())

colnames(away) <- c('Team', 'games')
colnames(home) <- c('Team', 'games')

game_counts <- left_join(home, away, by='Team')
game_counts[is.na(game_counts)] <- 0
game_counts <- mutate(game_counts, total_games = games.x + games.y)

#Calculate MOV for Home/Away
home.mov <-  group_by(games.tournament.teams, Home)
home.mov <- summarize(home.mov, home_mov=sum(MOV))

away.mov <-  group_by(games.tournament.teams, Away) 
away.mov <- mutate(away.mov, MOV = MOV * -1)
away.mov <- summarize(away.mov, away_mov=sum(MOV))

colnames(home.mov) <- c('Team', 'games')
colnames(away.mov) <- c('Team', 'games')

mov_counts <- left_join(home.mov, away.mov, by='Team')
mov_counts[is.na(mov_counts)] <- 0
mov_counts <- mutate(mov_counts, total_mov = games.x + games.y)

#Create MOV Vector
b <- mov_counts %>%
  select(Team, total_mov) %>%
  arrange(Team) %>%
  select(total_mov)

#Create adjacency Matrix
tournament.games <- select(games.tournament.teams, Home, Away, gp)
tournament.games.away <- select(games.tournament.teams, Away, Home, gp)

game.graph <- graph.data.frame(tournament.games)
game.graph.away <- graph.data.frame(tournament.games.away)

G <- get.adjacency(game.graph, sparse=FALSE)
G.away <- get.adjacency(game.graph.away, sparse=FALSE)

Team <- as.data.frame(teams.tournament)

diag_counts <- inner_join(Team, game_counts, by='Team') %>%
  select(Team, total_games)

sort.diag.game.counts <- arrange(diag_counts, Team)
diag_games <- select(sort.diag.game.counts, total_games)

#Sort Matrices
G_sort <- G[order(rownames(G)), order(colnames(G))]
G_sort.away <- G.away[order(rownames(G.away)), order(colnames(G.away))]

#Create matrix of home/away and diagonal totals
m <- (G_sort + G_sort.away) * -1 + diag(diag_games$total_games +2)

#Solve ranking <- G^-1 * b_mov_vector
r <- solve(as.matrix(m)) %*% as.matrix(b)
View(r)
write.csv(r, file='colley.csv')

plot(sort(r))
