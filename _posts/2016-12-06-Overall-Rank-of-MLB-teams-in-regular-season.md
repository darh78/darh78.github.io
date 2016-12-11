Overall ranking of MLB teams in regular season (Wild Card Era)
==============================================================

Loading needed packages
-----------------------

``` r
# Loading packages
 library(Lahman)  # Data source
 library(dplyr)   # To ease the data manipulation
```

For this post, the overall rank of a team in a regular season of the MLB will be considered based on the Winning and Loss percentage of each team. So, the following variables are needed, for each seaosn, to create that `WLP` percentage: - The team name / ID; - The season; - Won games, and - Lost games;

This info is available into the `Teams` data frame of the Lahman R package. As the analysis is consiring just the Wild Card Eras, only data from 1994 will be taken:

``` r
# getting the data from lahman package: From 1995 to 2015
Teams_data <- tbl_df(Teams) %>%
  select(yearID, name, teamIDBR, W, L, DivWin, WCWin, WSWin) %>%
  filter(yearID >= 1994)
```

Let's add a variable with the Winning - Loss percentage, `WLP`, and order the tibble by `yearID` and `WLP` (in descending order).

``` r
Teams_data <- mutate(Teams_data,
        WLP = W/(W+L)) %>%
  arrange(yearID, desc(WLP))
```

Now that `Teams_data` is ordered, let's add a new variable with the overall rank (called `OverallRank`) for each team on each regular season, defining number 1 to the team with the best `WLP`, 2 for the team with the best second `WLP` and so on.

``` r
Teams_data <- mutate(Teams_data,
         OverallRank = ave(WLP, yearID, FUN = seq_along))
```

By the time of building this article, the R Lahman Package (in version 5.0-0) did not have the 2016 data. So, I went to [baseball-reference](www.baseball.reference.com) and built an excel table with this data, in order to be bound to the `Teams_data` tibble.

``` r
# get 2016 data from baseball-reference
library(readxl)  # To read Excel files into R

T2016 <- tbl_df(read_excel("C:/Users/1328/Documents/R projects/darh78.github.io/data/T2016.xlsx"))

# binding both tibbles
Teams_data <- rbind(Teams_data, T2016)
```

During this period, 4 franchises changed there names at least once, so let's standardize the ID of each team with franchises ID that played in 2016 season.

``` r
Teams_data <- mutate(Teams_data,
                  FranchID = ifelse(teamIDBR == "ANA" | teamIDBR == "CAL" | teamIDBR == "LAA", "LAA",
                                        ifelse(teamIDBR == "FLA" | teamIDBR == "MIA", "MIA",
                                               ifelse(teamIDBR == "MON" | teamIDBR == "WAS" | teamIDBR == "WSN", "WSN",
                                                      ifelse(teamIDBR == "TBD" | teamIDBR == "TBR", "TBR",
                                                             teamIDBR)
                                               )
                                        )
                      )
                  )
```

Additionally, to better prepare `Teams_data` for the analysis, let's modify some of the `classes` of the variables and give better names to some of them:

``` r
Teams_data$WLP <- as.numeric(Teams_data$WLP)
Teams_data$yearID <- as.integer(Teams_data$yearID)
Teams_data$W <- as.integer(Teams_data$W)
Teams_data$L <- as.integer(Teams_data$L)
Teams_data$OverallRank <- as.integer(Teams_data$OverallRank)
Teams_data <- Teams_data %>% rename(Season = yearID, Team = name)
```

Having the tibble on this state:

``` r
Teams_data %>% slice(c(1, 100, 200, 300, 400, 500))
```

    ## # A tibble: 6 × 11
    ##   Season                 Team teamIDBR     W     L DivWin WCWin WSWin
    ##    <int>                <chr>    <chr> <int> <int>  <chr> <chr> <chr>
    ## 1   1994       Montreal Expos      MON    74    40   <NA>  <NA>  <NA>
    ## 2   1997    Milwaukee Brewers      MIL    78    83      N     N     N
    ## 3   2000       Montreal Expos      MON    67    95      N     N     N
    ## 4   2004      Minnesota Twins      MIN    92    70      Y     N     N
    ## 5   2007  St. Louis Cardinals      STL    78    84      N     N     N
    ## 6   2010 Arizona Diamondbacks      ARI    65    97      N     N     N
    ## # ... with 3 more variables: WLP <dbl>, OverallRank <int>, FranchID <chr>

Before ploting anything, let's see a summary of the variables: So, printing a summary of the new `TeamsStd` tibble, we have:

``` r
summary(Teams_data)
```

    ##      Season         Team             teamIDBR               W         
    ##  Min.   :1994   Length:682         Length:682         Min.   : 43.00  
    ##  1st Qu.:1999   Class :character   Class :character   1st Qu.: 71.00  
    ##  Median :2005   Mode  :character   Mode  :character   Median : 80.00  
    ##  Mean   :2005                                         Mean   : 79.61  
    ##  3rd Qu.:2011                                         3rd Qu.: 89.00  
    ##  Max.   :2016                                         Max.   :116.00  
    ##        L             DivWin             WCWin              WSWin          
    ##  Min.   : 40.00   Length:682         Length:682         Length:682        
    ##  1st Qu.: 71.00   Class :character   Class :character   Class :character  
    ##  Median : 79.00   Mode  :character   Mode  :character   Mode  :character  
    ##  Mean   : 79.61                                                           
    ##  3rd Qu.: 89.00                                                           
    ##  Max.   :119.00                                                           
    ##       WLP          OverallRank      FranchID        
    ##  Min.   :0.2654   Min.   : 1.00   Length:682        
    ##  1st Qu.:0.4506   1st Qu.: 8.00   Class :character  
    ##  Median :0.5000   Median :15.00   Mode  :character  
    ##  Mean   :0.5000   Mean   :15.34                     
    ##  3rd Qu.:0.5556   3rd Qu.:23.00                     
    ##  Max.   :0.7160   Max.   :30.00

Over this period, the maximum number of Wins by a team is 116, and the maxium number of losses is 119. Let's see who were those teams:

``` r
knitr::kable(Teams_data %>%
  filter(W == 116 | L == 119) %>%
  select(Season, Team, W, L, WLP, OverallRank))
```

|  Season| Team             |    W|    L|        WLP|  OverallRank|
|-------:|:-----------------|----:|----:|----------:|------------:|
|    2001| Seattle Mariners |  116|   46|  0.7160494|            1|
|    2003| Detroit Tigers   |   43|  119|  0.2654321|           30|

These two correspond also to the minumim and maximum `WLP` in this period.

Now, let's visualize how each team overall rank has been between 1994 and 2016.

``` r
library(ggplot2) # To visualize results
library(ggthemes)# To format vizes

Linegraph <- ggplot(Teams_data, aes(x = Season, y = OverallRank)) +
  geom_line(color = "cadetblue3", size = .8) +
  scale_y_reverse(breaks = c(1,30)) +
  facet_wrap(~ FranchID, ncol = 5) +
  labs(title = "Overall rank of MLB teams in regular season",
       subtitle = "based on WLP in the Wild Card Era (since 1994)",
       caption = "Data from Lahman R package 5.0-0")+
  theme_tufte() +
  theme(axis.ticks = element_blank(),
        panel.grid.major.y = element_line(colour = "gray86", linetype = "dotted", size = 0.1),
        panel.grid.minor.y = element_blank(),
        strip.text.x = element_text(size = 10, family = "serif", face = "bold", colour = "black", angle = 0),
        axis.text.x=element_text(angle = 90, hjust = 0, vjust = 1, size = 7),
        axis.text.y=element_text(angle = 0, hjust = 1, vjust = 0.5, size = 6)) +
  scale_x_continuous(breaks = c(1994, 2000, 2005, 2010, 2016))

Linegraph
```

![](2016-12-06-Overall-Rank-of-MLB-teams-in-regular-season_files/figure-markdown_github/rank_sparklines-1.png)

This interesting sparkline-type viz shows some importante insights: - Arizona and Tampa Bay have records from 1998, when they joing the MLB; - The most consistent team over the whole period are is The New York Yankees; - Baltimore, Kansas City, Pittsburgh and Washington had very bad years for long time; - Most of the teams have ups and downs in the overall rank / WLP;

Now, let's see how these results in the overall rank allow teams to clinch the postseason and who became World Champs. First lets add a new variable, called `clinch` to know if the team

``` r
Teams_data <- mutate(Teams_data,
                     clinch = ifelse((DivWin == "Y" | WCWin == "Y") & WSWin == "N", "Clinched Playoff",
                                      ifelse(WSWin == "Y", "World Champion", NA)))
```

And let's add those results to the previous plot.

``` r
Linegraph_ps <- ggplot(Teams_data, aes(x = Season, y = OverallRank)) +
  geom_line(color = "cadetblue3", size = .8) +
  geom_point(aes(color = clinch, shape = clinch, fill = clinch), alpha = 0.9) +
  guides(fill = FALSE) +
  scale_shape_manual(name = "Regular season result",
                     breaks = c("Clinched Playoff", "World Champ"),
                     values = c(21, 19),
                     labels = c("Clinched Playoff", "Eliminated")) +
  scale_colour_manual(name = "Postseason result",
                      breaks = c("Clinched Play", "World Champion", NA),  
                      values = c("darkblue", "red3", "cadetblue3"),
                      labels = c("", "World Champion", "")) +
  scale_fill_manual(name = "Clinched Postseason",
                    breaks = c("Clinched Playoff", "World Champion", NA),  
                    values = c("white", "orange", NA)) +
  scale_y_reverse(breaks = c(1,30)) +
  facet_wrap(~ FranchID, ncol = 5) +
  labs(title = "Overall rank of MLB teams in regular season",
       subtitle = "based on WLP in Wild Card Era (since 1995)",
       caption = "Data from Lahman R package 5.0-0")+
  theme_tufte() +
  theme(axis.ticks = element_blank(),
        panel.grid.major.y = element_line(colour = "gray86", linetype = "dotted", size = 0.1),
        panel.grid.minor.y = element_blank(),
        strip.text.x = element_text(size = 10, family = "serif", face = "bold", colour = "black", angle = 0),
        axis.text.x=element_text(angle = 90, hjust = -2, vjust = 1, size = 7),
        axis.text.y=element_text(angle = 0, hjust = 1, vjust = 0.5, size = 6)) +
  scale_x_continuous(breaks = c(1994, 2000, 2005, 2010, 2016))


Linegraph_ps
```

![](2016-12-06-Overall-Rank-of-MLB-teams-in-regular-season_files/figure-markdown_github/rank_sl_ps-1.png)

``` r
#ggsave(file="Overall_Linegraph.svg", plot=Overall_Linegraph, width=8, height=5)
#ggsave(file="Overall_Linegraph.png", plot=Overall_Linegraph, width=8, height=5)
```

Note: There was no Postseason in 1994.

``` r
Teams_Champs <- Teams_data %>%
  filter(WSWin == "Y")

Histo_Rank_Champs <- ggplot(Teams_Champs) +
  geom_histogram(mapping = aes(x = OverallRank), binwidth = 1, fill = "lightskyblue4", color = "darkblue") +
  guides(color = FALSE) +
  labs(title = "Regular season overall rank of World Series Champs",
       subtitle = "Wild Card Era (since 1995)",
       caption = "Data from Lahman R package 5.0-0") +
  theme_tufte() +
  theme(axis.ticks = element_blank(),
        panel.grid.major.y = element_line(colour = "gray86", linetype = "dotted", size = 0.1),
        panel.grid.minor.y = element_blank(),
        strip.text.x = element_text(size = 10, family = "serif", face = "bold", colour = "black", angle = 0),
        axis.text.x=element_text(angle = 0, hjust = 1, vjust = 1, size = 7),
        axis.text.y=element_text(angle = 0, hjust = 1, vjust = 0.5, size = 6)) +
  scale_x_continuous(breaks = c(1:13))

Histo_Rank_Champs
```

![](2016-12-06-Overall-Rank-of-MLB-teams-in-regular-season_files/figure-markdown_github/rank_champs-1.png)
