library(tidyverse)
library(igraph)

load("~/Scripts/MonteCarlo_Simulations/Data/MM.RData")

for (iterations in 1:10){

  var_name <- paste0("mm",iterations)
  # Initialization of parameters
  TotalPopulation <- 10000
  TotalEdges <- 1000000
  LeftWing <- 0.5
  RightWing <- 1 - LeftWing
  ExposureRate <- 0.01
  RetweetRate <- 1
  RetweetsExposure <- 0.5
  alpha <- 5
  FinalTime <- 600

  # Generate agent attributes efficiently
  #set.seed(123)  # Set a seed for reproducibility
  agents <- tibble(
    slants = sl[[var_name]], # sample(1:2,TotalPopulation,replace=TRUE,prob=c(LeftWing,RightWing))
    index = seq(1, TotalPopulation),
    IPA = sample(50:70, TotalPopulation, replace = TRUE),
    OPA = sample(30:50, TotalPopulation, replace = TRUE),
    diff_slants = rep(0, TotalPopulation),
    retweeting_status = rep(0, TotalPopulation),
    retweet_slant = rep(0, TotalPopulation),
    retweet_exposure = rep(0, TotalPopulation),
    news_content = rep(0, TotalPopulation),
    news_exposure = rep(0, TotalPopulation),
    gain = rep(0, TotalPopulation)
  )

  g <- net[[var_name]] #sample_fitness_pl(TotalPopulation, TotalEdges, 3, 3)
  agents$indegree <- degree(g, mode = "in")

  incoming_neighbors <- agents$index %>%
    map_df(~ data.frame(node = .x, incoming_neighbor = as.numeric(neighbors(g, .x, mode = "in"))))

  agents <- agents %>%
    left_join(
      incoming_neighbors %>%
        mutate(slants = agents$slants[incoming_neighbors$incoming_neighbor]) %>%
        count(node, slants) %>%
        filter(slants == agents$slants[match(node, agents$index)]) %>%
        select(node, n),
      by = c("index" = "node")
    ) %>%
    mutate(similarity = n/indegree)

  # Preallocate data structures
  MeanTimeSeriesValues <- matrix(0, nrow = 3, ncol = FinalTime)
  Vectors <- tibble(
    news_content = rep(0, TotalPopulation),
    news_exposure = rep(0, TotalPopulation),
    retweeting_status = rep(0, TotalPopulation),
    exposed_content = rep(0, TotalPopulation),
    retweet_exposure = rep(0, TotalPopulation)
  )

  for (t in 1:FinalTime) {
    # Mean values
    MeanTimeSeriesValues[1, t] <- mean(agents$IPA)
    MeanTimeSeriesValues[2, t] <- mean(agents$OPA)
    MeanTimeSeriesValues[3, t] <- MeanTimeSeriesValues[1, t] - MeanTimeSeriesValues[2, t]

    # News Content
    agents$news_content <- sample(1:2, TotalPopulation, replace = TRUE)
    agents$news_exposure <- runif(TotalPopulation) < ExposureRate
    agents$news_content[!agents$news_exposure] <- 0

    # Retweeting Status
    Vectors$retweeting_status <- agents$retweeting_status
    Vectors$retweet_exposure <- agents$retweet_exposure
    agents$retweet_exposure <- 0
    agents$retweet_slant <- 0

    # Difference
    agents$diff_slants <- abs(agents$slants - Vectors$exposed_content)
    agents$gain <- (-3 * alpha * agents$diff_slants) + alpha
    agents$gain[!Vectors$retweet_exposure & !Vectors$news_exposure] <- 0

    # New IPA and OPA
    agents$IPA[agents$gain > 0] <- agents$IPA[agents$gain > 0] + agents$gain[agents$gain > 0]
    agents$IPA[agents$IPA>100] <- 100
    agents$OPA[agents$gain < 0] <- agents$OPA[agents$gain < 0] + agents$gain[agents$gain < 0]
    agents$OPA[agents$OPA<0] <- 0

    # Identify agents with retweeting status
    condition <- which(Vectors$retweeting_status == 1) %>%
      map_df(~ data.frame(node = .x, inneighbor = as.numeric(neighbors(g, .x, mode = "in"))))
    if (nrow(condition)>0) {
      condition <- condition %>%
        mutate(evaluate = !agents$news_exposure[inneighbor] & !Vectors$retweet_exposure[inneighbor]) %>% #& !Vectors$retweet_exposure[inneighbor]
        group_by(inneighbor) %>%
        mutate(count = sum(evaluate)) %>%
        filter(evaluate==TRUE) %>%
        mutate(random_index = sample(1:n(), size = 1)) %>%
        mutate(exposure = ifelse(row_number() != random_index, FALSE, TRUE)) %>%
        mutate(exposed_slant = agents$slants[node]) %>%
        filter(exposure==TRUE) %>%
        select(inneighbor, exposed_slant)
      # Update retweet exposure and slant for retweeting agents
      agents$retweet_exposure[condition$inneighbor] <- 1
      agents$retweet_slant[condition$inneighbor] <- condition$exposed_slant
    }

    # News Content
    Vectors$news_content <- agents$news_content
    Vectors$news_exposure <- agents$news_exposure
    agents$retweeting_status <- 0

    # Retweeting Content
    agents$news_content[agents$retweet_exposure == 1] <- agents$retweet_slant[agents$retweet_exposure == 1]

    # Exposed to News/Retweets and Retweeting
    agents$retweeting_status <- ((runif(TotalPopulation) < RetweetRate) & (agents$slants == agents$news_content) & ((agents$news_exposure == 1) | (agents$retweet_exposure == 1)))

    # News and Retweeting Content
    Vectors$exposed_content <- agents$news_content
  }

  final_values <- tibble(Affect=MeanTimeSeriesValues[3,],
                         IPA=MeanTimeSeriesValues[1,],
                         OPA=MeanTimeSeriesValues[2,],
                         Time=1:FinalTime,
                         alpha=alpha,
                         Elite="Moderate",
                         Population="Moderate",
                         iterate=iterations)

  write.csv(final_values,file=paste0(var_name,".csv"))
}
