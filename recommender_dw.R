#Association Rules/Cross-Sell/Network Analysis Using Data Warehouse

################################################################################
#
#  Load packages
#
################################################################################
install.packages(c("arules", "arulesViz", "visNetwork", "igraph"))
library(arules)
library(arulesViz)
library(visNetwork)
library(igraph)

################################################################################
#
#  Data loading
#
################################################################################
#Load orders data
orders <- read.csv("orders_datawarehouse_file.csv", stringsAsFactors = FALSE)
head(orders)

#Format data as `transactions` object (matrix), for input into arules function
transactions_matrix <- as(split(orders[,"product_name"], 
                                orders[,"visitor_id"]
                                ), 
                          "transactions")

################################################################################
#
#  Calculate Association Rules
#
################################################################################

# Calculate associations using Apriori algorithm
# confidence: proportion of the transactions that contains X which also contains Y
# support: proportion of transactions in the database which contains the item-set
# lift: ratio of the observed support to that expected if X and Y were independent
rules <- apriori(transactions_matrix, 
                 parameter = list(support = 0.000125,
                                  confidence = 0.9,
                                  target = "rules")
                 )

#Convert to data frame for easier viewing
rules_df <- as(rules, "data.frame")
head(rules_df)
################################################################################
#
#  Data Prep for Visualization
#
################################################################################

#Use plot function from arulesViz to format data into nodes and edges
#We'll ignore `ig` plot output; use visNetwork package instead for more elegant visual
ig <- plot(rules, method="graph", control=list(type="items"))
ig_df <- get.data.frame( ig, what = "both" )

#Use short, random walk algorithm to detect product communities
cw <- cluster_walktrap(ig, steps = 4)
community <- as.character(cw$membership)

################################################################################
#
#  Visualization
#
################################################################################

#Build dataframes for nodes and edges
nodes <- data.frame(
  id = ig_df$vertices$name,
  group = community,
  shadow.size = 5,
  value = ig_df$vertices$confidence, # (lift, confidence or support)
  title = ifelse(ig_df$vertices$label == "",ig_df$vertices$name, ig_df$vertices$label),
  ig_df$vertices
)

edges <- data.frame(ig_df$edges, 
                    arrows = c("to", "from", "middle", "middle;to"))

#Create interactive visualization products using visNetwork
visNetwork(
          nodes = nodes,
          edges = ig_df$edges,
          width = "1200px",
          height = "700px"
          ) %>%
                visOptions(highlightNearest = TRUE) %>%
                visLayout(randomSeed = 1)
