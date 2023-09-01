library(igraph)
library(dplyr)
library(tidyr)
library(tidyverse)
library(caret)
library(pROC)
library(neuralnet)
library(httr)
library(XML)


data <- read.csv("input.csv")

# Create a graph from the data
interaction_graph <- graph_from_data_frame(data)

# Set the vertex attributes
unique_pathogen_proteins <- unique(data$pathogen_protein)
V(interaction_graph)$type <- V(interaction_graph)$name %in% unique_pathogen_proteins
V(interaction_graph)$color <- ifelse(V(interaction_graph)$type, "red", "blue")
V(interaction_graph)$label <- V(interaction_graph)$name
V(interaction_graph)$label.color <- "black"
V(interaction_graph)$label.font <- 1
V(interaction_graph)$size <- 6


# Calculate the number of nodes (vertices)
num_vertices <- vcount(interaction_graph)
cat("Number of nodes (vertices):", num_vertices, "\n")


# Calculate the number of edges
num_edges <- ecount(interaction_graph)
cat("Number of edges:", num_edges, "\n")

# Calculate the total number of possible interactions
pathogen_vertices <- V(interaction_graph)[V(interaction_graph)$type == TRUE]
host_vertices <- V(interaction_graph)[V(interaction_graph)$type == FALSE]

num_possible_interactions <- length(pathogen_vertices) * length(host_vertices)
cat("Total possible interactions:", num_possible_interactions, "\n")

# Calculate the total number of non-interactions
num_non_interactions <- num_possible_interactions - num_edges
cat("Total non-interactions:", num_non_interactions, "\n")

# Calculate the total number of possible interactions
pathogen_vertices <- V(interaction_graph)[V(interaction_graph)$type == TRUE]
host_vertices <- V(interaction_graph)[V(interaction_graph)$type == FALSE]

num_possible_interactions <- length(pathogen_vertices) * length(host_vertices)

# Calculate the total number of non-interactions
num_non_interactions <- num_possible_interactions - num_edges

# Specify the number of negative samples you want to generate
# Now the number of negative samples is equal to the number of interactions
num_negative_samples <- num_edges

#Create a negative dataset
generate_negative_dataset <- function(graph, num_negative_samples) {
  pathogen_vertices <- V(graph)[V(graph)$type == TRUE]
  host_vertices <- V(graph)[V(graph)$type == FALSE]
  
  num_possible_non_interactions <- length(pathogen_vertices) * length(host_vertices) - ecount(graph)
  num_negative_samples <- min(num_negative_samples, num_possible_non_interactions)
  
  cat("Number of pathogen vertices:", length(pathogen_vertices), "\n")
  cat("Number of host vertices:", length(host_vertices), "\n")
  cat("Number of possible non-interactions:", num_possible_non_interactions, "\n")
  cat("Number of negative samples:", num_negative_samples, "\n")
  
  negative_pairs <- data.frame(pathogen_protein = character(0), host_protein = character(0), stringsAsFactors = FALSE)
  tried_pairs <- list()
  
  while (nrow(negative_pairs) < num_negative_samples) {
    random_pathogen <- pathogen_vertices[sample.int(length(pathogen_vertices), 1)]
    random_host <- host_vertices[sample.int(length(host_vertices), 1)]
    
    if (!are_adjacent(graph, random_pathogen, random_host) && !(paste0(random_pathogen$name, random_host$name) %in% tried_pairs)) {
      negative_pairs <- rbind(negative_pairs, data.frame(pathogen_protein = random_pathogen$name, host_protein = random_host$name))
      tried_pairs <- append(tried_pairs, paste0(random_pathogen$name, random_host$name))
    }
  }
  
  return(negative_pairs)
}

# Generate the negative dataset
negative_dataset <- generate_negative_dataset(interaction_graph, num_negative_samples)



# Calculate the number of negative samples based on positive samples ratio
positive_samples <- num_edges
negative_samples_ratio <- 5
num_negative_samples <- positive_samples * negative_samples_ratio

# Generate the negative dataset
negative_dataset <- generate_negative_dataset(interaction_graph, num_negative_samples)

# Add a new column 'interaction' to the positive and negative datasets
data$interaction <- 1
negative_dataset$interaction <- 0

# Combine the positive and negative datasets
combined_dataset <- rbind(data, negative_dataset)

# Shuffle the combined dataset
set.seed(42) # For reproducibility
shuffled_indices <- sample(1:nrow(combined_dataset))
shuffled_dataset <- combined_dataset[shuffled_indices,]


# Count the number of positive and negative pairs
interaction_counts <- table(shuffled_dataset$interaction)

set.seed(42) # For reproducibility

# Split the shuffled dataset into training (80%) and independent test (20%) datasets
split_index <- createDataPartition(shuffled_dataset$interaction, p = 0.8, list = FALSE)
train_data <- shuffled_dataset[split_index, ]
independent_test_data <- shuffled_dataset[-split_index, ]

interaction_network <- igraph::as.undirected(interaction_graph)

network_features_bipartite <- data.frame(
  protein = igraph::V(interaction_network)$name,
  degree_centrality = igraph::degree(interaction_network),
  betweenness_centrality = igraph::betweenness(interaction_network),
  closeness_centrality = igraph::closeness(interaction_network),
  eigenvector_centrality = igraph::evcent(interaction_network)$vector,
  pagerank_centrality = igraph::page_rank(interaction_network)$vector,
  hub_score = igraph::hub_score(interaction_network)$vector,
  authority_score = igraph::authority_score(interaction_network)$vector,
  constraint = igraph::constraint(interaction_network),
  eccentricity = igraph::eccentricity(interaction_network)
)
network_features_pathogen <- network_features_bipartite[V(interaction_graph)$type == TRUE, ]
network_features_host <- network_features_bipartite[V(interaction_graph)$type == FALSE, ]
network_features <- rbind(network_features_pathogen, network_features_host)

# Preview the network features data frame
head(network_features)
# Merge the new network features with the shuffled dataset
shuffled_dataset <- merge(shuffled_dataset, network_features, by.x = "pathogen_protein", by.y = "protein", suffixes = c("", "_pathogen"))
shuffled_dataset <- merge(shuffled_dataset, network_features, by.x = "host_protein", by.y = "protein", suffixes = c("_pathogen", "_host"))

# Print the shuffled dataset with the new network features
head(shuffled_dataset)

# Find unique host proteins in the data
host_proteins <- unique(data$host_protein)

# Find the unique host proteins in both datasets
unique_host_proteins_shuffled <- unique(shuffled_dataset$host_protein)
unique_host_proteins_network <- unique(network_features$protein[network_features$protein %in% host_proteins])

# Check if there are any differences
missing_host_proteins <- setdiff(unique_host_proteins_shuffled, unique_host_proteins_network)
extra_host_proteins <- setdiff(unique_host_proteins_network, unique_host_proteins_shuffled)

cat("Missing host proteins in network_features:", length(missing_host_proteins), "\n")
cat("Extra host proteins in network_features:", length(extra_host_proteins), "\n")


# Function to get the feature vector for a given interaction
get_interaction_features <- function(row, features) {
  pathogen_features <- features[features$protein == row$pathogen_protein, ]
  host_features <- features[features$protein == row$host_protein, ]
  
  if (nrow(pathogen_features) == 0) {
    cat("Missing pathogen features for:", row$pathogen_protein, "\n")
  }
  if (nrow(host_features) == 0) {
    cat("Missing host features for:", row$host_protein, "\n")
  }
  
  combined_features <- cbind(pathogen_features["eigenvector_centrality"], host_features["eigenvector_centrality"])
  return(combined_features)
}

# Calculate feature vectors for each interaction in the shuffled_dataset
shuffled_dataset_features <- do.call(rbind, lapply(1:nrow(shuffled_dataset), function(i) get_interaction_features(shuffled_dataset[i,], network_features)))

# Convert the feature vectors into a data frame
shuffled_dataset_features_df <- data.frame(shuffled_dataset_features)
colnames(shuffled_dataset_features_df) <- c("pathogen_eigenvector_centrality", "host_eigenvector_centrality")

# Add the interaction column
shuffled_dataset_features_df$interaction <- shuffled_dataset$interaction

split_index <- createDataPartition(shuffled_dataset_features_df$interaction, p = 0.8, list = FALSE)
train_data <- shuffled_dataset_features_df[split_index, ]
independent_test_data <- shuffled_dataset_features_df[-split_index, ]

preprocessor <- preProcess(train_data[, -ncol(train_data)], method = "range")
train_data_normalized <- predict(preprocessor, train_data[, -ncol(train_data)])
test_data_normalized <- predict(preprocessor, independent_test_data[, -ncol(independent_test_data)])

train_data_normalized$interaction <- train_data$interaction
test_data_normalized$interaction <- independent_test_data$interaction

mlp_model <- neuralnet(
  interaction ~ .,
  data = train_data_normalized,
  hidden = c(10, 20), # Two hidden layers with 10 and 20 neurons respectively
  act.fct = "logistic",
  linear.output = FALSE,
  threshold = 0.1, # Lower threshold for convergence
  stepmax = 5e+06, # Increase the stepmax
  learningrate = 0.001 # Adjust the learning rate
)

predictions <- as.vector(compute(mlp_model, test_data_normalized[, -ncol(test_data_normalized)])$net.result)
predicted_class <- ifelse(predictions > 0.5, 1, 0)

cm <- confusionMatrix(
  as.factor(predicted_class),
  as.factor(test_data_normalized$interaction),
  positive = "1"
)

roc_obj <- roc(test_data_normalized$interaction, predictions)
auc <- auc(roc_obj)

tp <- cm$table[2, 2]
tn <- cm$table[1, 1]
fp <- cm$table[1, 2]
fn <- cm$table[2, 1]

mcc <- (tp * tn - fp * fn) / sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))

performance_matrix <- data.frame(
  Accuracy = cm$overall["Accuracy"],
  Sensitivity = cm$byClass[["Sensitivity"]],
  Specificity = cm$byClass[["Specificity"]],
  F1_Score = cm$byClass[["F1"]],
  Precision = cm$byClass[["Precision"]],
  MCC = mcc,
  AUC = auc
)

print(performance_matrix)



# Get all unique pathogen and host proteins
unique_pathogen_proteins <- unique(data$pathogen_protein)
unique_host_proteins <- unique(data$host_protein)

# Create a dataframe of all possible protein pairs
protein_pairs <- expand.grid(pathogen_protein = unique_pathogen_proteins,
                             host_protein = unique_host_proteins)

# Calculate feature vectors for each protein pair
protein_pair_features <- do.call(rbind, lapply(1:nrow(protein_pairs), function(i) get_interaction_features(protein_pairs[i,], network_features)))

# Convert the feature vectors into a data frame
protein_pair_features_df <- data.frame(protein_pair_features)
colnames(protein_pair_features_df) <- c("pathogen_eigenvector_centrality", "host_eigenvector_centrality")

# Normalize the protein pair features
protein_pair_features_normalized <- predict(preprocessor, protein_pair_features_df)

# Use the MLP model to predict interactions for all possible protein pairs
predicted_probabilities <- as.vector(compute(mlp_model, protein_pair_features_normalized)$net.result)

# Add the predicted probabilities to the protein_pairs dataframe
protein_pairs$predicted_probability <- predicted_probabilities

# Set a threshold for predicted interactions
threshold <- 0.7

# Filter the list to show only predicted interactions
predicted_interactions <- protein_pairs[predicted_probabilities > threshold,]

# Print the list of predicted interactions
print(predicted_interactions)
write.csv(predicted_interactions, file = "probabilityinteractions.csv")

pathogenprotein <- predicted_interactions$pathogen_protein
hostprotein <- predicted_interactions$host_protein


# Function to fetch GO annotations for a given protein from Uniprot
fetch_go_annotations <- function(protein) {
  base_url <- "https://www.uniprot.org/uniprot/"
  
  # Wrap GET request in tryCatch to handle errors
  response <- tryCatch({
    GET(paste0(base_url, protein, ".xml"))
  }, error = function(e) {
    print(paste("Error fetching data for protein:", protein, "\n", e))
    return(NULL)
  })
  
  # If response is NULL, return NULL
  if (is.null(response) || status_code(response) != 200) {
    return(NULL)
  }
  
  content <- rawToChar(response$content)
  xml_content <- xmlParse(content)
  
  # Check if 'go' elements exist in the response
  go_elements <- getNodeSet(xml_content, '//ns:dbReference[@type="GO"]', namespaces = c(ns='http://uniprot.org/uniprot'))
  
  
  if(length(go_elements) == 0) {
    print(paste("No GO elements found for protein: ", protein))
    return(NULL)
  }
  
  # Process GO elements if they exist
  go_annotations <- lapply(go_elements, function(go_elem) {
    tryCatch({
      term <- xpathSApply(go_elem, './ns:property[@type="term"]', xmlGetAttr, "value", namespaces = c(ns='http://uniprot.org/uniprot'))
      return(term)
    }, error = function(e) {
      print(paste("Error processing GO element for protein: ", protein))
      return(NULL)
    })
  })
  
  # Extract GO id and term
  go_annotations <- lapply(go_elements, function(go_elem) {
    go_id <- xmlGetAttr(go_elem, "id")
    go_term <- xpathSApply(go_elem, "./ns:property[@type='term']", xmlGetAttr, "value", namespaces = c(ns='http://uniprot.org/uniprot'))
    list(go_id = go_id, go_term = go_term)
  })
  
  # Convert to a data frame
  go_annotations <- do.call(rbind, go_annotations)
  go_annotations <- data.frame(protein = protein, t(go_annotations), stringsAsFactors = FALSE)
  
}


# Fetch GO annotations for all pathogen and host proteins
go_annotations <- lapply(c(pathogenprotein, hostprotein), fetch_go_annotations)

# Remove NULL from the list
go_annotations <- go_annotations[!sapply(go_annotations, is.null)]


# Bind all data frames
go_annotations <- bind_rows(go_annotations)
head(go_annotations)

# Transform the data frame to a long format
go_annotations_long <- go_annotations %>% 
  gather(key = "GO_term_number", value = "GO_term", -protein)

# Filter out rows where GO_term is NULL
go_annotations_long <- go_annotations_long %>% 
  filter(!is.na(GO_term) & GO_term != "NULL")

# Display the data
head(go_annotations_long)
# Group by protein and GO term, and count the number of rows for each group
go_term_counts <- go_annotations_long %>%
  group_by(protein, GO_term) %>%
  summarise(Count = n(), .groups = "drop")

# Select only the 'protein' and 'GO_term' columns
go_annotations_selected <- go_term_counts %>%
  dplyr::select(protein, GO_term)

# Filter out rows where GO_term is NULL or starts with "GO:"
go_annotations_selected <- go_annotations_selected %>% 
  filter(!is.na(GO_term) & !str_starts(GO_term, "GO:"))

