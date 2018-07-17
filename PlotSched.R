# Current directory
setwd("/home/leiria/dynamicEasyBackfill/Results")

# Dataframe
df_scheduling <- read.csv(batsim_file, nrows = nrows)

# Vectors
job_id <-  unlist(df_scheduling["job_id"], use.names = FALSE)
starting_time <-  unlist(df_scheduling["starting_time"], use.names = FALSE)
finish_time <- unlist(df_scheduling["finish_time"], use.names = FALSE)
allocated_processors <- unlist(df_scheduling["allocated_processors"], use.names = FALSE)

# Nodes filtering
starting_node <- 0
ending_node <- 0
for(i in 1:NROW(allocated_processors)){
  if(grepl("-", allocated_processors[i])){
    starting_node[i] <- strtoi(unlist(lapply(strsplit(as.character(allocated_processors[i]), "-"), function(x) x[1]), use.names=FALSE), base = 0L)
    ending_node[i] <- strtoi(unlist(lapply(strsplit(as.character(allocated_processors[i]), "-"), function(x) x[2]), use.names=FALSE), base = 0L)
  }else{
    starting_node[i] <- strtoi(allocated_processors[i])
    ending_node[i] <- strtoi(allocated_processors[i])
  }
}

# Plot
ggplot_dataframe = data.frame(x1 = starting_time, x2 = finish_time, y1 = starting_node, y2 = ending_node, r = job_id)
p <- ggplot() + 
                scale_x_continuous(name = "Time (Minutes)") + 
                scale_y_continuous(name = "Resources (Nodes)") +
                geom_rect(data = ggplot_dataframe, mapping = aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2), color = "black", alpha = 0.5) +
                geom_text(data = ggplot_dataframe, aes(x = x1 + (x2 - x1)/2, y = y1 + (y2 - y1)/2, label = r), size = 3)
p + ggtitle("Jobs scheduling")
