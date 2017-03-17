
# Imports the raw data from Ordway-Swisher Biological Station in Florida, US.
 osbs <- read.csv("./data-raw/osbs.CSV", header=FALSE, col.names = c("return_distance", "intensity"), blank.lines.skip = FALSE)

# Adds index values from row names and reorganizes data frame.
 osbs$index <- as.numeric(rownames(osbs))
 osbs = osbs[,c(3, 1, 2)]

 devtools:::use_data(osbs,  overwrite = TRUE, pkg = "./data-raw")
