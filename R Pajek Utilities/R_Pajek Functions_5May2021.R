#Pajek Functions: read_net, write_net, read_clu, write_clu, read_vect, write_vec
#Jonathan H. Morgan
#15 July 2019

################################
#   BASE FUNCTIONS AND LISTS   #
################################

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#Indicating to the user what network files, partition files, and vector files they have in thier directory
Net_Files = list.files(pattern="*.net")

Partition_Files = list.files(pattern="*.clu")

Vector_Files = list.files(pattern="*.vec")

################
#   read_net   #
################

print("Network Files in Your Directory")
Net_Files

#Reading in Network
read_net <- function(net_file) {
  net <- readLines(net_file)

  #Determining the length of the resulting nodes file
  vertices <- net[[1]]
  vertices <- strsplit(as.character(vertices),' ') 
  vertices <- lapply(vertices, function(x) x[x != ""])
  vertices <- as.numeric(vertices[[1]][[2]])
  
  #Need to account for the fact that meta-information is in the file
  v_num <- vertices + 1

  #Extracting the nodes list from the network file
  nodes <- net[2:v_num]
  nodes <- trim(nodes)

  nodes <- strsplit(as.character(nodes),'n/') 
  for(i in seq_along(nodes)){
    element <- strsplit(as.character(nodes[[i]]),'"')[[1]] 
    if(length(element) > 2) {
      supplemental_data <- strsplit(element[[3]],' ')[[1]] 
      supplemental_data <- supplemental_data[supplemental_data != ""]
      element <- c(element[[1]], element[[2]], supplemental_data)
      rm(supplemental_data)
    } else{
      element <- element
    }
    
    nodes[[i]] <- element
  }

  nodes <- lapply(nodes, function(x) x[x != ""])

  shapes <- nodes
  for (i in seq_along(shapes)){
    shapes[[i]] <- nodes[[i]][-c(1:5)]
  }

  shapes <- lapply(shapes, function(x) paste(x,collapse=" "))

  for (i in seq_along(nodes)){
    nodes[[i]] <- nodes[[i]][1:5]
  }

  nodes <-  as.data.frame(matrix(unlist(nodes), nrow = length(nodes), byrow = TRUE), stringsAsFactors = FALSE)
  colnames(nodes) <- c('ID', 'Label', 'x-coord', 'y-coord', 'z-coord')
  
  shapes <-  as.data.frame(matrix(unlist(shapes), nrow = length(shapes), byrow = TRUE),  stringsAsFactors = FALSE)
  colnames(shapes) <- c('shapes information')

  nodes <- cbind(nodes, shapes)

  #Removing shape information if there no information
  if(sum(nodes$`shapes information` == "") > 1){
    nodes <- nodes[-c(ncol(nodes))]
  }else{
    nodes <- nodes[,]
  }
  
  rm(shapes, i)

  #Creating Edges File
  vertices <- (vertices + 1)

  edges <- net[-c(1:vertices)]

  `Tie Type` <- edges[[1]]

  edges <- edges[-c(1)]
  edges <- trim(edges)

  edges <- strsplit(as.character(edges),' ') 
  edges <- lapply(edges, function(x) x[x != ""])

  edges <-  as.data.frame(matrix(unlist(edges), nrow = length(edges), byrow = TRUE), stringsAsFactors = FALSE)
  colnames(edges) <- c('Person i', 'Person j', 'Weight')
  
  #Removing edge color information as it adds little value when importing data
  edges <- edges[-c(4:5)]

  edges$`Tie Type` <- `Tie Type`
  
  rm(net, `Tie Type`)
  
  #writing objects to the Global Environment
  vertices <-  assign(x = "vertices", value = nodes,.GlobalEnv) 
  ties <-  assign(x = "ties", value = edges,.GlobalEnv) 
}

#read_net(Net_Files[[5]])

##################
#    write_net   #
##################

write_net <- function(tie_type, data_id, x_coord, y_coord, z_coord, 
                      node_color, node_border, 
                      `person i`, `person j`, weight, tie_color, 
                      net_name, `sort and simplify`) {
  #Generating Meta-Data
  vertices <- c('*Vertices')
  substr(tie_type, 1, 1) <- toupper(substr(tie_type, 1, 1))
  tie_type <- paste0('*', tie_type)
  
  `sort and simplify` <- as.logical(`sort and simplify`)

  #Preparing nodelist
  nodelist <- as.data.frame(data_id, stringsAsFactors = FALSE)
  id <- as.numeric(row.names.data.frame(nodelist))
  nodelist <- as.data.frame(cbind(id, nodelist))

  #Adding Coordinates data
  x <- as.character(x_coord)
  y <- as.character(y_coord)
  z <- as.character(z_coord)
  
  #Adding Color Information
  substr(node_color, 1, 1) <- trim(toupper(substr(node_color, 1, 1)))
  substr(node_border, 1, 1) <- trim(toupper(substr(node_border, 1, 1)))
  substr(tie_color, 1, 1) <- trim(toupper(substr(tie_color, 1, 1)))
  
  #Checking that ic and bc are either 1 or the same length as the vertices
  if (length(node_color) != length(data_id) & length(node_color) > 1){
    node_color <- ' '}
  if (length(node_color) != length(data_id) & sum(nchar(node_color) - nchar(gsub('\\,', '', node_color))) > 0){
    node_color <- ' '}
  if (length(node_color) != length(data_id) & sum(grepl("[[:alnum:]]", node_color) == TRUE) == 0){
    node_color <- ' '
  } else {
    node_color <- paste('ic', node_color) 
  }
  
  if (length(node_border) != length(data_id) & length(node_border) > 1){
    node_border <- ' '}
  if (length(node_border) != length(data_id) & sum(gregexpr(",", node_border, fixed=TRUE)[[1]] > 0) > 0){
    node_border <- ' '}
  if (length(node_border) != length(data_id) & sum(grepl("[[:alnum:]]", node_border) == TRUE) == 0){
    border_color <- ' '
  } else {
    node_border <- paste('bc', node_border) 
  }
  
  #Joing colors to nodelist
  nodelist$ic <- node_color
  nodelist$bc <- node_border
  
  rm(node_color, node_border)
  
  #Checking that strength is a non-blank value, if blank supply a tie strength of 1
  if (length(weight) == 1 & sum(grepl("[[:alnum:]]", weight) == TRUE) != length(weight)) {
    weight <- 1
  } else {
    weight <- weight
  }
  
  #Preparing edgelist
  sender <- as.numeric(`person i`)
  target <- as.numeric(`person j`)
  strength <- as.numeric(weight)

  #Creating ties list
  ties <- as.data.frame(cbind(sender, target, strength), stringsAsFactors = FALSE)
  
  #Sorting by sender and target is specified by user
  if (`sort and simplify` == TRUE) {
    ties <- ties[order(ties[,1], ties[,2] ), ]
  } else {
    ties <- ties
  }
  
  #Checking for duplicated ties and deleting them if there are any
  if (`sort and simplify` == TRUE) {
    ties <- ties [!duplicated(ties[c(1,2)]),]
  } else {
    ties <- ties
  }
  
  #Transforming to Characeter
  sender <- as.character(`person i`)
  target <- as.character(`person j`)
  strength <- as.character(weight)
  
  #Performing length checks and assigning tie color
  if (length(tie_color) != length(`person i`) & length(tie_color) > 1){
    tie_color <- ' '}
  if (length(tie_color) != length(`person i`) & sum(gregexpr(",", tie_color, fixed=TRUE)[[1]] > 0) > 0){
    tie_color <- ' '}
  if (length(tie_color) != length(`person i`) & sum(grepl("[[:alnum:]]", tie_color) == TRUE) == 0){
    tie_color <- ' '
  } else {
    tie_color <- paste('c', tie_color) 
  }
  
  #Joining color to ties
  ties$c <- tie_color
  
  rm(tie_color)

  #Moving into Pajek format
  pajek_list <- vector("list", 4)
  pajek_list[[1]] <- paste(vertices, length(id))
  pajek_list[[2]] <- paste(id, nodelist$data_id, x, y, z, nodelist$ic, nodelist$bc, sep= ' ')
  pajek_list[[2]] <- trim(pajek_list[[2]])
  pajek_list[[3]] <- tie_type
  pajek_list[[4]] <- paste(ties[[1]], ties[[2]], ties[[3]], ties[[4]], sep = ' ')

  #Outputting as a large character
  vertices <- pajek_list[[1]]
  nodelist <- pajek_list[[2]]
  tie_type <- pajek_list[[3]]
  edges <- pajek_list[[4]]

  nodelist <- append(vertices, nodelist)
  edges <- append(tie_type, edges)
  pajek_list <- append(nodelist, edges)

  rm(vertices, tie_type, nodelist, id, x, y, z, sender, target, strength, ties)

  #Writing Files
  fileConn <- file(paste0(net_name, ".net"))
  writeLines(pajek_list, fileConn)
  close(fileConn)
}

#write_net Parameters
  #Tie Type: Arces or Edges
  #The Id from your data you are using. The function will write a sequential Pajek ID based on row number.
  #x_coord: Provide the vector if you have coordinates or simply write an empty a blank, ' '. 
  #y_coord: Same as x
  #z_coord: Same as x and y
  #node_color: Specifies the vertex's fill color
  #node_border: Specifies the vertex's border color
  #`person i`: Specify who person i is or in a directed network who the sender is.
  #`person j`: Specify who person j is or in a directed network the target is.
  #weight:  Specify the vector with the tie weights, if the ties have no weights input 1 (Pajek's default)
  #tie_color: Specifies the color of the ties.
  #net_name: Sepcify the file name of the output file. The file will be written to your work directory. The function will specify that it it is a .net file.

#write_net('Edges', nodes$Label, nodes$`x-coord`, nodes$`y-coord`, nodes$`z-coord`, 'blue', 'white', ties$`Person i`, ties$`Person j`, ties$Weight, 'gray', 'test_net')

################
#   read_clu   #
################

print("Partition Files in Your Directory")
Partition_Files

read_clu <- function() {
  
  #Pulling all partitions from the directory
  Partition_List <- vector("list", length(Partition_Files))
  for (i in seq_along(Partition_List)){
    Partition_List[[i]] <- readLines(Partition_Files[[i]])
  }
  
  #Naming Vector List with file names
  for (i in seq_along(Partition_List)){
    Names <- gsub('.clu', '', Partition_Files)
    names(Partition_List)[[i]] <- paste0(Names[[i]])
  }

  vertices <- vector("list", length(Partition_List))
  for (i in seq_along(vertices)){
    vertices[[i]] <-  vector("list", 2)
  }

  #Moving vertices into list format to assign them a size designator
  for (i in seq_along(vertices)){
    vertices[[i]][[1]] <- Partition_List[[i]][[1]]
    vertices[[i]][[2]] <- Partition_List[[i]][-c(1)]
  
    vertices[[i]][[1]] <- strsplit(as.character(vertices[[i]][[1]]),' ') 
    vertices[[i]][[1]] <- lapply(vertices[[i]][[1]], function(x) x[x != ""])
    vertices[[i]][[1]] <- as.numeric(vertices[[i]][[1]][[1]][[2]])
  }

  sizes <- vector("list", length(Partition_Files))
  for (i in seq_along(vertices)){
    sizes[[i]] <- vertices[[i]][[1]]
  }

  clu_id <- unique(unlist(sizes))

  sizes <- unlist(sizes)
  lengths <- vector("list", length(clu_id))
  for (i in seq_along(lengths)){
    lengths[[i]] <- sum(sizes == clu_id[[i]], na.rm=TRUE)
  }

  Partitions <- vector("list", length(clu_id))
  for (i in seq_along(Partitions)){
    obj_name <- paste0("Partititions_", i)  # create a unique R object name that you'll use through the iteration
    id <-  seq(1,clu_id[[i]], by = 1)
    Partitions[[i]] <-  assign(x = obj_name, value = id)   # save iteration to the uniquely named R object
    Partitions[[i]] <- as.data.frame(cbind(clu_id[[i]], Partitions[[i]]))
    colnames(Partitions[[i]]) <- c('Netwwork Size', 'ID')
    names(Partitions)[[i]] <- paste0("Partition_", i)
    rm(id, obj_name, i)
  }

  rm(sizes)

  vertices <- lapply(vertices, function(x) x[-c(1)])
  for (i in seq_along(vertices)){
    vertices[[i]] <- vertices[[i]][[1]]
    names(vertices)[[i]] <- paste0(Names[[i]])
  }

  #Reading in Separate Partitions by Size that will be merged into the partition data.frames.
  Partition_List <- vector("list", length(clu_id))
  for (i in seq_along(clu_id)){
    Partition_List[[i]] <- vector("list", lengths[[i]])
    names(Partition_List)[[i]] <- paste0("Network_", i)
  }

  for (i in seq_along(Partition_List)){
    Partition_List[[i]] <- vertices[lengths(vertices) == clu_id[[i]]]
  }

  #Adding Partitions to the Partition Data.Frames
  for (i in seq_along(Partition_List)){
    Partition_List[[i]] <- do.call(cbind.data.frame, Partition_List[[i]])
  }

  for (i in seq_along(Partitions)){
    Partitions[[i]] <- cbind(Partitions[[i]], Partition_List[[i]])
  }

  #Outputting list as separate data.farmes
  list2env(Partitions,envir=.GlobalEnv)

  rm(Partition_List, Partitions, vertices, lengths, clu_id)
}

#read_clu 
  #read_clu reads all partitions in your directory, separates them by size in the event the 
  #the partition files refer to different networks, and outputs them as separate partition data.frames that can be cbinded to the node list.

#read_clu()

################
#   read_vec   #
################

print("Vector Files in Your Directory")
Vector_Files

read_vec <- function() {
  
  #Pulling all vectors from the directory
  Vector_List <- vector("list", length(Vector_Files))
  for (i in seq_along(Vector_List)){
    Vector_List[[i]] <- readLines(Vector_Files[[i]])
  }
  
  #Naming Vector List with file names
  for (i in seq_along(Vector_List)){
    Names <- gsub('.vec', '', Vector_Files)
    names(Vector_List)[[i]] <- paste0(Names[[i]])
  }
  
  vertices <- vector("list", length(Vector_List))
  for (i in seq_along(vertices)){
    vertices[[i]] <-  vector("list", 2)
  }
  
  #Moving vertices into list format to assign them a size designator
  for (i in seq_along(vertices)){
    vertices[[i]][[1]] <- Vector_List[[i]][[1]]
    vertices[[i]][[2]] <- Vector_List[[i]][-c(1)]
    
    vertices[[i]][[1]] <- strsplit(as.character(vertices[[i]][[1]]),' ') 
    vertices[[i]][[1]] <- lapply(vertices[[i]][[1]], function(x) x[x != ""])
    vertices[[i]][[1]] <- as.numeric(vertices[[i]][[1]][[1]][[2]])
  }
  
  sizes <- vector("list", length(Vector_Files))
  for (i in seq_along(vertices)){
    sizes[[i]] <- vertices[[i]][[1]]
  }
  
  clu_id <- unique(unlist(sizes))
  
  sizes <- unlist(sizes)
  lengths <- vector("list", length(clu_id))
  for (i in seq_along(lengths)){
    lengths[[i]] <- sum(sizes == clu_id[[i]], na.rm=TRUE)
  }
  
  Vectors <- vector("list", length(clu_id))
  for (i in seq_along(Vectors)){
    obj_name <- paste0("Vectors_", i)  # create a unique R object name that you'll use through the iteration
    id <-  seq(1,clu_id[[i]], by = 1)
    Vectors[[i]] <-  assign(x = obj_name, value = id)   # save iteration to the uniquely named R object
    Vectors[[i]] <- as.data.frame(cbind(clu_id[[i]], Vectors[[i]]))
    colnames(Vectors[[i]]) <- c('Netwwork Size', 'ID')
    names(Vectors)[[i]] <- paste0("Vector_", i)
    rm(id, obj_name, i)
  }
  
  rm(sizes)
  
  vertices <- lapply(vertices, function(x) x[-c(1)])
  for (i in seq_along(vertices)){
    vertices[[i]] <- vertices[[i]][[1]]
    names(vertices)[[i]] <- paste0(Names[[i]])
  }
  
  #Reading in Separate Vectors by Size that will be merged into the partition data.frames.
  Vector_List <- vector("list", length(clu_id))
  for (i in seq_along(clu_id)){
    Vector_List[[i]] <- vector("list", lengths[[i]])
    names(Vector_List)[[i]] <- paste0("Network_", i)
  }
  
  for (i in seq_along(Vector_List)){
    Vector_List[[i]] <- vertices[lengths(vertices) == clu_id[[i]]]
  }
  
  #Converting Vectors to Doubles 
  for (i in seq_along(Vector_List)){
    Vector_List[[i]] <- lapply(Vector_List[[i]], function(x) as.double(x))
  }
  
  #Adding Vectors to the Vector Data.Frames
  for (i in seq_along(Vector_List)){
    Vector_List[[i]] <- do.call(cbind.data.frame, Vector_List[[i]])
  }
  
  for (i in seq_along(Vectors)){
    Vectors[[i]] <- cbind(Vectors[[i]], Vector_List[[i]])
  }
  
  #Outputting list as separate data.farmes
  list2env(Vectors,envir=.GlobalEnv)
  
  rm(Vector_List, Vectors, vertices, lengths, clu_id)
}

#read_vec 
#read_vec reads all vector files in your directory, separates them by size in the event the 
#the vector files refer to different networks, and outputs them as separate vector data.frames that can be cbinded to the node list.

#read_vec()

#################
#   write_clu   #
#################

write_clu <- function(cat_variables, var_names) {

  #Creating Names for the files that will be exported based on the column names of the variables if the variables 
  #are from a data.frame or based on the var_names field if they a vector of some kind.
  Names <- character(length=length(cat_variables))
  if (class(cat_variables) == "data.frame") {
    Names <- colnames(cat_variables)
  } else {
    Names <- var_names
  }

  #Transforming variables into character varaibl- var_nameses
  if (class(cat_variables) == "data.frame") {
    cat_variables <- lapply(cat_variables, function(x) as.character(x))
  } else {
    cat_variables <- as.character(cat_variables)
  }

  #Creating meta-information
  vertices <- c('*Vertices')

  #Pulling file lengths to specify the files
  if (class(cat_variables) == "list") {
    lengths <- vector("list", length(cat_variables))
  } else {
    lengths <- 1
  }

  for (i in seq_along(cat_variables)){
    if (class(cat_variables) == "list") {
      lengths[[i]] <- length(cat_variables[[i]])
    } else {
      lengths <- length(cat_variables)
    }
  }

  #Creating Meta Data
  meta_list <- vector("list", length(lengths))
  for (i in seq_along(lengths)){
   meta_list[[i]] <- paste(vertices, lengths[[i]]) 
  }

  #Moving into Pajek format
  pajek_list <- vector("list", length(lengths))
  for (i in seq_along(pajek_list)){
    if (class(cat_variables) == "list") {
     pajek_list[[i]] <- append(meta_list[[i]], cat_variables[[i]]) 
    } else {
      pajek_list[[i]] <- unlist(append(meta_list, cat_variables))
    }
  }

  rm(lengths, meta_list, vertices) 

  #Checking names to ensure there are no unamed files
  for (i in seq_along(Names)){
    if (grepl("[[:alnum:]]", Names[[i]]) == FALSE) {
      Names <- paste0('partition_', i)
    } else {
      Names[[i]] == Names[[i]]
    }
  }

  #Writing Files
  fileConn <- vector("list", length(pajek_list))
  for (i in seq_along(fileConn)){
    fileConn[[i]] <- file(paste0(Names[[i]], ".clu"))
    writeLines(pajek_list[[i]], fileConn[[i]])
    close(fileConn[[i]])
  }
}

#write_clu
  #Writes categorical variables such as gender and race out as Pajek partition files.
  #The function supports writing partition files based on a subset of variables from a data.frame.
  #If writing partions from a data.frame, the function will retain the variable names from the dataset.
  #The functon also supports writing clu files from vectors. To write-out multiple vectors at one time,
  #simply move the function into a list.

#write_clu(Partition_1[[3]], 'test_clu')

#################
#   write_vec   #
#################


write_vec <- function(con_variables, var_names) {
  
  #Creating Names for the files that will be exported based on the column names of the variables if the variables 
  #are from a data.frame or based on the var_names field if they a vector of some kind.
  Names <- character(length=length(con_variables))
  if (class(con_variables) == "data.frame") {
    Names <- colnames(con_variables)
  } else {
    Names <- var_names
  }
  
  #Transforming variables into character varaibl- var_nameses
  if (class(con_variables) == "data.frame") {
    con_variables <- lapply(con_variables, function(x) as.character(x))
  } else {
    con_variables <- as.character(con_variables)
  }
  
  #Creating meta-information
  vertices <- c('*Vertices')
  
  #Pulling file lengths to specify the files
  if (class(con_variables) == "list") {
    lengths <- vector("list", length(con_variables))
  } else {
    lengths <- 1
  }
  
  for (i in seq_along(con_variables)){
    if (class(con_variables) == "list") {
      lengths[[i]] <- length(con_variables[[i]])
    } else {
      lengths <- length(con_variables)
    }
  }
  
  #Creating Meta Data
  meta_list <- vector("list", length(lengths))
  for (i in seq_along(lengths)){
    meta_list[[i]] <- paste(vertices, lengths[[i]]) 
  }
  
  #Moving into Pajek format
  pajek_list <- vector("list", length(lengths))
  for (i in seq_along(pajek_list)){
    if (class(con_variables) == "list") {
      pajek_list[[i]] <- append(meta_list[[i]], con_variables[[i]]) 
    } else {
      pajek_list[[i]] <- unlist(append(meta_list, con_variables))
    }
  }
  
  rm(lengths, meta_list, vertices) 
  
  #Checking names to ensure there are no unamed files
  for (i in seq_along(Names)){
    if (grepl("[[:alnum:]]", Names[[i]]) == FALSE) {
      Names <- paste0('vector_', i)
    } else {
      Names[[i]] == Names[[i]]
    }
  }
  
  #Writing Files
  fileConn <- vector("list", length(pajek_list))
  for (i in seq_along(fileConn)){
    fileConn[[i]] <- file(paste0(Names[[i]], ".vec"))
    writeLines(pajek_list[[i]], fileConn[[i]])
    close(fileConn[[i]])
  }
}

#write_vec
#Writes continuous variables such as size or income out as Pajek vector files.
#The function operates in the same way as write_clu.

#write_vec(Vector_2[[3]], ' ')