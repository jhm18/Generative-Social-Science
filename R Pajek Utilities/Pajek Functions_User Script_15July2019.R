#Pajek Functions: Demonstration Script
#Jonathan H. Morgan
#15 July 2019

#Specifying the working directory that we will reading and writing our Pajek Files
setwd("/Users/jhmorgan/Desktop/Personal/GPM_Diss/Time1_Language")
getwd()

#Reading in the lists of Pajek Files and the Functions
source("/Applications/Pajek64/R_Pajek Functions_15July2019.R")

################
#   read_net   #
################

#Reading in one network from our list of Pajek network files (Net_Files)
  #Reads in Pajek's Nodeslist and Edgelist as data.frames

read_net(Net_Files[[1]])

#Reading multiple networks
 #First, reading all the Pajek .net files in the directory into a list

#Creating a List to output  the networks' Nodelists and a list to output the networks' Edgelists 
Node_List <- vector("list", length(Net_Files))
Ties_List <- vector("list", length(Net_Files))

for (i in seq_along(Net_Files)) {
  read_net(Net_Files[[i]])
  Ties_List[[i]] <- ties
  Node_List[[i]] <- vertices
  rm(vertices, ties)
  
  names(Ties_List)[[i]] <- paste0("Network_", i)
  names(Node_List)[[i]] <- paste0("Network_", i)
}

#################
#   write_net   #
#################

#write_net Parameters:

#tie_type:    Specify if the network is directed (Arcs) or undirected (Edges)
#data_id:     The unique observation Id from your data  
#             The function will write a sequential Pajek ID based on row number.
#x_coord:     Specifies a vector of x-coordinates (e.g., nodes$x_coord). 
#             If you are not using spatial data, simply write an empty a blank, ' '. 
#y_coord:     Same as x
#z_coord:     Same as x and y
#node_color:  Specify the vertex colors either a single color (e.g., 'blue') or 
#             from a vector where colors are associated with attributes.
#             You can also leave this value blank, ' '. 
#             The other color commands work in the same way.
#node_border: Specify the vertex border colors. 
#`person i`:  Specify who person i is or in a directed network who the sender is.
#`person j`:  Specify who person j is or in a directed network the target is.
#weight:      Specify the vector with the tie weights, if the ties have no weights input 1 
#             (Pajek's default). The function will specify 1 if left blank, but not great practice.
#tie_color:   Specify the tie colors
#net_name:    Sepcify the file name of the output file. 
#             The file will be written to your work directory. 
#             The function will specify that that the file is a .net file.
#`sort        Specifies if the ties are to be sorted and simplified (duplicate ties deleted) by write_net
# and        `sort and simplify`is a logical variable, with TRUE indicating that write_net should
# simplify`   sort and simplify the tie list.
#             Having the ties sorted can be useful for subsequent analyses, but
#             for large networks it adds processing time and is not strictly
#             necessary for Pajek to read the file. Also, Pajek supports simplification within the program. 
#             For very large networks, sorting and simplifying elsewhere is recommended.

#Example of writing-out one network

#Reading in a single network file to then write out as demonstration
read_net(Net_Files[[5]])

#Specifying a different output directory
setwd("/Users/jhmorgan/Desktop")

#Example of wrting-out one network from our data
write_net('edges', vertices$Label , 
          vertices$`x-coord`, vertices$`y-coord`, vertices$`z-coord`, 
          'blue' , 'white',
          ties$`Person i`, ties$`Person j`, ties$Weight, ' ', 
          'test_net', TRUE)

#Example of writing-out multiple networks

#Creating a vector of names that we will assign our networks 
  #Note:  Pajek doesn't deal well with blanks in file names
  
net_names <- c('net_1', 'net_2', 'net_3', 'net_4', 'net_5')

#All of the networks in the example code are undirected language similarity networks. 
#But, if you were writing out both directed and undirected networks, 
#you would need to create a vector that specifies each network's tie type.

#Example vector specifying whether the network is directed or undirected
#tie_types <- c('Edges', 'Arcs', 'Edges', 'Edges', Edges)

#Writing-out all the files I read in but with new names
for (i in seq_along(Net_Files)) {
  write_net('Edges', Node_List[[i]]$Label , 
            Node_List[[i]]$`x-coord`, Node_List[[i]]$`y-coord`, Node_List[[i]]$`z-coord`, 
            'blue', 'white',
            Ties_List[[i]]$`Person i`, Ties_List[[i]]$`Person j`, Ties_List[[i]]$Weight, 'Gray',
            net_names[[i]], TRUE)
}

################
#   read_clu   #
################

#read_clu reads all the partition files in your working directory.
#Outputs them as data.frames grouped by size, 
#and retains the file labels as column names for your reference
#This functionality facilitates quickly merging attributes to the applicable nodelists.

#Resetting the work diretory to read in partition files
setwd("C:/jhm18/GPM_Diss/Time1_Language")

read_clu()

#################
#   write_clu   #
#################

#write_clu writes categorical variables such as gender and race out as Pajek partition files.
#The function supports writing partition files from a subset of variables from a data.frame.
#If writing partitions from a data.frame, 
#the function will retain the variable names from the dataset.
#The function also supports writing .clu files from R vectors. 
#To write-out multiple vectors at one time, you can move the function into a list.

#Specifying a different directly to write files
setwd("C:/jhm18")

#Writing single partition where the user has specified a name
#Note: Pajek doesn't handle white space well in file names.
write_clu(Partition_1$Time1Language_WC, 'Langauge_1_WC')

#Writing multiple partition files from a data.frame. 
#Names will be the data.frame's column names
write_clu(Partition_1[3:4], ' ')

#Writing multiple single vectors where the partition names are supplied by a names list.
#If you are writing one vector and forget the name, 
#the function will supply a default.
#If you are writing multiple partitions, 
#you need to supply a names list or the function will write-over the files.

par_names <- c("test_1", "test_2", "test_3", 'test_4', 'test_5') #Suppling the function a list of names.

for (i in seq_along(Partition_1)) {
  write_clu(Partition_1[[i]], par_names[[i]])
}

################
#   read_vec   #
################

#read_vec's functionality is identical to read_clu. 
#Like read_clu, the function reads in .vec files, sorts them by size, 
#and outputs them as data.frame based on size. 
#The names of the vector files are retained as the column names of the outputted data.frame.

#Resetting the work diretory to read in partition files
setwd("C:/jhm18/GPM_Diss/Time1_Language")

read_vec()

#################
#   write_vec   #
#################

#write_vect operates in an identical fashion as write_clu, but for Pajek's .vec  files.

#Specifying a different directly to write files
setwd("C:/jhm18")
getwd()

#Writing single vector where the user has specified a name
#Note: Pajek doesn't handle white space well in file names or directory paths.
write_vec(Vector_1$Language1_x_coordinates, 'Langauge_1_x')

#Writing multiple partition files from a data.frame. Names will be the data.frame's column names
write_vec(Vector_2[3:4], ' ')

#Writing multiple single vectors where the names are supplied by a names list.
#If you are writing one vector and forget the name, the function will supply a default.
#If you are writing multiple vectors, 
#you need to supply a names list or the function will write-over  the files.

vec_names <- c("test_1", "test_2", "test_3", 'test_4', 'test_5', 'test6') #Suppling the function a list of names.

for (i in seq_along(Vector_2)) {
  write_vec(Vector_2[[i]], vec_names[[i]])
}