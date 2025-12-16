# testing popnet_client.R

# Set working directory in R - where the data is etc.
# setwd("C:/Users/pekpi/Nextcloud/Work/Academic/Individual projects/PopnetEngine software reasoning/R testing")

# Load the "library" with functions for communicating
source("R/threadle_client.R")

# path to the exe file
path_to_exe <-"../bin/Debug/net8.0/Threadle.CLIconsole"

# Start a Threadle instance

th_start_threadle(path_to_exe)
# Move into examples folder
th_set_workdir("../Examples")

# Load a network file into Threadle (will also load a nodeset file)
lazeganet <- th_load_network("lazega","lazega.tsv")
lazega_nodeset <- th_load_file("lazega_nodeset", "node_set")
th_inventory()

th_info(lazeganet)
th_info("lazega_nodeset")
# Get info about the lazega_nodeset
th_info(lazega_nodeset)

info <- function(name, format="json") {
  retval <- .send_command(sprintf("info(name=%s, format=%s)",name, format))
  if (format =="json")
    jsonlite::fromJSON(retval)
  else
    retval
}

# Get an inventory of stored objects

mynodes <- th_load_file("mynodes","iv_nodes.tsv", type = "nodeset")

mynet <- th_create_network("mynet", nodeset = mynodes)

th_info(mynodes)
th_info(mynet)

n1 <- "mynet"
n2 <- "Help_with_a_decision"
n3 <- "1"
cmd <- sprintf("addlayer(network = %s, layername = %s, mode = %s)",n1,n2,n3)
.send_command(cmd)
th_info(mynet)

n4 <- "edges.tsv"
n4 <- "edges_big.tsv"
n5 <- "edgelist"
cmd <- sprintf("importlayer(network = %s, layername = %s, file = %s, format = %s)",n1,n2,n4,n5)
.send_command(cmd)

n6 <- "mynet_test.tsv"
n7 <- "mynet_test_nodeset.tsv"
cmd <- sprintf("savefile(structure = %s, file = %s, nodesetfile = %s)", n1, n6, n7)
.send_command(cmd)


iv <- th_load_network("lazega", "iv_big.tsv") |> system.time()
th_inventory()
th_info("lazega")
th_info("lazega_nodeset")

# Get nbr of nodes in the network (can either use the network or nodeset)
nbr_nodes <- th_get_nbr_nodes("lazega")
nbr_nodes

# Get a random starting node
nodeid <- th_get_nodeid_by_index("lazega", sample(0:nbr_nodes-1,1))
nodeid

# Get Office attribute
office_current <- th_get_attr("lazega_nodeset",nodeid,"village")
office_current

# Get a random alter in the friends layer:
random_alter_nodeid <- th_get_random_alter("lazega", 99, layername = "Help_with_a_decision")
random_alter_nodeid

# Get office attribute of this
office_alter <- th_get_attr("lazega_nodeset", random_alter_nodeid, "caste")
office_alter

th_get_random_node <- function(name) {
  cli <- sprintf("getrandomnode(structure=%s)",name)
  as.numeric(.send_command(cli))
}
th_info("lazega")
th_get_random_node("lazega")


th_density <- function(name,layer) {
  cli <- sprintf("density(network = %s, layername = %s)",name,layer)
  as.numeric(.send_command(cli))
}

th_density("lazega", "Help_with_a_decision") |> system.time()
# etc - so this should be looped of course, but mechanism is all there for random walker now

th_remove_node <- function(name,nodeid) {
  cli <- sprintf("removenode(structure = %s, nodeid = %d)",name,nodeid)
  as.numeric(.send_command(cli))
}
info("lazega")
th_remove_node("lazega", 1092172)

th_stop_threadle()

