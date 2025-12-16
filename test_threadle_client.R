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

lazeganet_nodeset <- th_load_file("lazega_nodeset", "lazega_nodes.tsv", type = "nodeset")
lazeganet <- th_load_network("lazega","lazega.tsv")
th_info(lazeganet)
th_info("lazega")
th_info(lazeganet_nodeset)
th_info("lazega_nodeset")
# require name of structure rather than structure$name


# Load a network file into Threadle (will also load a nodeset file)
th_inventory()

# Get nbr of nodes in the network (can either use the network or nodeset)
nbr_nodes <- th_get_nbr_nodes(lazeganet)
nbr_nodes

# Get a random starting node
nodeid <- th_get_nodeid_by_index(lazeganet, sample(0:nbr_nodes-1,1))
nodeid

# Get Office attribute
office_current <- th_get_attr(lazeganet,nodeid,"Office")
office_current

th_info("lazega")
th_info(lazeganet)
th_info(lazeganet_nodeset)
# Get a random alter in the friends layer:
random_alter_nodeid <- th_get_random_alter(lazeganet, nodeid, layername = "friends")
random_alter_nodeid

# Get office attribute of this
office_alter <- th_get_attr(lazeganet, random_alter_nodeid, "Gender")
office_alter

th_get_random_node("lazega")
th_get_random_node("lazega_nodeset")
th_get_random_node(lazeganet)
th_get_random_node(lazeganet_nodeset)

th_density(lazeganet_nodeset, "friends")
th_density(lazeganet, "friends")
th_density("lazega", "friends")

th_view(lazeganet_nodeset)
th_view(lazeganet)
th_view("lazega")

th_info(lazeganet_nodeset)
th_info(lazeganet)
th_info("lazega")

# etc - so this should be looped of course, but mechanism is all there for random walker now
th_info(lazeganet_nodeset)$NbrNodes
th_remove_node("lazega", 15)

.stop_threadle()
