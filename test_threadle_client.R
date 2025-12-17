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
th_remove_node("lazega", 11)
th_remove_node("lazega", 4)
th_remove_node(lazeganet_nodeset, 20)

lazeganet_nodeset <- th_load_file("lazega_nodeset", "lazega_nodes.tsv", type = "nodeset")
lazeganet <- th_load_network("lazega","lazega.tsv")
th_info(lazeganet)
th_info(lazeganet_nodeset)
th_view(lazeganet_nodeset)

th_view(lazeganet)

th_add_node(lazeganet, 100)


th_add_layer(lazeganet, "test1", mode = 1)
th_add_edge(lazeganet, "test1", 31, 36)


th_add_layer(lazeganet, "test2", mode = 2)
th_add_aff(lazeganet, "test2", 20, "newaff")
th_remove_edge(lazeganet, "test", 31, 36)
th_add_edge(lazeganet, "friends", 1, 100)

th_add_edge("lazega", "test", 1, 70)
th_add_edge("lazega", "test", 1, 3, 1, "true")
th_view("lazega")






th_info(lazeganet)



mynet_nodeset <- th_load_file("mynet_nodeset", "mynet_nodesetfile.tsv", type = "nodeset")
mynet <- th_load_network("mynet", "mynet.tsv")
th_info(mynet)
th_view(mynet_nodeset)
th_view(mynet)

sub_ns <- th_filter(name="sub_nodeset", nodeset=mynet_nodeset, attrname="gender", cond="eq", attrvalue="f")
th_view(sub_ns)

th_setting
th_subnet("mynet_sub", mynet, sub_ns)
th_view("mynet_sub")
nbr_nodes <- th_get_nbr_nodes(mynet)
nodeid <- th_get_nodeid_by_index(mynet, sample(0:nbr_nodes-1,1))
random_alter_nodeid <- th_get_random_alter(mynet, nodeid, layername = "trade")
random_alter_nodeid
th_get_edge(mynet, "trade", 456, 567)

th_get_edge(mynet, "trade", 123, 456)

th_get_edge(mynet, "trade", 890, 234)
th_get_edge(mynet, "trade", 890, 789)

th_dichotomize(mynet, "trade", cond = "ge", threshold = 1000, truevalue = 1, newlayername = "trade_test")
th_view(mynet)
th_get_edge(mynet, "trade_test-1", 890, 234)
th_get_edge(mynet, "trade_test-1", 890, 789)
th_view(mynet_nodeset)
th_filter("tttestmeow", nodeset = mynet_nodeset, "gender", cond = "eq", attrvalue = "o")
th_info("tttestmeow")
th_view("tttestmeow")
th_inventory()

th_generate("testlemiao", 5, 2)
th_view("testlemiao")
th_inventory()
th_save_file("testlemiao", "testlemiao.tsv")
th_save_file("testlemiao_nodeset", "testlemiao.tsv")

th_save_file("testlemiao", "")
th_save_file("testlemiao_nodeset")
th_save_file

th_view(mynet)
th_view(mynet_nodeset)

th_remove_attr(mynet, 123, "gender")

th_remove_aff(mynet, )

th_check_edge(mynet, "work", 123, 345)
th_check_edge(mynet, "kinship", 345, 456)
tt <- th_degree(mynet, "kinship", attrname = "kinshihp_degree")


th_inventory()
th_info(mynet)
th_remove_aff(mynet, "work", 789, "ica")

th_save_file(mynet, "testtt.tsv")
th_remove("testlemiao")
th_dichotomize
th_view(mynet)
th_remove_hyper(mynet, "trade_test", "")
th_add_hyper(mynet, "work", "LiU", nodes = c(789, 234,345, 123))
th_save_file(mynet, "testtt.tsv")

th_remove_hyper(mynet, "work", "LiU")
th_info(mynet)

th_remove_layer(mynet, "kinship")

th_add_hyper(lazeganet, "test", "new1122e111hyper", nodes = c(1,2,32,4,5,7), addmissingnodes = FALSE)
th_clear_layer(lazeganet, "test1")
th_view(lazeganet)

th_setting("verbose", TRUE)
th_setting("verbose", FALSE)
th_view(mynet)

th_subnet("ceshi", mynet, )

th_setting
th_info(mynet_nodeset)
th_undefine_attr(mynet_nodeset, "weight")
#---

setwd("/Users/doge/Documents/Threadle/Threadle.CLIconsole/Examples")
mynet = loadfile("mynet.tsv", "network")
lazega = loadfile("lazega.tsv", "network")

.stop_threadle()