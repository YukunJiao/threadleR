# testing popnet_client.R

# Set working directory in R - where the data is etc.
# setwd("C:/Users/pekpi/Nextcloud/Work/Academic/Individual projects/PopnetEngine software reasoning/R testing")

# Load the "library" with functions for communicating
devtools::load_all()

# path to the exe file
path_to_exe <-"../bin/Debug/net8.0/Threadle.CLIconsole"

# Start a Threadle instance

th_start_threadle(path_to_exe)

# Move into examples folder
th_set_workdir("../Examples")

options(threadle.return = "response")
options(threadle.return = "payload")
options(threadle.print_message = FALSE)

lazeganet_nodeset <- th_load_file("lazega_nodeset", "lazega_nodes.tsv", type = "nodeset")
lazeganet <- th_load_network("lazega","lazega.tsv", type = "network")
lazeganet <- th_load_file("lazega","lazega.tsv", type = "network")
mynet_nodeset <- th_load_file("mynet_nodeset", "mynet_nodesetfile.tsv", type = "nodeset")
mynet <- th_load_network("mynet", "mynet.tsv")
mynet <- th_load_file("mynet", "mynet.tsv", type = "network")
mynet$name
mynet_nodeset$name

th_info("lazega")
th_info(lazeganet)
th_info("lazega_nodeset")
th_info(lazeganet_nodeset)
th_info(mynet)
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
random_alter_nodeid <- th_get_random_alter("lazega", nodeid, layername = "friends")
random_alter_nodeid

# Get office attribute of this
office_alter <- th_get_attr(lazeganet, random_alter_nodeid, "Gender")
office_alter

th_get_random_node("lazega")
th_get_random_node("lazega_nodeset")
th_get_random_node(lazeganet)
th_get_random_node(lazeganet_nodeset)

th_density(lazeganet, "friends")
th_density("lazega", "friends")

th_info(lazeganet_nodeset)
th_info(lazeganet)
th_info("lazega")

# etc - so this should be looped of course, but mechanism is all there for random walker now
th_info(lazeganet_nodeset)$NbrNodes
th_info("lazega")
th_info("lazega_nodeset")
th_remove_node("lazega_nodeset", 11)
th_remove_node("lazega", 4)
th_remove_node(lazeganet_nodeset, 20)

lazeganet_nodeset <- th_load_file("lazega_nodeset", "lazega_nodes.tsv", type = "nodeset")
lazeganet <- th_load_file("lazega","lazega.tsv", type = "network")
th_info(lazeganet)
th_info(lazeganet_nodeset)


th_add_node(lazeganet, 100)


th_add_layer(lazeganet, "test1", mode = 1)
th_add_edge(lazeganet, "test1", 31, 36)


th_add_layer(lazeganet, "test2", mode = 2)
th_info(lazeganet)
th_add_aff(lazeganet, "test2", 30, "newaff")
th_remove_edge(lazeganet, "test1", 31, 36)
th_add_edge(lazeganet, "friends", 1, 100)

th_add_edge("lazega", "test1", 1, 70)
th_add_edge("lazega", "test1", 1, 3, 1, "true")


th_info(lazeganet)

th_inventory()

mytestnet_nodeset <- th_load_file("mytest_nodeset", "mytestnet_nodeset.tsv", type = "nodeset")
mytestnet <- th_load_network("mytest", "mytestnet.tsv")
th_info(mynet_nodeset)
th_info(mynet)

# ERROR
# sub_ns <- th_filter(name="sub_nodeset", nodeset=mynet_nodeset, attrname="gender", cond="eq", attrvalue="f")
# th_info(sub_ns)
# .send_command("sub_nodeset = filter(nodeset=mynet_nodeset, attrname=gender, cond=eq, attrvalue=f)")
# ERROR
# th_subnet("mynet_sub", mynet, "sub_nodeset")

nbr_nodes <- th_get_nbr_nodes(mytestnet)
nodeid <- th_get_nodeid_by_index(mytestnet, sample(0:(nbr_nodes-1),1))
# nodeid <- 0
# nodeid <- numeric(0)
th_info(mytestnet)
random_alter_nodeid <- th_get_random_alter(mytestnet, nodeid, layername = "trade")
random_alter_nodeid

th_get_random_node(mytestnet)


name <- "mytestnet"
nodeid <- ""
layername <- "trade"
direction <- "both"
balanced <- FALSE
th_get_random_alter(mytestnet, nodeid = 456, layername = "")
th_info(mytestnet)

th_get_edge(mytestnet, "trade", 456, 567)

th_get_edge(mytestnet, "trade", 123, 456)

th_get_edge(mytestnet, "trade", 890, 234)
th_get_edge(mytestnet, "trade", 890, 789)

th_dichotomize(mytestnet, "trade", cond = "ge", threshold = 1000, truevalue = 1, newlayername = "trade_test")

th_get_edge(mytestnet, "trade_test", 890, 234)
th_get_edge(mytestnet, "trade_test", 890, 789)
th_info(mytestnet_nodeset)
th_info(mytestnet)

mytestnet_nodeset <- th_load_file("mytest_nodeset", "mytestnet_nodeset.tsv", type = "nodeset")
mytestnet <- th_load_network("mytest", "mytestnet.tsv")
th_filter("testmeow", nodeset = mytestnet_nodeset, "gender", cond = "eq", attrvalue = "o")
th_info("testmeow")
.send_command("testmeow = filter(nodeset=mynet_nodeset, attrname=gender, cond=eq, attrvalue=o)")
th_inventory()
getwd()

mytestnet_nodeset <- th_load_file("mytestnet_nodeset", "mynet_nodesetfile.tsv", type = "nodeset")
mytestnet <- th_load_network("mytestnet", "mynet.tsv")
mytestnet_nodeset$name
mytestnet$name
th_generate("testlemiao", 5, 2, type = "network")
th_inventory()
th_info(mytestnet)
th_save_file("mytestnet_nodeset", "")
th_save_file("mytestnet")


th_save_file("testlemiao_nodeset")
th_save_file("testlemiao", "")
th_save_file("testlemiao", "nodes.bin.gz")
# th_save_file("mytestnet", file = "nodes.bin.gz")

th_info(mynet)
th_info(mynet_nodeset)
mynet_nodeset <- th_load_file("mynet_nodeset", "mynet_nodesetfile.tsv", type = "nodeset")
mynet <- th_load_file("mynet", "mynet.tsv", "network")

mynet_nodeset <- th_load_file("mynet_nodeset", "mynet_nodesetfile.tsv", type = "nodeset")
mynet <- th_load_network("mynet", "mynet.tsv")
th_info(mynet)
th_remove_attr("mynet", 123, "gender")

th_check_edge(mynet, "trade", 123, 345)
th_check_edge(mynet, "kinship", 345, 456)

tt <- th_degree(mynet, "kinship", attrname = "kinship_degree")
th_degree(mynet, "kinship", attrname = NULL)
th_degree(mynet, "work", attrname = "")

th_info(mynet)
th_info(mynet_nodeset)
th_inventory()
th_info(mynet)
th_remove_aff(mynet, "work", 789, "ica")

th_save_file(mynet, "testtt.tsv")

options(threadle.return = "response")
th_add_hyper(mynet, "work", "LiU", nodes = "")
th_add_hyper(mynet, "work", "LiU", nodes = c())
th_add_hyper(mynet, "work", "LiU", nodes = 123)
th_add_hyper(mynet, "work", "LiU", nodes = c(123,456))
th_save_file(mynet, "testtt.tsv")

th_remove_hyper(mynet, "work", "LiU")
th_info(mynet)
th_remove_layer(mynet, "work")
th_info(lazeganet)
th_remove_layer(lazeganet, "test1")
th_add_layer(lazeganet, "test", mode = 2)
th_add_hyper(lazeganet, "test", "test_name", nodes = c(1,2,32,4,5,7), addmissingnodes = FALSE)
th_info(lazeganet)
th_clear_layer(lazeganet, "test2")

th_add_layer(lazeganet, "test2", mode = 2)
th_remove_layer(lazeganet, "test2")
th_setting("verbose", TRUE)
th_setting("verbose", FALSE)
# th_setting("verbose", FALSE)
th_info(mynet, "json")
th_info(mynet_nodeset)
mynet_nodeset <- th_load_file("mynet_nodeset", "mynet_nodesetfile.tsv", type = "nodeset")
mynet <- th_load_network("mynet", "mynet.tsv")
th_info(mynet_nodeset)
th_undefine_attr(mynet_nodeset, "gender")
#---

th_stop_threadle()

{"Assign":null,"Command":"setwd","Args":{"dir":"/Users/doge/Documents/Threadle/Threadle.CLIconsole/Examples"}}
{"Assign":null,"Command":"i","Args":null}
{"Assign":"mytest_nodeset","Command":"loadfile","Args":{"file":"mytestnet_nodeset.tsv","type":"nodeset"}}
{"Assign":"mytest","Command":"loadfile","Args":{"name":"mytest","file":"mytestnet.tsv","type":"network"}}
{"Assign":null,"Command":"i","Args":null}


setwd("/Users/doge/Documents/Threadle/Threadle.CLIconsole/Examples")
mynet_nodeset = loadfile("mynet_nodeset", "mynet_nodesetfile.tsv", type = "nodeset")
mynet = loadfile("mynet.tsv", "network")

lazega = loadfile("lazega.tsv", "network")

mytestnet_nodeset <- th_load_file("mynet_nodeset", "mynet_nodesetfile.tsv", type = "nodeset")
mytestnet <- th_load_network("mynet", "mynet.tsv")

test = filter(nodeset=mynet_nodeset, attrname=gender, cond=eq, attrvalue=o)
th_save_file(mytestnet)
th_save_file(mytestnet, file = "test.tsv.gz")
th_save_file(mytestnet_nodeset, file = "tdd.bin.gz")

shQuote("123", "cmd2")

# .send_command("sub_nodeset = filter(nodeset=mynet_nodeset, attrname=gender, cond=eq, attrvalue=f)")
