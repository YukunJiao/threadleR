# testing threadle_client.R

# Install and load threadleR
remotes::install_github("YukunJiao/threadleR")
library(threadleR)
devtools::load_all()

# Optional: switch protocol / command logging
# options(threadle.command = "cli") # for debugging only (default: "json")
options(threadle.print_cmd = TRUE) # print commands sent to Threadle

# Start a Threadle instance
# (uses `threadle` on PATH; if not found, provide a full path)
th_start_threadle("~/Documents/Threadle/Threadle.CLIconsole/bin/Debug/net8.0/threadle")

# Sync Threadle working directory with threadleR
th_sync_wd()

# Copy example files to a local folder
ex_dir <- th_stage_examples_to_wd("./inst/examples")
# Use that folder as Threadle's working directory
th_set_workdir(ex_dir)

# Optional: return mode / message printing
options(threadle.return = "response") # for debugging only (default: "payload")
# options(threadle.print_message = FALSE) # not recommended (messages are printed by default)

# ---

lazeganet_nodeset <- th_load_file("lazega_nodeset", "lazega_nodes.tsv", type = "nodeset")
lazeganet <- th_load_file("lazega","lazega.tsv", type = "network")

mynet_nodeset <- th_load_file("mynet_nodeset", "mynet_nodesetfile.tsv", type = "nodeset")
mynet <- th_load_file("mynet", "mynet.tsv", type = "network")
th_info(lazeganet)
th_components(lazeganet, "friends", "gender") |> class()
th_i()
th_info(mynet)
th_get_random_node(mynet)
th_get_random_node(mynet)
th_get_random_node(mynet_nodeset)
th_get_node_hyperedges(mynet,  "work", 123) |> class()


th_get_attr_summary

th_get_attr_summary(mynet_nodeset, "gender")

NS <- th_create_nodeset(var = "ABC", name = "nodeSet", createnodes = 3)
NS <- th_create_nodeset(var = "ABC", createnodes = 3)
NS
th_info(NS) # Error: [VariableNotFound] No IStructure named 'ABC' found.
th_info(mynet)
th_density(mynet, "kinship") |> class()
th_density(mynet, "trade")
th_density(mynet, "work")

mynet_nodeset <- th_load_file("mynet_nodeset", "mynet_nodesetfile.tsv", type = "nodeset")
th_create_network("mynet", nodeset = "mynet_nodeset", name = "mynet")
th_add_layer("mynet", "kinship", mode = "1")
th_delete_all()
th_generate("mynet", "kinship", type = "er", p = 0.2) |> class()
th_info("mynet")
th_info("mynet_nodeset")
a <-
th_generate_attr(mynet, "height")
th_generate_attr
aa <-
th_get_all_nodes(mynet)
th_info(mynet_nodeset)

th_get_attr(mynet, 123, attrname = "gender")
aa |> class()
th_get_attr_summary("mynet_nodeset", "weight")

# library(usethis)
# use_data(mynet, mynet_nodeset, overwrite = TRUE)
# use_data(lazeganet, lazeganet_nodeset, overwrite = TRUE)
# load("data/mynet_nodeset.rda")
# load("data/mynet.rda")
# load("data/lazeganet.rda")
# load("data/lazeganet_nodeset.rda")

th_info(lazeganet_nodeset)
th_info(lazeganet)

th_shortest_path(lazeganet, 1, 23, "friends")
th_shortest_path(lazeganet, 1, 23, "advice")
th_shortest_path(lazeganet, 1, 23, "collaboration")
th_shortest_path(lazeganet, 1, 23)

mynet_nodeset <- th_load_file("mynet_nodeset", "mynet_nodesetfile.tsv", type = "nodeset")
mynet <- th_load_file("mynet", "mynet.tsv", type = "network")
th_info(mynet)
th_i()

th_create_nodeset("test")
th_info("test")
th_delete("test") |> class()
th_delete_all() |> class()
# Get node's alters on one layer or multiple layers
a <- th_get_node_alters(lazeganet, 23, "friends", direction = "out")
th_get_node_alters(lazeganet, 23, c("advice", "friends"), direction = "out")
th_get_node_alters(lazeganet, 23, c("advice"), direction = "out")
th_get_node_alters(lazeganet, 23, direction = "out")

th_get_node_alters(mynet, 123, direction = "in")

th_get_node_hyperedges()

th_get_node_alters(mynet, 234)

# Get nbr of nodes in the network (can either use the network or nodeset)
nbr_nodes <- th_get_nbr_nodes(lazeganet)
nbr_nodes

# Get a random starting node
nodeid <- th_get_nodeid_by_index(lazeganet, sample(0:nbr_nodes-1,1))
nodeid

# Get Office attribute
office_current <- th_get_attr(lazeganet,nodeid,"Office")
office_current
th_create_network("test","lazega_nodeset","sss") |> class()
th_create_network("lazega_nodeset", "sss")

th_create_nodeset("testnodeset") |> class()
th_info("testnodeset")
th_define_attr('testnodeset', "testattr") |> class()

# Get a random alter in the friends layer:
random_alter_nodeid <- th_get_random_alter("lazega", nodeid, layername = "friends", balanced = "fff")
random_alter_nodeid
th_get_workdir()
# Get office attribute of this
office_alter <- th_get_attr(lazeganet, random_alter_nodeid, "Gender")
office_alter

th_get_random_node("lazega")
th_get_random_node("lazega_nodeset")
th_get_random_node(lazeganet)
th_get_random_node(lazeganet_nodeset)

th_density(lazeganet, "friends") |> class()
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
th_info(mynet)
th_info(mynet_nodeset)
th_get_all_nodes(mynet_nodeset)
th_get_edge(mynet, "trade", 123, 345)
th_get_edge(mynet, "trade", 345, 123)
th_info(mynet)
th_get_hyperedge_nodes(mynet, "work", "ias") |> class()
th_get_all_hyperedges(mynet, "work")

th_get_edge()
lazeganet_nodeset <- th_load_file("lazega_nodeset", "lazega_nodes.tsv", type = "nodeset")
lazeganet <- th_load_file("lazega","lazega.tsv", type = "network")
th_info(lazeganet)
th_info(lazeganet_nodeset)

th_get_nbr_nodes(mynet) |> class()



th_add_node(lazeganet, 100) |> class()

th_add_layer(lazeganet, "test122", mode = 1, valuetype = "binary", selfties = "test")
th_add_edge(lazeganet, "test122", 31, 35, addmissingnodes = "ttt")


th_add_layer(lazeganet, "test3", mode = 2)
th_info(lazeganet)
th_add_aff(lazeganet, "test3", 30, "newaff")
class(th_add_aff(lazeganet, "test3", 30, "newaff"))
class(th_add_aff(lazeganet, "test3", 30, "newaff"))
th_remove_edge(lazeganet, "test122", 31, 35)
th_add_edge(lazeganet, "friends", 1, 100)
th_remove_edge(lazeganet, "friends", 1, 100)
aa <- th_add_edge(lazeganet, "friends", 1, 100)
aa
class(th_add_edge(lazeganet, "friends", 1, 100))
th_add_edge("lazega", "test122", 1, 70)
th_add_edge("lazega", "test122", 1, 3, 1, "true")

th_info(mynet_nodeset)
sub_ns <- th_filter(name="sub_nodeset", nodeset=mynet_nodeset, attrname="gender", cond="eq", attrvalue="f")
th_info(sub_ns)

th_subnet("mynet_sub", mynet, "sub_nodeset")


nbr_nodes <- th_get_nbr_nodes(mynet)
nodeid <- th_get_nodeid_by_index(mynet, sample(0:(nbr_nodes-1),1))
# nodeid <- NULL
# nodeid <- numeric(0)
th_info(mynet)
random_alter_nodeid <- th_get_random_alter(mynet, nodeid, layername = "trade")
random_alter_nodeid |> class()
th_get_random_edge(mynet, "work") |> class()
th_get_random_alter(mynet, "null", layername = "trade")

th_get_random_node(mynet)


th_get_random_alter(mynet, nodeid = 456, layername = "")
th_info(mynet)

th_get_edge(mynet, "trade", 456, 567)

th_get_edge(mynet, "trade", 123, 456)

th_get_edge(mynet, "trade", 890, 234)
th_get_edge(mynet, "trade", 890, 789)

th_dichotomize(mynet, "trade", cond = "ge", threshold = 1000, truevalue = 1, newlayername = "trade_test") |> class()
th_get_edge(mynet, "trade_test", 890, 234)
th_get_edge(mynet, "trade_test", 890, 789)
th_get_edge(mynet, "trade_test", 890, 123)
th_info(mynet_nodeset)
th_info(mynet)

th_filter("test123", nodeset = mynet_nodeset, "gender", cond = "eq", attrvalue = "o") |> class()
th_info("test123")
th_i()
th_info(mynet)
getwd()

th_delete_all()
th_generate(network = "mynet", layername = "kinship", p = 2, type = 'er')
th_i()
th_info(mynet)
mytestnet_nodeset <- th_load_file("mytestnet_nodeset", "mynet_nodesetfile.tsv", type = "nodeset")
mytestnet <- th_load_file("mytestnet", "mynet.tsv", "network")

th_save_file("mytestnet_nodeset", "")
th_save_file("mytestnet", "/Users/doge/Documents/mytestnet.tsv")

th_i()

th_save_file("test123")
th_save_file("test123", "")
th_save_file("test123", "nodes.bin.gz")
# th_save_file("mytestnet", file = "nodes.bin.gz")

th_info(mynet)
th_info(mynet_nodeset)
mynet_nodeset <- th_load_file("mynet_nodeset", "mynet_nodesetfile.tsv", type = "nodeset")
mynet <- th_load_file("mynet", "mynet.tsv", "network")
th_info(mynet)
th_remove_attr("mynet", 123, "gender")

th_check_edge(mynet, "trade", 123, 345)
th_check_edge(mynet, "kinship", 345, 456)

tt <- th_degree(mynet, "kinship", attrname = "kinship_degree")
tt
th_degree(mynet, "kinship", attrname = NULL)
th_degree(mynet, "trade", attrname = "")
th_degree(mynet, "work", attrname = "weight") |> class()

th_info(mynet)
th_info(mynet_nodeset)
th_i()
th_info(mynet)
th_remove_aff(mynet, "work", 789, "ica")
lazega <- th_load_file("lazega", "lazega.tsv", type = "network")
th_info(lazega)
th_info(lazeganet_nodeset)
th_degree(lazega, "advice", attrname = NULL)
th_info(lazeganet_nodeset)
th_degree(lazega, "friends", attrname = "")
th_get_attr(lazega, 2, "ttt")

th_save_file(mynet, "testtt.tsv")

# options(threadle.return = "response")
mynet
th_add_hyper(mynet, "work", "LiU", nodes = "")
th_add_hyper(mynet, "work", "LiU", nodes = c())
th_add_hyper(mynet, "work", "LiU", nodes = 123) |> class()
th_add_hyper(mynet, "work", "LiU", nodes = c(123,456))
th_save_file(mynet, "testtt.tsv")

th_remove_hyper(mynet, "work", "LiU")
th_info(mynet)
th_remove_layer(mynet, "trade")
th_info(lazeganet)

lazeganet_nodeset <- th_load_file("lazega_nodeset", "lazega_nodes.tsv", type = "nodeset")
lazeganet <- th_load_file("lazega","lazega.tsv", type = "network")
th_info(lazeganet)

th_add_layer(lazeganet, "test1", mode = 1)
th_add_layer(lazeganet, "test2", mode = 2)
th_remove_layer(lazeganet, "advice")
th_remove_layer(lazeganet, "test1")
th_info(mynet)
th_components(mynet, "kinship", "testattr")

th_components

th_add_hyper(lazeganet, "test2", "test_name", nodes = c(1,2,32,4,5,7), addmissingnodes = FALSE)
th_info(lazeganet)
th_clear_layer(lazeganet, "test2") |> class()

th_add_layer(lazeganet, "test123", mode = 1)
th_remove_layer(lazeganet, "test123")
th_setting("verbose", TRUE)
th_setting("verbose", FALSE)
# th_setting("verbose", FALSE)
th_info(mynet)
th_info(mynet_nodeset)
mynet_nodeset <- th_load_file("mynet_nodeset", "mynet_nodesetfile.tsv", type = "nodeset")
mynet <- th_load_file("mynet", "mynet.tsv", type = "network")
th_info(mynet_nodeset)
th_undefine_attr(mynet_nodeset, "gender")
#---

th_get_workdir()
th_stop_threadle()

mynet_nodeset <- th_load_file("mynet_nodeset", "mynet_nodesetfile.tsv", type = "nodeset")
mynet <- th_load_file("mynet", "mynet.tsv", type = "network")
th_filter("test_nodeset", nodeset = "mynet_nodeset", "gender", cond = "eq", attrvalue = "o")


th_filter("test_nodeset", nodeset = "mynet_nodeset", "gender", cond = "eq", attrvalue = "o")
th_i()
th_info("testmeow_nodeset")

setwd("/Users/doge/Documents/Threadle/Threadle.CLIconsole/Examples")
mynet_nodeset = loadfile("mynet_nodesetfile.tsv", type = "nodeset")
mynet = loadfile("mynet.tsv", "network")


lazega = loadfile("lazega.tsv", "network")



test = filter(nodeset=mynet_nodeset, attrname=gender, cond=eq, attrvalue=o)

th_save_file(mytestnet)
th_save_file(mytestnet, file = "test.tsv.gz")
th_save_file(mytestnet_nodeset, file = "tdd.bin.gz")

shQuote("123", "cmd2")

# .send_command("sub_nodeset = filter(nodeset=mynet_nodeset, attrname=gender, cond=eq, attrvalue=f)")
