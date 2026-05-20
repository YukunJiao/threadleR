# End-to-end threadleR command workflow using bundled example data.
#
# Usage:
#   Rscript scripts/test_all_commands_example_data.R

library(threadleR)
devtools::load_all()

options(
  threadle.print_cmd = FALSE,
  threadle.print_message = FALSE,
  threadle.return = "payload",
  threadle.timeout = 1800
)

print(th_is_available())
th_start_threadle()
on.exit(th_stop_threadle(), add = TRUE)
getwd()
th_get_workdir()
# Load bundled example data.
example_data <- th_load_examples(c("mynet", "lazega"))
example_data <- th_load_examples(c("mynet"))
print(names(example_data))
print(th_get_workdir())
print(th_dir())

print(th_i())
print(th_info(mynet))
print(th_preview(mynet))
print(th_cmd("info", args = list(structure = mynet)))

# Explore nodes, attributes, one-mode ties, and two-mode affiliations.
n_mynet <- th_get_nbr_nodes(mynet)
nodes_mynet <- th_get_all_nodes(mynet, offset = 0, limit = 8)
first_node <- th_get_nodeid_by_index(mynet, index = 0)
random_node <- th_get_random_node(mynet)
gender_123 <- th_get_attr(mynet_nodeset, nodeid = 123, attrname = "gender")
some_genders <- th_get_attrs(mynet_nodeset, nodes = c(123, 234, 345), attrname = "gender")
gender_summary <- th_get_attr_summary(mynet_nodeset, attrname = "gender")

print(n_mynet)
print(nodes_mynet)
print(first_node)
print(random_node)
print(gender_123)
print(some_genders)
print(gender_summary)

trade_123_345 <- th_get_edge(mynet, "trade", node1id = 123, node2id = 345)
has_kinship <- th_check_edge(mynet, "kinship", node1id = 123, node2id = 345)
trade_edges <- th_get_all_edges(mynet, "trade", offset = 0, limit = 10)
random_trade_edge <- th_get_random_edge(mynet, "trade")
alters_345 <- th_get_node_alters(mynet, nodeid = 345, layernames = "trade", direction = "both")
degree_345 <- th_get_degree(mynet, nodeid = 345, layernames = "trade", direction = "both")
random_alter_345 <- th_get_random_alter(mynet, nodeid = 345, layernames = "trade", direction = "both")
density_kinship <- th_density(mynet, "kinship")

print(trade_123_345)
print(has_kinship)
print(trade_edges)
print(random_trade_edge)
print(alters_345)
print(degree_345)
print(random_alter_345)
print(density_kinship)

work_hyperedges <- th_get_all_hyperedges(mynet, "work", offset = 0, limit = 10)
ias_nodes <- th_get_hyperedge_nodes(mynet, "work", hypername = "ias")
node_123_hypers <- th_get_node_hyperedges(mynet, "work", nodeid = 123)

print(work_hyperedges)
print(ias_nodes)
print(node_123_hypers)

# Derive structures from the example data.
female_nodeset <- th_filter("female_nodeset", mynet_nodeset, "gender", cond = "eq", attrvalue = "f")
female_mynet <- th_subnet("female_mynet", mynet, female_nodeset)
print(th_info(female_nodeset))
print(th_info(female_mynet))

th_components(mynet, "kinship", attrname = "kinship_component")
th_degree(mynet, "kinship", attrname = "kinship_degree", direction = "both")
print(th_get_attr_summary(mynet, "kinship_degree"))

short_123_567 <- th_shortest_path(mynet, node1id = 123, node2id = 567, layernames = "kinship")
gender_shortest_paths <- th_shortest_paths("gender_shortest_paths", mynet, attrname = "gender", layernames = "kinship")
print(short_123_567)
print(th_info(gender_shortest_paths))

th_symmetrize(mynet, "trade", method = "max", newlayername = "trade_sym")
th_dichotomize(mynet, "trade", cond = "ge", threshold = 1000, truevalue = 1, falsevalue = 0, newlayername = "trade_hi")
th_project_two_mode(mynet, "work", method = "count", newlayername = "work_projected")
print(th_get_edge(mynet, "trade_sym", 123, 345))
print(th_get_edge(mynet, "trade_hi", 890, 234))
print(th_get_edge(mynet, "work_projected", 123, 456))

th_pack(mynet, layername = "kinship")
th_unpack(mynet, layername = "kinship")

# Build and edit a small network.
lab_nodes <- th_create_nodeset("lab_nodes", name = "Lab nodes", createnodes = 5)
lab_net <- th_create_network("lab_net", lab_nodes, name = "Lab network")

th_add_node(lab_nodes, nodeid = 10)
print(th_get_all_nodes(lab_nodes, offset = 0, limit = 20))
th_remove_node(lab_nodes, nodeid = 10)

th_add_layer(lab_net, "friendship", mode = 1, directed = FALSE, valuetype = "binary", selfties = FALSE)
th_add_layer(lab_net, "events", mode = 2)
th_add_edge(lab_net, "friendship", node1id = 1, node2id = 2)
th_add_edge(lab_net, "friendship", node1id = 2, node2id = 3)
print(th_check_edge(lab_net, "friendship", 1, 2))
print(th_get_all_edges(lab_net, "friendship", offset = 0, limit = 10))
th_remove_edge(lab_net, "friendship", node1id = 2, node2id = 3)

th_add_edge(lab_net, "friendship", node1id = 3, node2id = 4)
print(th_get_random_edge(lab_net, "friendship"))
th_clear_layer(lab_net, "friendship")
print(th_get_all_edges(lab_net, "friendship", offset = 0, limit = 10))

th_add_hyper(lab_net, "events", hypername = "seminar", nodes = c(1, 2))
th_add_aff(lab_net, "events", nodeid = 3, hypername = "seminar")
print(th_get_hyperedge_nodes(lab_net, "events", "seminar"))
print(th_get_node_hyperedges(lab_net, "events", 3))
th_remove_aff(lab_net, "events", nodeid = 3, hypername = "seminar")
th_remove_hyper(lab_net, "events", hypername = "seminar")

th_add_layer(lab_net, "temporary", mode = 1)
th_remove_layer(lab_net, "temporary")

th_define_attr(lab_nodes, "score", "int")
th_set_attr(lab_nodes, nodeid = 1, attrname = "score", attrvalue = 7)
print(th_get_attr(lab_nodes, 1, "score"))
th_remove_attr(lab_nodes, nodeid = 1, attrname = "score")
th_undefine_attr(lab_nodes, "score")

th_generate_attr(lab_nodes, "random_score", attrtype = "int", min = 1, max = 10)
print(th_get_attr_summary(lab_nodes, "random_score"))

# Generate a random layer and run random-walk commands.
random_nodes <- th_create_nodeset("random_nodes", createnodes = 20)
random_net <- th_create_network("random_net", random_nodes)
th_add_layer(random_net, "er_layer", mode = 1, directed = FALSE, valuetype = "binary")
th_generate(random_net, "er_layer", type = "er", p = 1)
print(th_get_all_edges(random_net, "er_layer", offset = 0, limit = 10))

walk_nodes <- th_create_nodeset("walk_nodes", createnodes = 6)
walk_net <- th_create_network("walk_net", walk_nodes)
th_add_layer(walk_net, "walk_layer", mode = 1, directed = FALSE, valuetype = "binary")
th_add_edge(walk_net, "walk_layer", 1, 2)
th_add_edge(walk_net, "walk_layer", 2, 3)
th_add_edge(walk_net, "walk_layer", 3, 4)
th_add_edge(walk_net, "walk_layer", 4, 5)
th_define_attr(walk_net, "group", "int")
for (node in 1:5) {
  th_set_attr(walk_net, nodeid = node, attrname = "group", attrvalue = ifelse(node <= 2, 1, 2))
}

rw_distances <- th_rwdistances(
  "rw_distances",
  walk_net,
  attrname = "group",
  maxsteps = 30L,
  layernames = "walk_layer",
  walkfactor = 0.5,
  balanced = FALSE,
  weighted = FALSE,
  backtrack = FALSE,
  savesteps = FALSE
)
rw_fpt <- th_rwfpt(
  "rw_fpt",
  walk_net,
  attrname = "group",
  maxsteps = 30L,
  layernames = "walk_layer",
  walkfactor = 0.5,
  minpairobs = 1L,
  balanced = FALSE,
  weighted = FALSE
)
print(th_info(rw_distances))
print(th_info(rw_fpt))

# Miscellaneous session helpers.
th_setting("verbose", FALSE)
th_setting("nodecache", TRUE)
th_setting("nodecache", FALSE)
th_random_seed(42L)
th_sync_wd()

# Export, save, import, and load a Threadle script.
exports_dir <- getwd()
dir.create(exports_dir, recursive = TRUE, showWarnings = FALSE)

kinship_edges_file <- file.path(exports_dir, "mynet_kinship_edges.tsv")
mynet_gexf_file <- file.path(exports_dir, "mynet_kinship.gexf")
mynet_saved_file <- file.path(exports_dir, "mynet_saved.tsv")

th_export_layer(mynet, "kinship", file = kinship_edges_file, header = TRUE, sep = "\t")
th_export(mynet, format = "gexf", file = mynet_gexf_file, layername = "kinship")
th_save_file(mynet, file = mynet_saved_file)
stopifnot(file.exists(kinship_edges_file))
stopifnot(file.exists(mynet_gexf_file))
stopifnot(file.exists(mynet_saved_file))

mynet_nodeset_copy <- th_load_file(
  "mynet_nodeset_copy",
  file.path(system.file("extdata", package = "threadleR"), "mynet_nodesetfile.tsv"),
  type = "nodeset"
)
print(th_info(mynet_nodeset_copy))

import_nodes <- th_create_nodeset("import_nodes", createnodes = 3)
import_net <- th_create_network("import_net", import_nodes)
th_add_layer(import_net, "imported_edges", mode = 1, directed = FALSE, valuetype = "valued")
edge_file <- file.path(exports_dir, "import_edges.tsv")
write.table(
  data.frame(node1 = c(1, 2), node2 = c(2, 3), value = c(4, 5)),
  file = edge_file,
  sep = "\t",
  row.names = FALSE,
  col.names = FALSE,
  quote = FALSE
)
th_import_layer(
  import_net,
  "imported_edges",
  file = edge_file,
  format = "edgelist",
  node1col = 0,
  node2col = 1,
  valuecol = 2,
  header = FALSE,
  sep = "\t",
  addmissingnodes = FALSE
)
print(th_get_edge(import_net, "imported_edges", 1, 2))

script_file <- file.path(exports_dir, "threadle_script.txt")
writeLines(c(
  "script_nodes = createnodeset(name=script_nodes, createnodes=3)",
  "script_net = createnetwork(name=script_net, nodeset=script_nodes)",
  "addlayer(network=script_net, layername=script_layer, mode=1, directed=false, valuetype=binary, selfties=false)"
), con = script_file)
th_load_script(script_file)
print(th_info("script_net"))

# Remove objects and stop Threadle.
delete_me <- th_create_nodeset("delete_me", createnodes = 1)
th_delete(delete_me)
print(th_i())

th_delete_all()
print(th_i())

th_stop_threadle()
cat("Workflow complete. Output directory: ", exports_dir, "\n", sep = "")
