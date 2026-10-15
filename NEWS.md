# threadleR 0.6.0

Catches threadleR up with the Threadle 1.3 CLI.

* New wrappers for all Threadle 1.3 analyses: centrality (`th_betweenness_centrality()`,
  `th_closeness_centrality()`, `th_harmonic_centrality()`, `th_eigenvector_centrality()`,
  `th_edge_betweenness_centrality()`, `th_pagerank()`), structural cohesion and community
  detection (`th_clustering_coefficient()`, `th_transitivity()`, `th_coreness()`,
  `th_constraint()`, `th_effective_size()`, `th_community_detection()`,
  `th_small_world_coefficient()`), assortativity (`th_assortativity()`,
  `th_degree_assortativity()`), census and distance measures (`th_dyad_census()`,
  `th_triadic_census()`, `th_diameter()`, `th_eccentricity()`), layer/network structural
  operations (`th_duplicate()`, `th_rename()`, `th_binary_to_valued()`,
  `th_symmetric_to_directed()`, `th_rewire()`, `th_merge_layers()`, `th_merge_networks()`,
  `th_merge_nodesets()`, `th_import_network()`), and attribute/data tools
  (`th_convert_attr()`, `th_set_attrs()`, `th_get_attrs_table()`, `th_get_layer_stats()`,
  `th_get_alter_details()`, `th_system_info()`).
* **Breaking change**: `th_shortest_paths()` now wraps the CLI's repurposed
  `shortestpaths()` command — an exact batch lookup for specific node pairs
  (`node1ids`/`node2ids`), not the old attribute-based distance distribution. That old
  behavior is now `th_shortest_paths_attributes()`, wrapping the CLI's renamed
  `shortestpathsattributes()`.
* **Breaking change**: `th_rwed()` and `th_rwdistances()` are removed. The CLI's `rwed`
  command was removed in Threadle 1.3; use `th_rwfpt()` instead. `th_rwdistances()`
  wrapped a CLI command that no longer exists either.

# threadleR 0.5.1

# threadleR 0.5.0

# threadleR 0.4.3

# threadleR 0.4.2

# threadleR 0.4.1

# threadleR 0.4.0

# threadleR 0.3.0

# threadleR 0.2.1

* Initial CRAN submission.
