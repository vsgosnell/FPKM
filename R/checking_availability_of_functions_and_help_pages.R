# Test script to check if functions are available and documented

# Load your package


# # List of functions to test
# functions_to_test <- c("calculate_fpkm", "log_transform_fpkm", "merge_fpkm_tables",
#                        "plot_fpkm_boxplot", "plot_fpkm_distribution", "plot_pca_fpkm",
#                        "process_fpkm_data", "filter_low_expression", "get_top_expressed_genes",
#                        "normalize_counts_tpm", "plot_expression_heatmap", "plot_gene_expression",
#                        "plot_sample_correlation", "read_rna_seq_data", "summarize_expression")
#
# # Loop over each function and check if it exists and if the help page is available
# for (func in functions_to_test) {
#   # Check if the function exists
#   if (exists(func)) {
#     print(paste("Function", func, "exists."))
#
#     # Try to open the help page for the function
#     tryCatch({
#       help(func)
#       print(paste("Help page for", func, "is available."))
#     }, error = function(e) {
#       print(paste("No help page found for", func))
#     })
#   } else {
#     print(paste("Function", func, "does not exist."))
#   }
# }

