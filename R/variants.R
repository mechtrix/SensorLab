#' Compute possible variants in manufacturing
#'
#' @param num_products number of products to manufacture
#' @param num_tools_per_product average number of tools used per product
#' @param total_num_available_tools total number of available tools
#'
#' @returns a simulated dartaframe
#' @export
#'
#' @examples
#'  dataset <- comp_variants()

comp_variants <- function(
  num_products = 5,
  num_tools_per_product = 5,
  total_num_available_tools = 7
) {
  sim_grid <- tidyr::expand_grid(
    num_products,
    num_tools_per_product,
    total_num_available_tools
  )

  sim_res <- sim_grid |>
    dplyr::mutate(
      combinations_per_product = choose(
        total_num_available_tools,
        num_tools_per_product
      ),
      variants = combinations_per_product^num_products
    )

  return(sim_res)
}
