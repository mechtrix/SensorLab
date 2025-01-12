#' plot rule of ten: ggplot function to plot the rule of ten
#'
#' @returns a ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#' plt_rule_of_ten()
#' }
#'
plt_rule_of_ten <- function(){

  df <- data.frame(stage = c(
    "Idea",
    "Development",
    "Preperation",
    "Production",
    "Endtest",
    "Delivery"
  ),
  cost = c(
    1,
    10,
    100,
    1000,
    10000,
    100000
  ),
  stage_idx = c(
    1,2,3,4,5,6
  )
  )

  model <- stats::nls(cost ~ exp(a*stage_idx),data = df)

  df_pred <- df %>%
    modelr::add_predictions(model = model)

  df_new <- data.frame(stage_idx = seq(1,6,0.001)) %>%
    modelr::add_predictions(model = model)

  df_highlight <- df_new %>%
    dplyr::filter(stage_idx == 1|stage_idx == 2|stage_idx == 3|stage_idx == 4|stage_idx == 4.82|stage_idx==6) %>%
    tibble::add_column(cost = c("1 \u20ac","10 \u20ac","100 \u20ac","1.000 \u20ac","10.000 \u20ac","100.000 \u20ac")) %>%
    tibble::add_column(x = c(1,2,3,4,5.15,5.7)) %>%
    tibble::add_column(y = c (3000,3500,4500,7000,10000,100000))




  plt.rot <- df_new %>%
    ggplot2::ggplot()+
    ggplot2::geom_line(
      ggplot2::aes(
        x = stage_idx,
        y=pred),
      linewidth = 1.5,
      color = "steelblue"
      )+
    ggplot2::geom_label(
      data = df_highlight,
      ggplot2::aes(
        x = x,
        y = y,
        label = paste0(format(cost,big.mark = "."))),
      label.size = NA
      )+
    ggplot2::labs(title = "Rule of Ten",
         x = "",
         y = "Cost of single error")+
    ggplot2::scale_x_continuous(breaks = seq(1,6),
                       labels = df$stage)+
    ggplot2::scale_y_continuous(labels = scales::dollar_format(suffix="\u20ac",prefix=""),
                       breaks = seq(0,1000000,10000))+
    ggplot2::theme_minimal()+
    ggplot2::theme(text = ggplot2::element_text(size = 20),
          panel.grid.major.x = ggplot2::element_blank(),
          panel.grid.minor.x = ggplot2::element_blank(),
          panel.grid = ggplot2::element_blank(),
          axis.line = ggplot2::element_line(color = "black"))

  return(plt.rot)

# ggsave(filename = "rule_of_ten.png",plot = plt.rot, units = "px",width = 3000, height = 2000)

}
