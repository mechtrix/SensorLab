#' ggplot function to draw a confusion matrix based on the caret package
#'
#' @param cm a confusion matrix as returned by the caret package/function confusionMatrix
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#' ggconmat(cm)
#' }
#'
ggconmat <- function(cm){

cm_table <- cm$table |>
  as.data.frame() |>
  dplyr::mutate(
    rating =
      dplyr::case_when(
        Prediction == Reference ~ "good",
        TRUE ~ "bad"
      )
  )

p <- cm_table |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = Reference,
      y = Prediction,
      label = Freq,
      fill = rating
    )
  )+
  ggplot2::geom_tile()+
  ggplot2::geom_text(
    size = 8
  )+
  ggplot2::scale_x_discrete(
    expand = c(0,0,0,0),
    position = "top"
  )+
  ggplot2::scale_y_discrete(
    expand = c(0,0,0,0),
    limit = rev
  )+
  ggplot2::labs(
    title = "Confusion Matrix",
    fill = ""
  )+
  ggplot2::scale_fill_manual(
    values = c("good" = "green4","bad" = "red3")
  )+
  ggplot2::theme_classic(
    base_size = 20
  )+
  ggplot2::theme(
    legend.position = "bottom"
  )

return(p)

}
