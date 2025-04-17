# Set general theme for plots
plot_theme <- theme_minimal(base_size = 12) +
  theme(
    # Bold title
    plot.title = element_text(face = "bold", size = rel(0.7)),
    # Plain subtitle that is grey
    plot.subtitle = element_text(face = "plain", size = rel(1), color = "grey70"),
    # Slightly smaller bold legend title
    legend.title = element_text(face = "bold", size = 10),  # Adjust the size here for a smaller legend title
    # Bold axis titles
    axis.title = element_text(face = "bold"),
    # Smaller legend text
    legend.text = element_text(size = 9)  # Adjust the size here for smaller legend text
  )
