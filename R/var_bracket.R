theme(plot.margin = unit(c(1, 1, 1, 1.3), "lines")) +
    annotation_custom(
        grob = textGrob(label = "Individual Level", gp = gpar(cex = .7, fontface="italic"), rot=90),
        ymin = 5, ymax = 5,
        xmin = -7.8, xmax = -7.8) +
    annotation_custom(grob = linesGrob(), xmin = -7.5, xmax = -7.5, ymin = .7, ymax = 9.4) +
    annotation_custom(grob = linesGrob(), xmin = -7.5, xmax = -7, ymin = 9.4, ymax = 9.4) +
    annotation_custom(grob = linesGrob(), xmin = -7.5, xmax = -7, ymin = .7, ymax = .7)
