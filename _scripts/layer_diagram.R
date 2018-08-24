#
# Draw figure for atmospheric layer model
#
library(tidyverse)
library(stringr)


make_layer_diagram = function(n_layers, boundary = TRUE) {
  layer_unit = 9 / (2 * (n_layers + 1) + n_layers)
  layer_thickness = layer_unit
  layer_spacing = 2 * layer_unit

  visible = data_frame(x = 1, xend = 2, y = 10, yend = 1,
                       xlab = 1.05, ylab = 10, just = 0,
                       name = "Visible")

  layers = data_frame(name = c("Earth", paste("Atmospheric Layer", seq(n_layers))),
                      class = c("Earth", rep("Atmosphere", n_layers)),
                      xmin = 0, xmax = 10,
                      ymin = c(0, 1 + seq(n_layers) * (layer_thickness + layer_spacing) - layer_thickness),
                      ymax = c(1, 1 + seq(n_layers) * (layer_thickness + layer_spacing)),
                      x = 5.5,
                      y = c(0.5, 1 + seq(n_layers) * (layer_thickness + layer_spacing) - 0.5 * layer_thickness)
  )

  if (n_layers == 1) layers$name[2] = "Atmospheric Layer"

  ir = data_frame(x = 5.5, xend = 6.5, y = layers$ymax[n_layers + 1], yend = 10,
                  xlab = 6.2, ylab = (y + yend) / 2,  just = 0,
                  name = paste0("I['", n_layers, ",up']"))
  if (n_layers >= 1) {
    ir_up = data_frame(x = 4, xend = 5, y = head(layers$ymax, -1), yend = tail(layers$ymin, -1),
                       xlab = 4.3, ylab = (y + yend) / 2, just = 1,
                       name = paste0("I['", c("ground", seq(n_layers - 1))[1:n_layers], ",up']"))
    ir_down = data_frame(x = 6, xend = 7, y =  tail(layers$ymin, -1), yend = head(layers$ymax, -1),
                         xlab = 6.7, ylab = (y + yend) / 2, just = 0,
                         name = paste0("I['", seq(n_layers), ",down']"))
    ir = bind_rows(ir, ir_up, ir_down)
  } else {
    ir$name[1] = "I['ground,up']"
  }

  ir <- na.omit(ir)
  layers <- na.omit(layers)

  p <- ggplot() +
    geom_rect(data = layers, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = class)) +
    geom_segment(data = visible, mapping = aes(x = x, y = y, xend = xend, yend = yend), arrow = arrow(type = "closed"), size = 1) +
    geom_segment(data = ir, mapping = aes(x = x, y = y, xend = xend, yend = yend), arrow = arrow(type = "closed"), size = 1, linetype = "dashed") +
    geom_text(mapping = aes(x = x, y = y, color = class, label = name), data = layers, size = 7, hjust = 0.5, vjust = 0.5)  +
    geom_text(data = ir, mapping = aes(x = xlab, y = ylab, label = name, hjust = just), vjust = 0.5, size = 5, parse = TRUE) +
    annotate("text", x = visible$xlab, y = visible$ylab, label = visible$name, hjust = 0, vjust = 1, size = 5) +
    xlim(0,10) + ylim(0,10) +
    scale_fill_manual(values = c(Earth = "gray30", Atmosphere = "gray80"), guide = "none") +
    scale_color_manual(values = c(Earth = "white", Atmosphere = "black"), guide = "none") +
    theme_bw() +
    theme(axis.text = element_blank(), axis.title = element_blank(),
          axis.ticks = element_blank(), panel.grid = element_blank())

  if (boundary) {
    boundary_y <- 10 - 0.25 * layer_spacing
    p <- p +
      geom_hline(yintercept = boundary_y, linetype = "dotted", size = 1) +
      annotate("text", x = 10, y =boundary_y + 0.1, hjust = 1, vjust = 0,
               label = "Boundary to space", color = "black", size = 5)
  }

  p
}

make_nuclear_winter_diagram = function(boundary = TRUE) {
  n_layers = 1
  layer_unit = 9 / (2 * (n_layers + 1) + n_layers)
  layer_thickness = layer_unit
  layer_spacing = 2 * layer_unit

  layers = data_frame(name = c("Earth", "Dusty atmosphere"),
                      class = c("Earth", "Atmosphere"),
                      xmin = 0, xmax = 10,
                      ymin = c(0, 1 + seq(n_layers) * (layer_thickness + layer_spacing) - layer_thickness),
                      ymax = c(1, 1 + seq(n_layers) * (layer_thickness + layer_spacing)),
                      x = 5.5,
                      y = c(0.5, 1 + seq(n_layers) * (layer_thickness + layer_spacing) - 0.5 * layer_thickness)
  )

  visible = data_frame(x = 1, xend = 2, y = 10, yend = max(layers$ymax),
                       xlab = 1.05, ylab = 10, just = 0,
                       name = "Visible")


  ir = data_frame(x = 5.5, xend = 6.5, y = layers$ymax[n_layers + 1], yend = 10,
                  xlab = 6.2, ylab = (y + yend) / 2,  just = 0,
                  name = paste0("I['", n_layers, ",up']"))
  if (n_layers >= 1) {
    ir_up = data_frame(x = 4, xend = 5, y = head(layers$ymax, -1), yend = tail(layers$ymin, -1),
                       xlab = 4.3, ylab = (y + yend) / 2, just = 1,
                       name = paste0("I['", c("ground", seq(n_layers - 1))[1:n_layers], ",up']"))
    ir_down = data_frame(x = 6, xend = 7, y =  tail(layers$ymin, -1), yend = head(layers$ymax, -1),
                         xlab = 6.7, ylab = (y + yend) / 2, just = 0,
                         name = paste0("I['", seq(n_layers), ",down']"))
    ir = bind_rows(ir, ir_up, ir_down)
  } else {
    ir$name[1] = "I['ground,up']"
  }

  ir <- na.omit(ir)
  layers <- na.omit(layers)

  p <- ggplot() +
    geom_rect(data = layers, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = class)) +
    geom_segment(data = visible, mapping = aes(x = x, y = y, xend = xend, yend = yend), arrow = arrow(type = "closed"), size = 1) +
    geom_segment(data = ir, mapping = aes(x = x, y = y, xend = xend, yend = yend), arrow = arrow(type = "closed"), size = 1, linetype = "dashed") +
    geom_text(mapping = aes(x = x, y = y, color = class, label = name), data = layers, size = 7, hjust = 0.5, vjust = 0.5)  +
    geom_text(data = ir, mapping = aes(x = xlab, y = ylab, label = name, hjust = just), vjust = 0.5, size = 5, parse = TRUE) +
    annotate("text", x = visible$xlab, y = visible$ylab, label = visible$name, hjust = 0, vjust = 1, size = 5) +
    xlim(0,10) + ylim(0,10) +
    scale_fill_manual(values = c(Earth = "gray30", Atmosphere = "gray60"), guide = "none") +
    scale_color_manual(values = c(Earth = "white", Atmosphere = "black"), guide = "none") +
    theme_bw() +
    theme(axis.text = element_blank(), axis.title = element_blank(),
          axis.ticks = element_blank(), panel.grid = element_blank())

  if (boundary) {
    boundary_y <- 10 - 0.25 * layer_spacing
    p <- p +
      geom_hline(yintercept = boundary_y, linetype = "dotted", size = 1) +
      annotate("text", x = 10, y =boundary_y + 0.1, hjust = 1, vjust = 0,
               label = "Boundary to space", color = "black", size = 5)
  }

  p
}
