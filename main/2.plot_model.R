# Required packages
# install.packages(c("dlnm", "scales"))
library(dlnm)
library(scales)

plot_results <- function(prediction, output_dir = "results/plots", label = "plot") {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Extract objects
  pred_temp <- prediction$prediction
  pred_heat <- prediction$prediction_heat
  pred_cold <- prediction$prediction_cold
  temp_range <- prediction$temp_range
  t_quantiles <- prediction$temp_quantiles
  
  # Plot cumulative exposure-response
  filename1 <- file.path(output_dir, paste0(label, "_cum_response.tiff"))
  tiff(filename1, width = 900, height = 600, res = 150)
  plot(pred_temp, "overall", lwd = 2, ylab = "Relative Risk",
       xlab = "Temperature (°C)", main = paste0(label, " - Cumulative RR"))
  dev.off()
  
  # Plot lag-response slices
  filename2 <- file.path(output_dir, paste0(label, "_lag_response.tiff"))
  tiff(filename2, width = 900, height = 600, res = 150)
  plot(pred_temp, "slices", var = t_quantiles["99%"], col = "red", ylab = "Relative Risk",
       main = paste0(label, " - Lag Response"), ci = "n")
  lines(pred_temp, "slices", var = t_quantiles["90%"], col = "orange")
  lines(pred_temp, "slices", var = t_quantiles["10%"], col = "deepskyblue")
  lines(pred_temp, "slices", var = t_quantiles["1%"], col = "blue")
  legend("topright", legend = c("99th", "90th", "10th", "1st"),
         col = c("red", "orange", "deepskyblue", "blue"), lty = 1, bty = "n")
  dev.off()
  
  # Plot 3D RR surface
  filename3 <- file.path(output_dir, paste0(label, "_3d_surface.tiff"))
  tiff(filename3, width = 1000, height = 1000, res = 150)
  plot(pred_temp, xlab = "Temperature (°C)", zlab = "RR", theta = 200, phi = 40, lphi = 30,
       main = paste0(label, " - 3D RR"))
  dev.off()
}
