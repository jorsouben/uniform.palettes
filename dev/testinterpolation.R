# Sample data
x <- c(0, 1, 2, 3, 4, 5, 6)
y <- c(0, 0.8, 0.9, 0.1, -0.8, -1, 0) # y[1] == y[7] for periodic

# Dense x values for smooth plotting
# x_dense <- seq(min(x), max(x), length.out = 300)
x_dense <- seq(min(x), max(x), length.out = 17)

# Create spline functions
fmm_fun <- splinefun(x, y, method = "fmm")
natural_fun <- splinefun(x, y, method = "natural")
periodic_fun <- splinefun(x, y, method = "periodic")
monoH_fun <- splinefun(x, y, method = "monoH.FC")
hyman_fun <- splinefun(x, y, method = "hyman")

# Evaluate splines
y_fmm <- fmm_fun(x_dense)
y_natural <- natural_fun(x_dense)
y_periodic <- periodic_fun(x_dense)
y_monoH <- monoH_fun(x_dense)
y_hyman <- hyman_fun(x_dense)

# Plot
plot(x, y, pch = 19, col = "black", main = "Spline Interpolation Methods", xlab = "x", ylab = "y")
lines(x_dense, y_fmm, col = "blue", lwd = 2)
lines(x_dense, y_natural, col = "green", lwd = 2)
lines(x_dense, y_periodic, col = "purple", lwd = 2)
lines(x_dense, y_monoH, col = "orange", lwd = 2)
lines(x_dense, y_hyman, col = "red", lwd = 2)

legend("topright",
  legend = c("fmm", "natural", "periodic", "monoH.FC", "hyman"),
  col = c("blue", "green", "purple", "orange", "red"), lwd = 2
)

# We need to ensure that the original points are included in the
# interpolated curves
# To do so, we could optionally fix one or two of the original points
# by using the 'ties' argument in splinefun, e.g., ties = list(x = x[c(1, 4)])
# Or we could use the 'method' argument in splinefun to specify
# a method that guarantees interpolation at the original points.?
# For example, 'fmm' and 'natural' methods guarantee interpolation at the original points.
# NO: we should compute the x_dense values to include the original fixed x values
# given the k original points and the n output x_dense points, we estimate
# the closest integer number of resulting intervals between the 2 fixed points
# and then we can compute the x_dense values accordingly.
# If only one point is fixed, we compute the xmin and max in x_dense
# so that the fixed point is included.
# For example, if we fix x[1] and x[4], we can compute:
# fixed_points <- c(x[1], x[4])
# n_intervals <- round((max(x) - min(x)) / (fixed_points[2] - fixed_points[1]) * (length(x_dense) - 1))
# x_dense <- seq(min(x), max(x), length.out = n_intervals +

# 1)
# This ensures that the fixed points are included in x_dense.
# If we fix only x[1], we can compute:
# fixed_point <- x[1]
# x_dense <- seq(min(x), max(x), length.out = length(x_dense))
# and then ensure that fixed_point is included in x_dense.
# This way, we can ensure that the original points are included in the interpolated curves.
# However, in this example, we are not fixing any points, so we proceed as is.
# Note: The periodic method requires that the first and last y values are equal.
