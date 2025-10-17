# uniform.palettes

An R package for creating and manipulating perceptually uniform color palettes, with a focus on CIEDE2000 color difference metrics.

This package is an experimental attempt to study and potentially automate the generation of scientific colour maps following the principles established by Fabio Crameri's Scientific Colour Maps[^1]. It provides tools for both generating perceptually uniform palettes and diagnosing their properties.

⚠️ **Note**: This package is in very early development and not yet ready for production use. Features are experimental and the API may change.

## Features

- 🎨 Create and manipulate color palettes with the `ColorMap` class
- 🔄 Convert between color spaces (HEX, RGB, LAB)
- 📊 Analyze perceptual uniformity using CIEDE2000 metrics
- 🔧 Tools for palette equalization and interpolation
- 📈 Visualization utilities for perceptual qualities and distortion.

## Installation

The package is not yet on CRAN. You can try it by cloning the repository and using `devtools`:

```r
# Clone the repository first, then:
devtools::load_all()
```

## Basic Usage

```r
# Create a ColorMap object from hex colors
pal <- ColorMap$new(c("#FF0000", "#00FF00", "#0000FF"))

# Get colors in different spaces
pal$get_rgb()  # RGB values
pal$get_lab()  # LAB values

# Analyze perceptual differences
pal$deltas()     # Sequential CIEDE2000 differences
pal$cum_deltas() # Cumulative differences

# Visualize the palette
pal$swatch()     # Color swatches
pal$bands()      # Color bands
pal$sineramp()   # Sinusoidal ramp test

# Diagnostics plot
pal |> diagnostic_plot()

# Perceptual distances equalization
pal |> equalize(n = 256)
```

## Key Functions
- `as_colormap()`: Convert to a `ColorMap` class object
- `equalize()`: Equalize color differences in a palette
- `channel_interpolation()`: Interpolate between colors
- `diagnostic_plot()`: Generate diagnostics following Scientific Colour Maps principles
- `plot_volcano_3d()`: 3D palette distortion simulation demo with plotly

## Contributing

The package is in active development. Issues, suggestions, and pull requests are welcome, but please note that the API is not yet stable.

## TODO

Development roadmap and planned improvements:

### Core Functionality
- [ ] Improve color space interpolation methods (currently basic and rough)
- [ ] Rewrite equalization algorithms in C for better performance
- [ ] Add Lightness range optimization
- [ ] Implement PCA and other methods to smooth palette paths in color space
- [ ] Study and implement methods for CVD (Color Vision Deficiency) friendly palettes
- [ ] Generalize to support additional perceptual distance metrics beyond CIEDE2000

### Visualization and Diagnostics
- [ ] Implement 3D color space visualization tools (from dev prototypes)
- [ ] Complete Scientific Colour Maps diagnostics suite
- [ ] Add more interactive visualization tools

### Code Quality and Distribution
- [ ] Add test coverage for new functionality
- [ ] Remove deprecated functions and old approaches
- [ ] Documentation improvements
- [ ] Prepare for CRAN submission

## License

This package is released under the MIT License.

## Acknowledgments

- Color difference calculations based on the CIEDE2000 formula
- Uses the farver package for efficient color space conversions
- Includes IGN (Instituto Geográfico Nacional) data for examples

[^1]: Crameri, F., Shephard, G. E. & Heron, P. J. The misuse of colour in science communication. Nature Communications 11, 5444 (2020). DOI: [10.1038/s41467-020-19160-7](https://doi.org/10.1038/s41467-020-19160-7)
