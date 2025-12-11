# PPA-F25-ImageProcessing

Image Segmentation - Thresholding (Sequential & Parallel MPL)

## Files

| File | Description |
|------|-------------|
| `ThresholdSeq.sml` | Sequential implementation (MLton) |
| `ThresholdPar.sml` | Parallel implementation (MPL) |
| `ThresholdSeq.mlb` | MLton build file |
| `ThresholdPar.mlb` | MPL build file |
| `generate_test_images.py` | Test image generator |

## Algorithms

### Normal Thresholding
```
if pixel >= threshold then 255 else 0
```

### Otsu's Method
Automatically finds optimal threshold by maximizing between-class variance:

```
1. Build histogram H[0..255]
2. For each t in 0..254:
   - w0 = P(background), w1 = P(foreground)
   - μ0 = mean(background), μ1 = mean(foreground)
   - σ²_B = w0 × w1 × (μ0 - μ1)²
3. Return argmax(σ²_B)
```

## Parallelization (ThresholdPar.sml)

| Step | Parallel Primitive |
|------|-------------------|
| Histogram building | `Parallel.reduce` with histogram merge |
| Variance search | `Parallel.reduce` over 256 thresholds |
| Threshold apply | `Parallel.tabulate` over pixels |

## Build & Test

```bash
# On CIMS server:
make clean
make seq        # Build sequential
make par        # Build parallel

# Generate test images
make test-images

# Run tests
./threshold_seq test_images/test_bimodal.txt output.txt otsu
./threshold_par test_images/test_bimodal.txt output.txt otsu

# With processor control
./threshold_par @mpl procs 8 -- test_images/test_4096x4096.txt output.txt otsu
```

## Usage

```bash
./threshold_seq <input.txt> <output.txt> <method> [threshold]
./threshold_par <input.txt> <output.txt> <method> [threshold]

# Methods: normal, otsu
# threshold: required for 'normal' method (0-255)
```

## Input/Output Format

```
H W C
p00 p01 p02 ... p0(W-1)
p10 p11 ...
...
```

- Line 1: Height Width Channels (C=1 for grayscale)
- Following H lines: W space-separated pixel values (0-255)

## Other Parallelizable Segmentation Algorithms

1. **Multi-level Otsu** - Multiple thresholds, parallel over combinations
2. **K-Means Clustering** - Parallel assignment & centroid updates
3. **Mean Shift** - Parallel per-pixel iterations
4. **SLIC Superpixels** - Grid-based parallel k-means
5. **Graph-based (Felzenszwalb)** - Parallel Union-Find
