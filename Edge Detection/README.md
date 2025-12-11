# Canny Edge Detection - Parallel Implementation

Sequential and parallel implementations of the Canny edge detector in SML using MPL.

## Algorithm Pipeline

1. **Gaussian Blur** (3x3) - Noise reduction
2. **Sobel Gradients** - Compute Gx, Gy, magnitude
3. **Non-Maximum Suppression** - Thin edges to 1-pixel width
4. **Double Threshold** - Classify strong/weak/non-edges
5. **Hysteresis** - Connect weak edges to strong edges

## Files

| File | Description |
|------|-------------|
| `CannySeq.sml` | Sequential implementation |
| `CannyPar.sml` | Parallel implementation (MPL) |
| `CannyBenchmark.sml` | Performance benchmarking |
| `generate_test_images.py` | Test image generator |

## Setup (Run Once)

```bash
# Copy MPL libraries from Segmentation
make setup

# Or manually:
cp -r ../Segmentation/lib .
cp -r ../Segmentation/lib-local .
```

## Build

```bash
# Build sequential
make seq

# Build parallel
make par

# Build benchmark
make benchmark
```

## Run

```bash
# Generate test images
make test-images

# Run sequential
./canny_seq test_images/test_1024x1024.txt output.txt

# Run parallel (16 procs)
./canny_par @mpl procs 16 -- test_images/test_1024x1024.txt output.txt

# Run benchmark (64 procs)
./canny_bench @mpl procs 64 --
```

## Parallelization Strategy

Each stage processes pixels independently -> embarrassingly parallel:

- **Blur**: Each output pixel = weighted sum of 3x3 neighborhood
- **Sobel**: Each pixel = 2 convolutions + magnitude calculation
- **NMS**: Each pixel = direction quantization + neighbor comparison
- **Threshold**: Each pixel = compare to thresholds
- **Hysteresis**: Each pixel = check 8 neighbors

All stages use `Parallel.parfor` over pixel chunks.

## Expected Speedups

Canny is more compute-intensive than Otsu/K-means histogram operations:
- ~40-50 arithmetic ops per pixel per stage
- Multiple passes over image data
- Expected: **8-15x speedup** on large images with sufficient workers

## Input/Output Format

Same as Segmentation:
```
H W 1
p00 p01 p02 ... p0(W-1)
p10 p11 ...
...
```
- Line 1: Height Width Channels (C=1 for grayscale)
- Following lines: pixel values 0-255

