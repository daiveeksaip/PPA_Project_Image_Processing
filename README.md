# Parallel Image Processing - MPL Implementation

This project implements parallel image processing algorithms in Standard ML using MPL (MaPLe).

## Key Files

### Edge Detection - Canny Algorithm
| File | Description |
|------|-------------|
| `CannySeq.sml` | **Core sequential implementation** - Full Canny pipeline (Blur → Sobel → NMS → Threshold → Hysteresis) |
| `CannyPar.sml` | **Core parallel implementation** - Same pipeline parallelized using `Parallel.parfor` with chunked workers |
| `CannyBenchmark.sml` | Benchmark comparing Seq vs Par with timing and speedup measurements |
| `CannyCorrectness.sml` | Verifies parallel output matches sequential (pixel-by-pixel comparison) |

### Image Segmentation - Otsu Thresholding
| File | Description |
|------|-------------|
| `ThresholdSeq.sml` | **Core sequential Otsu** - Histogram computation + between-class variance optimization |
| `ThresholdPar.sml` | **Core parallel Otsu** - Uses `Parallel.reduce` for histogram and threshold search |
| `BenchmarkMax.sml` | **Best optimized benchmark** - Local histograms per worker + sequential merge |

### Image Segmentation - K-Means Clustering
| File | Description |
|------|-------------|
| `KMeansSeq.sml` | **Core sequential K-Means** - Iterative centroid assignment and update |
| `KMeansPar.sml` | **Core parallel K-Means** - Per-worker accumulators with parallel merge |
| `KMeansBenchmark.sml` | Full benchmark with worker scaling and K-value scaling experiments |

### Correctness Testing
| File | Description |
|------|-------------|
| `CorrectnessTest.sml` | Tests both Otsu and K-Means (Seq vs Par comparison) |

## Project Structure

```
PPA-F25-ImageProcessing/
├── Edge Detection/       # Canny Edge Detection
│   ├── CannySeq.sml      # Sequential implementation
│   ├── CannyPar.sml      # Parallel implementation
│   ├── CannyBenchmark.sml # Performance benchmarks
│   ├── CannyCorrectness.sml # Correctness tests
│   └── Makefile
│
├── Segmentation/         # Image Segmentation (Otsu + K-Means)
│   ├── ThresholdSeq.sml  # Sequential Otsu thresholding
│   ├── ThresholdPar.sml  # Parallel Otsu thresholding
│   ├── Benchmark.sml     # Basic benchmark
│   ├── BenchmarkOpt.sml  # Optimized benchmark
│   ├── BenchmarkMax.sml  # Maximum optimization benchmark
│   ├── CorrectnessTest.sml # Correctness tests
│   ├── KMeansSeq.sml     # Sequential K-Means
│   ├── KMeansPar.sml     # Parallel K-Means
│   ├── KMeansBenchmark.sml # K-Means benchmark
│   └── Makefile
│
└── README.md             # This file
```

## Prerequisites

- MPL (MaPLe) compiler installed
- Access to CIMS NYU cluster (recommended for parallel execution)

## Running on CIMS NYU

1. SSH into CIMS:
   ```bash
   ssh <netid>@access.cims.nyu.edu
   ```

2. Load MPL module:
   ```bash
   module load mpl
   ```

## Edge Detection

### Generate Test Images
```bash
cd "Edge Detection"
python3 generate_test_images.py
```

### Run Correctness Tests
```bash
make correctness
./canny_correctness
```

### Run Benchmarks
```bash
make benchmark
./canny_benchmark @mpl procs 16 --
```

### Run Single Image Processing
```bash
# Sequential
make seq
./canny_seq test_images/test_1024x1024.txt output.txt

# Parallel
make par
./canny_par test_images/test_1024x1024.txt output.txt @mpl procs 16 --
```

## Image Segmentation

### Generate Test Images
```bash
cd Segmentation
python3 generate_test_images.py
python3 generate_large_images.py  # For larger benchmark images
```

### Run Correctness Tests
```bash
make correctness
./correctness_test @mpl procs 16 --
```

### Run Otsu Benchmarks
```bash
# Basic benchmark
make benchmark
./benchmark @mpl procs 16 --

# Optimized benchmark
make benchmark_opt
./benchmark_opt @mpl procs 16 --

# Maximum optimization benchmark
make benchmark_max
./benchmark_max @mpl procs 16 --
```

### Run K-Means Benchmarks
```bash
make kmeans_benchmark
./kmeans_benchmark @mpl procs 16 --
```

### Run Single Image Segmentation
```bash
# Sequential Otsu
make threshold_seq
./threshold_seq input.txt output.txt otsu

# Parallel Otsu
make threshold_par
./threshold_par input.txt output.txt otsu @mpl procs 16 --

# Sequential K-Means
make kmeans_seq
./kmeans_seq input.txt output.txt 4

# Parallel K-Means
make kmeans_par
./kmeans_par input.txt output.txt 4 @mpl procs 16 --
```

## Image Format

Input images use a simple text format:
```
H W C
pixel_values...
```
- H: height
- W: width
- C: channels (1 for grayscale)
- pixel_values: space-separated integers 0-255

## Adjusting Parallelism

Change the number of processors using the `@mpl procs N` flag:
```bash
./benchmark @mpl procs 32 --   # Use 32 processors
./benchmark @mpl procs 64 --   # Use 64 processors
```

## Expected Results

### Otsu Thresholding
- Small images (256x256): ~1-2x speedup
- Large images (4096x4096+): ~3-6x speedup

### Canny Edge Detection
- More compute-intensive per pixel
- Large images (4096x4096+): ~8-15x+ speedup expected

### K-Means Segmentation
- Most compute-intensive (O(N * K * iterations))
- Large images: ~15-20x+ speedup expected


