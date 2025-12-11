## Parallel Otsu Thresholding – Design, Optimizations, and Scaling

### Goal

Implement **normal thresholding** and **Otsu’s method** for image segmentation in SML, then **parallelize Otsu** using MPL to:

- **Compare sequential vs parallel** performance.
- **Scale with image size** and **number of processors**.
- Understand **where speedup comes from** and **why it saturates**.

---

## Algorithms Implemented

### Normal Thresholding

**Idea**: Fixed threshold \(T\), classify each pixel independently.

- **Rule**:  
  - If pixel \(\ge T\) → 255 (foreground / white)  
  - Else → 0 (background / black)
- **Work per pixel**: 1 comparison + 1 store.
- **Parallelization**: Embarrassingly parallel over pixels; we use `Parallel.tabulate` in the parallel version.

### Otsu’s Method (Histogram-based Thresholding)

**Goal**: Automatically find the threshold \(T^\*\) that best separates foreground and background by **maximizing between-class variance**.

Steps:

1. **Build histogram** `H[0..255]` from all pixels.
2. For each threshold \(t \in [0, 254]\):
   - Split pixels into:
     - Class 0: intensities \(\le t\)
     - Class 1: intensities \(> t\)
   - Compute:
     - \(w_0\): fraction of pixels in class 0 (`cumW[t]`)
     - \(w_1 = 1 - w_0\)
     - \(\mu_0\): mean intensity of class 0 (`cumM[t] / w0`)
     - \(\mu_1\): mean intensity of class 1
   - **Between-class variance**:
     \[
     \sigma_B^2 = w_0 \cdot w_1 \cdot (\mu_0 - \mu_1)^2
     \]
3. **Pick \(t^\*\)** that maximizes \(\sigma_B^2\).
4. Apply normal thresholding at \(t^\*\).

Key properties:

- One **full pass** over all \(N\) pixels to build `H`.
- A **fixed** amount of work over 256 bins for cumulative sums + variance search.
- One more **full pass** over all pixels to apply the chosen threshold.

---

## Sequential vs Parallel Designs

### Sequential Baseline (`ThresholdSeq.sml`, reused in benchmarks)

1. **Histogram**:
   - Single tight loop over `int array` of length \(N = H \times W\).
   - Direct `Array.sub` and `Array.update`.
2. **Otsu**:
   - Sequential cumulative sums over 256 bins.
   - Sequential scan to find the best `t`.
3. **Apply**:
   - Tight loop to write an output `int array` (0 or 255).

This version is **bandwidth-bound**: performance dominated by memory reads/writes, not arithmetic.

### Initial Parallel Design (`ThresholdPar.sml` + `Benchmark.sml`)

**Parallel primitives used**:

- `Parallel.reduce` over chunks to build histograms.
- `Parallel.reduce` over 256 thresholds for Otsu argmax.
- `Parallel.tabulate` over pixels to apply the threshold.

Approach:

- Partition pixels into many chunks.
- Compute partial histograms in parallel.
- Merge histograms via a tree of reductions.
- Parallelize Otsu’s argmax across 256 bins.
- Apply threshold in parallel with `Parallel.tabulate`.

Result:

- **Correct** and **nicely parallel**, but:
  - Many small parallel tasks.
  - Multiple `Parallel.reduce` calls.
  - Extra allocation for intermediate histograms.
  - Overhead significant for small/medium images.

---

## First-Level Optimization (`BenchmarkOpt.sml`)

Focus: **reduce Parallel overhead** and tune **grain size / chunks**.

Key ideas:

- Use `histPar` with **adaptively chosen grain**.
- Keep histogram & Otsu parallelization, but:
  - Avoid `ForkJoin` directly.
  - Use only `Parallel.reduce` and `Parallel.tabulate`.
- Build a dedicated **benchmark driver** that:
  - Tests multiple image sizes.
  - Runs multiple iterations per configuration.
  - Performs a **chunk scaling experiment** for 4096×4096.

### Representative Results (16 procs, 64 chunks, `benchmark_opt`)

- **256×256**: speedup ≈ **0.4×** (parallel slower: overhead dominates).
- **1024×1024**: speedup ≈ **1.0×** (break-even).
- **2048×2048**: speedup ≈ **2.0×**.
- **4096×4096**: speedup ≈ **2.1×**.

Chunk scaling (4096×4096, 16 procs):

| Chunks | Speedup |
|--------|---------|
| 4      | 0.80×   |
| 8      | 1.33×   |
| 16     | 1.92×   |
| 32     | 2.11×   |
| 64     | 2.33×   |
| 128    | 2.59×   |
| 256    | 2.17×   |
| 512    | 2.49×   |

**Takeaways**:

- **Small images**: parallel overhead > useful work.
- **Large images**: reasonable speedup (~2–2.6×).
- There is a **sweet spot** in chunk counts: too few chunks wastes cores, too many increases scheduling overhead.

---

## Maximum Optimization (`BenchmarkMax.sml`)

Goal: **“Milk” all realistic performance** for Otsu on this CPU/MPL setup, by:

- Minimizing the number of parallel primitives.
- Eliminating unnecessary allocations and `Seq.nth` in hot loops.
- Using **local per-worker histograms** with a **cheap merge**.
- Keeping Otsu’s 256-bin math **sequential** (overhead of parallelizing it isn’t worth it).

### Design Decisions

#### 1. Histogram – Local per-worker arrays + sequential merge

- Let `numWorkers` ≈ number of MPL “processors”.
- Partition pixels into `numWorkers` contiguous chunks.
- For each worker `w` (in parallel):
  - Build a **local 256-bin histogram** `H_w` in a tight loop.
  - Completely independent, great locality.
- After all workers finish:
  - Merge histograms **sequentially**:
    - For each bin `b` from 0 to 255:
      - Sum `H_0[b] + H_1[b] + ... + H_{P-1}[b]`.
  - Cost: `P × 256` integer adds – negligible compared to scanning millions of pixels.

This replaces:

- Tree-structured `Parallel.reduce` over many temporary histograms
- With:
  - **One `Parallel.parfor`** over workers + a **single, tiny merge loop**.

#### 2. Otsu – Sequential, tight code

- Kept exactly **sequential**:
  - Only 256 iterations for cumulative sums.
  - Only 256 iterations for argmax.
- Parallelization here would:
  - Introduce runtime overhead larger than the work.
  - Not change asymptotic behavior for large images.

#### 3. Threshold Apply – Single `Parallel.tabulate`

- Apply the final threshold in **one `Parallel.tabulate`** over all pixels.
- No extra conversions in the hot path beyond a final `Seq.t` → `array` copy (or keep as `Seq.t` in `fullParUltra`).

#### 4. Two Parallel Variants

- **Par-Opt**:
  - Local histograms + **sequential** merge.
  - Otsu sequential.
  - Threshold apply with `Parallel.tabulate`.
- **Par-Ultra**:
  - Local histograms.
  - **Parallel merge** over 256 bins with `Parallel.parfor`.
  - Otsu sequential.
  - Threshold apply with `Parallel.tabulate`.

Par-Ultra is slightly more aggressive; Par-Opt is simpler and often close in performance.

---

## Measured Results – Maximum Optimization

### Full Benchmark (`benchmark_max`, 16 workers)

From the run:

```text
test_images/test_bimodal.txt
  Size: 256x256 (65536 pixels)
  Workers: 16
  Sequential    : avg=0.0022s, min=0.0021s
  Par-Opt       : avg=0.0012s, min=0.0009s
  Par-Ultra     : avg=0.0010s, min=0.0007s
  Speedup (Opt):   2.21x
  Speedup (Ultra): 3.16x

test_images/test_1024x1024.txt
  Size: 1024x1024 (1048576 pixels)
  Workers: 16
  Sequential    : avg=0.0159s, min=0.0153s
  Par-Opt       : avg=0.0088s, min=0.0082s
  Par-Ultra     : avg=0.0062s, min=0.0054s
  Speedup (Opt):   1.87x
  Speedup (Ultra): 2.82x

test_images/test_2048x2048.txt
  Size: 2048x2048 (4194304 pixels)
  Workers: 16
  Sequential    : avg=0.0960s, min=0.0955s
  Par-Opt       : avg=0.0321s, min=0.0278s
  Par-Ultra     : avg=0.0239s, min=0.0207s
  Speedup (Opt):   3.43x
  Speedup (Ultra): 4.62x

test_images/test_4096x4096.txt
  Size: 4096x4096 (16777216 pixels)
  Workers: 16
  Sequential    : avg=0.2273s, min=0.2252s
  Par-Opt       : avg=0.1114s, min=0.1084s
  Par-Ultra     : avg=0.0780s, min=0.0758s
  Speedup (Opt):   2.08x
  Speedup (Ultra): 2.97x
```

**Observations**:

- For **small images** (256×256), overhead still matters, but Par-Ultra already reaches ~3× speedup.
- For **medium images** (1024×1024), speedup ~2–3×.
- For **large images** (2048×2048, 4096×4096), speedups of:
  - **3–4.6×** (2048×2048).
  - **~3×** (4096×4096) with Par-Ultra.
- These are **significantly better** than the earlier ~2× from the initial parallel design.

### Worker Count Scaling (4096×4096, Par-Ultra)

```text
Workers | Seq (s)  | Par (s)  | Speedup
--------+----------+----------+--------
1       | 0.2501   | 0.2034   | 1.23x
2       | 0.2317   | 0.1971   | 1.18x
4       | 0.2295   | 0.1944   | 1.18x
8       | 0.2238   | 0.1190   | 1.88x
16      | 0.2472   | 0.0740   | 3.34x
32      | 0.3098   | 0.0540   | 5.74x
64      | 0.2200   | 0.0501   | 4.39x
```

Notes:

- Sequential times vary a bit due to noise; using **min times** is more representative.
- Overall trend:
  - Little benefit from 1 → 4 workers.
  - Big gains from 4 → 16.
  - Continued improvement up to 32 workers (best speedup here ~5–6×).
  - At 64 workers, speedup dips again (oversubscription and scheduling overhead).

**Interpretation**:

- We see **strong scaling** up to a point; beyond that:
  - The MPL runtime and OS start oversubscribing cores.
  - Memory bandwidth becomes the main bottleneck.

---

## Why Speedup Saturates (High-Level Analysis)

Otsu’s method in this implementation is fundamentally:

- **Two memory-bound passes** over the image (histogram + threshold apply).
- **A tiny O(256)** compute phase for Otsu’s math.

On a multicore CPU:

- Per pixel, work is minimal: 1–2 loads, a few integer ops, 1 store.
- Once multiple cores are active, they are mostly **waiting on memory**, not ALUs.
- **Memory bandwidth** and **cache behavior** limit the effective speedup.
- The parallel runtime adds **fixed overhead** (task creation, synchronization) per parallel region.

Even with perfect parallel code:

- Amdahl’s law + bandwidth limits mean that **speedup cannot approach the number of cores** for such a light-weight kernel.
- In practice, for this setup, seeing **~3–6× speedup** for large images is already **close to the realistic ceiling**.

---

## Could We Do Even Better?

Within this exact model (CPU + MPL + Otsu):

- We have already:
  - Flattened the image to a single `int array`.
  - Removed `Seq.nth` from hot loops.
  - Minimized `Parallel` calls (one `parfor` for hist, one `tabulate` for apply).
  - Used local per-worker histograms with cheap merges.
  - Avoided parallelizing the tiny 256-bin Otsu phase (kept sequential).
  - Tuned worker counts and chunks.

Further gains would likely require:

- **Changing the algorithm**:
  - More compute-heavy segmentation (e.g., k-means on intensity features, mean-shift, graph-based segmentation) to increase arithmetic intensity.
- **Changing the hardware**:
  - GPU implementation (histogram + threshold via CUDA/OpenCL).
  - SIMD/vectorized CPU kernels with hand-tuned intrinsics.

For the scope of this project and the CIMS MPL environment, the current design is **effectively “maxed out”** for Otsu.

---

## How to Reproduce the Experiments

From the `PPA-F25-ImageProcessing` directory on CIMS:

```bash
module load mlton   # if needed for sequential build

make clean

# Build sequential and baseline parallel
make seq
make par

# Build optimized and max-optimized benchmarks
make benchmark-opt
make benchmark-max

# Generate standard test images (up to 4096x4096)
make test-images

# Generate larger test images (e.g., 8192x8192)
make test-images-large

# Run original optimized benchmark (chunk scaling, etc.)
./benchmark_opt @mpl procs 16 --

# Run maximum-optimized benchmark (full + worker scaling)
./benchmark_max @mpl procs 16 --
```

You can vary `@mpl procs P` to explore scaling with processor count.

---

## K-Means Segmentation – Results and Comparison

After Otsu, we implemented **K-Means clustering on intensities** as a second segmentation method, using the **same image set** (256×256 up to 8192×8192). The parallel K-Means uses **local per-worker accumulators + merge**, analogous to the “max-optimized” Otsu histogram design.

### K-Means vs Otsu: High-Level Takeaways

- **Otsu**:
  - \(O(N)\) work, very low arithmetic per pixel (memory-bound).
  - Best speedups: **~3×** at 4096×4096 (16 workers), **~5–6×** in aggressive worker scaling.
- **K-Means**:
  - \(O(N \cdot K \cdot \text{iterations})\) work; significantly more arithmetic per pixel.
  - Much better parallel scaling, especially for large images and larger K.
  - On the same machine and images we see **~10–15× speedups** at 4k/8k with 64+ workers, and up to **~20–25×** when oversubscribing more workers.

### Representative K-Means Speedups (K = 4, 64 Workers)

From `kmeans_bench @mpl procs 64 --` (first full run with 64 workers):

| Image Size  | Pixels      | Seq Time (min) | Par Time (min) | Speedup |
|-------------|------------:|---------------:|---------------:|--------:|
| 256×256     |      65,536 | 0.0171 s       | 0.0030 s       | 5.7×    |
| 1024×1024   |   1,048,576 | 0.3348 s       | 0.0287 s       | 11.7×   |
| 2048×2048   |   4,194,304 | 1.3883 s       | 0.1151 s       | 12.1×   |
| 4096×4096   |  16,777,216 | 4.4068 s       | 0.3571 s       | 12.3×   |
| 8192×8192   |  67,108,864 | 21.2145 s      | 1.7728 s       | 12.0×   |

**Interpretation:**

- For **small images** (256×256), K-Means is already doing enough work that parallelism helps (~5–6×).
- For **medium and large images** (≥1024×1024), speedup **stabilizes around ~12×** with 64 workers.
- This is significantly better than Otsu, reflecting K-Means’ much higher compute per pixel.

### Worker Scaling (4096×4096, K = 4)

Key part of the worker scaling table:

```text
Workers | Seq (s)  | Par (s)  | Speedup
--------+----------+----------+--------
1       | 4.47     | 5.17     | 0.86x
2       | 4.50     | 4.89     | 0.92x
4       | 4.48     | 4.95     | 0.91x
8       | 4.54     | 2.59     | 1.75x
16      | 4.39     | 1.30     | 3.37x
32      | 4.34     | 0.67     | 6.53x
64      | 4.36     | 0.36     | 12.2x
128     | 4.41     | 0.19     | 23.5x
256     | 4.48     | 0.21     | 21.3x
```

**Interpretation:**

- Little benefit at 1–4 workers (parallel overhead dominates).
- Strong scaling from **8 → 64 workers**, reaching ~12× speedup at 64 workers.
- At 128/256 workers, we see **very high reported speedups (~20–24×)**, but these are likely affected by:
  - Oversubscription relative to physical cores.
  - Run-to-run noise and scheduler interactions.
  - The fact that the **sequential time is fixed**, while parallel work is spread over many logical workers.

For **fair comparison**, 64 workers is a good “representative maximum” on this machine: high speedup without extreme oversubscription.

### Worker Scaling (8192×8192, K = 4)

From the 8k×8k scaling section:

```text
Workers | Seq (s)  | Par (s)  | Speedup
--------+----------+----------+--------
1       | 22.48    | 25.00    | 0.90x
2       | 22.63    | 25.79    | 0.88x
4       | 22.86    | 24.84    | 0.92x
8       | 22.78    | 13.08    | 1.74x
16      | 22.68    | 6.62     | 3.43x
32      | 21.62    | 3.34     | 6.48x
64      | 22.61    | 1.77     | 12.8x
128     | 22.44    | 0.93     | 24.1x
256     | 22.38    | 1.21     | 18.5x
```

Again, **64 workers** gives ~12–13× speedup on the largest image, and 128/256 show even larger nominal speedups but with more noise and oversubscription effects.

### K Scaling (Effect of Number of Clusters)

With 64 workers, 4096×4096:

```text
K   | Seq (s)  | Par (s)  | Speedup
----+----------+----------+--------
2   | 1.26     | 0.10     | 13.1x
4   | 4.48     | 0.36     | 12.5x
8   | 12.54    | 0.85     | 14.8x
16  | 15.57    | 1.01     | 15.4x
```

And for 8192×8192:

```text
K   | Seq (s)  | Par (s)  | Speedup
----+----------+----------+--------
2   | 5.47     | 0.39     | 14.2x
4   | 21.67    | 1.79     | 12.1x
8   | 48.90    | 3.34     | 14.7x
16  | 51.59    | 3.37     | 15.3x
```

**Interpretation:**

- As **K increases**, both sequential and parallel times grow roughly linearly.
- **Speedup stays consistently high (~12–15×)** across K, confirming that:
  - The parallel implementation scales well with additional cluster work.
  - Extra compute per pixel just gives more opportunity to amortize parallel overhead.

### Which Results Are Most Representative?

- For **Otsu**, the most representative numbers are:
  - ~2–3× speedup at 4k with moderate workers (16).
  - ~5–6× in more aggressive worker-scaling runs, limited by memory bandwidth.
- For **K-Means**:
  - The **64-worker runs** on **4096×4096** and **8192×8192** are the cleanest indicators:
    - ~12–13× speedup consistently across these large images.
  - The very high speedups at 128/256 workers (20–25×) are interesting but less “portable”:
    - They depend more on oversubscription and the specific scheduler behavior.
    - For a report, it is better to **highlight 64-worker results** and mention 128/256 as “peak” speedups with caveats.

### Are the Results Satisfactory?

Yes:

- **Otsu** is bandwidth-bound; your **~3–6× speedup** matches realistic limits for its low arithmetic intensity.
- **K-Means** is compute-heavy; your **~12× speedup at 4k/8k with 64 workers** (and ~20×+ at higher worker counts) demonstrates:
  - Much better utilization of available cores.
  - The value of choosing algorithms with more work per pixel for parallel speedups.

Together, these two methods clearly show:

- How algorithmic structure (O(N) vs O(N·K·iterations)) affects parallel scalability.
- How MPL’s primitives (`Parallel.parfor`, `Parallel.tabulate`) can be used to **max out realistic speedup** on this CIMS setup.

---

## Summary

- Implemented **normal** and **Otsu’s** thresholding in SML.
- Developed **sequential**, **parallel**, and **multiple optimized** parallel Otsu versions.
- Implemented **K-Means intensity clustering** (sequential and parallel) on the **same image set**.
- Carefully tuned:
  - Histogram and centroid update strategies.
  - Use of MPL primitives (`Parallel.parfor`, `Parallel.tabulate`).
  - Chunk sizes and worker counts, including scaling to 64+ workers.
- Achieved:
  - For Otsu: **~3× speedup** at 4k with 16 workers, **~5–6×** in worker-scaling experiments.
  - For K-Means: **~12–13× speedup** at 4k/8k with 64 workers, and **up to ~20–25×** at higher worker counts.
- Overall, the results are **satisfactory and illustrative**:
  - Otsu shows the limits of parallelizing a bandwidth-bound kernel.
  - K-Means shows how a more compute-heavy method can exploit many cores for significantly higher speedups.

