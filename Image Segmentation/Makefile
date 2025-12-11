# Makefile for Image Segmentation

# ============ OTSU THRESHOLDING ============

# Sequential (MLton)
seq: ThresholdSeq.sml
	mlton -output threshold_seq ThresholdSeq.sml

# Parallel (MPL)
par: *.sml ThresholdPar.mlb lib-local/*.sml lib-local/*.mlb
	mpl -default-type int64 -default-type word64 -output threshold_par ThresholdPar.mlb

# Benchmarks
benchmark: Benchmark.sml Benchmark.mlb lib-local/*.sml lib-local/*.mlb
	mpl -default-type int64 -default-type word64 -output benchmark Benchmark.mlb

benchmark-opt: BenchmarkOpt.sml BenchmarkOpt.mlb lib-local/*.sml lib-local/*.mlb
	mpl -default-type int64 -default-type word64 -output benchmark_opt BenchmarkOpt.mlb

benchmark-max: BenchmarkMax.sml BenchmarkMax.mlb lib-local/*.sml lib-local/*.mlb
	mpl -default-type int64 -default-type word64 -output benchmark_max BenchmarkMax.mlb

# ============ K-MEANS SEGMENTATION ============

# Sequential K-Means (MLton)
kmeans-seq: KMeansSeq.sml
	mlton -output kmeans_seq KMeansSeq.sml

# Parallel K-Means (MPL)
kmeans-par: KMeansPar.sml KMeansPar.mlb lib-local/*.sml lib-local/*.mlb
	mpl -default-type int64 -default-type word64 -output kmeans_par KMeansPar.mlb

# K-Means Benchmark
kmeans-bench: KMeansBenchmark.sml KMeansBenchmark.mlb lib-local/*.sml lib-local/*.mlb
	mpl -default-type int64 -default-type word64 -output kmeans_bench KMeansBenchmark.mlb

# ============ CORRECTNESS TEST ============

# Tests both Otsu and K-Means (seq vs par comparison)
correctness: CorrectnessTest.sml CorrectnessTest.mlb lib-local/*.sml lib-local/*.mlb
	mpl -default-type int64 -default-type word64 -output correctness_test CorrectnessTest.mlb

# ============ TEST IMAGES ============

test-images:
	python3 generate_test_images.py

test-images-large:
	python3 generate_large_images.py

# ============ CONVENIENCE TARGETS ============

all: seq par kmeans-seq kmeans-par

benchmarks: benchmark-max kmeans-bench

clean:
	rm -f threshold_seq threshold_par benchmark benchmark_opt benchmark_max
	rm -f kmeans_seq kmeans_par kmeans_bench correctness_test
	rm -f output*.txt

# ============ QUICK RUN ============

run-otsu: benchmark-max test-images
	./benchmark_max @mpl procs 16 --

run-kmeans: kmeans-bench test-images-large
	./kmeans_bench @mpl procs 16 --
