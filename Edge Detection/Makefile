# Makefile for Canny Edge Detection

# ============ SEQUENTIAL ============

seq: CannySeq.sml
	mlton -output canny_seq CannySeq.sml

# ============ PARALLEL (MPL) ============

par: CannyPar.sml CannyPar.mlb lib-local/*.sml lib-local/*.mlb
	mpl -default-type int64 -default-type word64 -output canny_par CannyPar.mlb

# ============ BENCHMARK ============

benchmark: CannyBenchmark.sml CannyBenchmark.mlb lib-local/*.sml lib-local/*.mlb
	mpl -default-type int64 -default-type word64 -output canny_bench CannyBenchmark.mlb

# ============ CORRECTNESS TEST ============

correctness: CannyCorrectness.sml CannyCorrectness.mlb lib-local/*.sml lib-local/*.mlb
	mpl -default-type int64 -default-type word64 -output canny_test CannyCorrectness.mlb

# ============ TEST IMAGES ============

test-images:
	python3 generate_test_images.py

# ============ SETUP ============

# Copy lib directories from Segmentation (run once)
setup:
	@if [ ! -d "lib" ]; then \
		echo "Copying lib directories from Segmentation..."; \
		cp -r ../Segmentation/lib .; \
		cp -r ../Segmentation/lib-local .; \
		echo "Done!"; \
	else \
		echo "lib directories already exist."; \
	fi

# ============ CONVENIENCE ============

all: seq par benchmark correctness

clean:
	rm -f canny_seq canny_par canny_bench canny_test
	rm -f *.txt
	rm -rf test_images

# ============ QUICK RUN ============

run-seq: seq test-images
	./canny_seq test_images/test_1024x1024.txt output_seq.txt

run-par: par test-images
	./canny_par @mpl procs 16 -- test_images/test_1024x1024.txt output_par.txt

run-bench: benchmark test-images
	./canny_bench @mpl procs 64 --

run-test: correctness test-images
	./canny_test @mpl procs 16 --

