structure CorrectnessTest =
struct

  (* Utils *)
  fun tokens s = String.tokens Char.isSpace s
  fun toInt s = case Int.fromString s of SOME n => n | NONE => raise Fail "bad int"
  fun toReal n = Real.fromInt n
  fun realAbs x = if x < 0.0 then ~x else x
  val NUM_BINS = 256 val MAX_ITER = 50 val CONVERGE_THRESH = 0.5

  (* IO *)
  fun readImage (file: string) : int array * int * int =
    let val ins = TextIO.openIn file
        val hdr = case TextIO.inputLine ins of SOME s => s | NONE => raise Fail "no header"
        val (h, w, c) = case tokens hdr of [a,b,d] => (toInt a, toInt b, toInt d) | _ => raise Fail "bad header"
        val _ = if c <> 1 then raise Fail "grayscale only" else ()
        val n = h * w val arr = Array.array (n, 0)
        fun rd i = if i >= n then ()
          else case TextIO.scanStream (Int.scan StringCvt.DEC) ins of
                 SOME v => (Array.update (arr, i, Int.max(0, Int.min(255, v))); rd (i+1))
               | NONE => raise Fail "missing pixel"
        val _ = rd 0 val _ = TextIO.closeIn ins
    in (arr, h, w) end

  (* Otsu-Seq *)
  fun histSeq (pix: int array) : int array =
    let val n = Array.length pix val h = Array.array (NUM_BINS, 0)
        fun lp i = if i >= n then () else let val v = Array.sub (pix, i)
            in Array.update (h, v, Array.sub (h, v) + 1); lp (i+1) end
    in lp 0; h end

  (* Otsu-Par *)
  fun histPar (pix: int array, numWorkers: int) : int array =
    let val n = Array.length pix val chunkSize = (n + numWorkers - 1) div numWorkers
        val localHists = Array.tabulate (numWorkers, fn _ => Array.array (NUM_BINS, 0))
        val _ = Parallel.parfor (0, numWorkers) (fn w =>
          let val lo = w * chunkSize val hi = Int.min (n, lo + chunkSize) val myHist = Array.sub (localHists, w)
              fun lp i = if i >= hi then () else let val v = Array.sub (pix, i)
                  in Array.update (myHist, v, Array.sub (myHist, v) + 1); lp (i+1) end
          in lp lo end)
        val final = Array.array (NUM_BINS, 0)
        fun mergeBin bin = let fun sumW w acc = if w >= numWorkers then acc
              else sumW (w+1) (acc + Array.sub (Array.sub (localHists, w), bin))
            in Array.update (final, bin, sumW 0 0) end
        fun mergeAll bin = if bin >= NUM_BINS then () else (mergeBin bin; mergeAll (bin+1))
        val _ = mergeAll 0
    in final end

  fun otsuThreshold (hist: int array, total: int) : int =
    let val n = toReal total val cumW = Array.array (NUM_BINS, 0.0) val cumM = Array.array (NUM_BINS, 0.0)
        fun build i prevW prevM = if i >= NUM_BINS then ()
          else let val p = toReal (Array.sub (hist, i)) / n val newW = prevW + p val newM = prevM + toReal i * p
               in Array.update (cumW, i, newW); Array.update (cumM, i, newM); build (i+1) newW newM end
        val _ = build 0 0.0 0.0 val mu = Array.sub (cumM, NUM_BINS - 1)
        fun find t bestT bestV = if t >= NUM_BINS - 1 then bestT
          else let val w0 = Array.sub (cumW, t) val w1 = 1.0 - w0
               in if w0 < 1E~10 orelse w1 < 1E~10 then find (t+1) bestT bestV
                  else let val m0 = Array.sub (cumM, t) / w0 val m1 = (mu - Array.sub (cumM, t)) / w1
                           val d = m0 - m1 val v = w0 * w1 * d * d
                       in if v > bestV then find (t+1) t v else find (t+1) bestT bestV end end
    in find 0 0 ~1.0 end

  fun applyThresholdSeq (pix: int array, t: int) : int array =
    Array.tabulate (Array.length pix, fn i => if Array.sub (pix, i) >= t then 255 else 0)

  fun applyThresholdPar (pix: int array, t: int) : int array =
    let val n = Array.length pix
        val result = Parallel.tabulate (0, n) (fn i => if Array.sub (pix, i) >= t then 255 else 0)
    in Array.tabulate (n, fn i => Seq.nth result i) end

  fun otsuSeq (pix: int array) : int * int array =
    let val hist = histSeq pix val t = otsuThreshold (hist, Array.length pix) val out = applyThresholdSeq (pix, t)
    in (t, out) end

  fun otsuPar (pix: int array, numWorkers: int) : int * int array =
    let val hist = histPar (pix, numWorkers) val t = otsuThreshold (hist, Array.length pix) val out = applyThresholdPar (pix, t)
    in (t, out) end

  (* KMeans *)
  fun initCentroids (k: int) : real array = Array.tabulate (k, fn i => toReal i * 255.0 / toReal (k - 1))

  fun nearestCentroid (centroids: real array, k: int, pixVal: int) : int =
    let val pv = toReal pixVal
        fun find i bestIdx bestDist = if i >= k then bestIdx
          else let val d = realAbs (pv - Array.sub (centroids, i))
               in if d < bestDist then find (i+1) i d else find (i+1) bestIdx bestDist end
    in find 0 0 1000.0 end

  fun kmeansSeq (pix: int array, k: int) : int array =
    let val n = Array.length pix val labels = Array.array (n, 0)
        fun iterate centroids iter =
          let val sums = Array.array (k, 0.0) val counts = Array.array (k, 0)
              fun process i = if i >= n then ()
                else let val pixVal = Array.sub (pix, i) val lbl = nearestCentroid (centroids, k, pixVal)
                     in Array.update (labels, i, lbl);
                        Array.update (sums, lbl, Array.sub (sums, lbl) + toReal pixVal);
                        Array.update (counts, lbl, Array.sub (counts, lbl) + 1); process (i + 1) end
              val _ = process 0
              val newCentroids = Array.tabulate (k, fn i => let val c = Array.sub (counts, i)
                  in if c = 0 then Array.sub (centroids, i) else Array.sub (sums, i) / toReal c end)
              fun maxMove i acc = if i >= k then acc
                else let val mv = realAbs (Array.sub (newCentroids, i) - Array.sub (centroids, i))
                     in maxMove (i+1) (if mv > acc then mv else acc) end
              val movement = maxMove 0 0.0
          in if iter >= MAX_ITER orelse movement < CONVERGE_THRESH then labels else iterate newCentroids (iter + 1) end
    in iterate (initCentroids k) 1 end

  fun kmeansPar (pix: int array, k: int, numWorkers: int) : int array =
    let val n = Array.length pix val labels = Array.array (n, 0) val chunkSize = (n + numWorkers - 1) div numWorkers
        fun iterate centroids iter =
          let val workerSums = Array.tabulate (numWorkers, fn _ => Array.array (k, 0.0))
              val workerCounts = Array.tabulate (numWorkers, fn _ => Array.array (k, 0))
              val _ = Parallel.parfor (0, numWorkers) (fn w =>
                let val lo = w * chunkSize val hi = Int.min (n, lo + chunkSize)
                    val mySums = Array.sub (workerSums, w) val myCounts = Array.sub (workerCounts, w)
                    fun processPixel i = if i >= hi then ()
                      else let val pixVal = Array.sub (pix, i) val lbl = nearestCentroid (centroids, k, pixVal)
                           in Array.update (labels, i, lbl);
                              Array.update (mySums, lbl, Array.sub (mySums, lbl) + toReal pixVal);
                              Array.update (myCounts, lbl, Array.sub (myCounts, lbl) + 1); processPixel (i + 1) end
                in processPixel lo end)
              val finalSums = Array.array (k, 0.0) val finalCounts = Array.array (k, 0)
              val _ = Parallel.parfor (0, k) (fn c =>
                let fun sumW w sAcc cAcc = if w >= numWorkers then (sAcc, cAcc)
                      else sumW (w+1) (sAcc + Array.sub (Array.sub (workerSums, w), c)) (cAcc + Array.sub (Array.sub (workerCounts, w), c))
                    val (s, cnt) = sumW 0 0.0 0
                in Array.update (finalSums, c, s); Array.update (finalCounts, c, cnt) end)
              val newCentroids = Array.tabulate (k, fn i => let val c = Array.sub (finalCounts, i)
                  in if c = 0 then Array.sub (centroids, i) else Array.sub (finalSums, i) / toReal c end)
              fun maxMove i acc = if i >= k then acc
                else let val mv = realAbs (Array.sub (newCentroids, i) - Array.sub (centroids, i))
                     in maxMove (i+1) (if mv > acc then mv else acc) end
              val movement = maxMove 0 0.0
          in if iter >= MAX_ITER orelse movement < CONVERGE_THRESH then labels else iterate newCentroids (iter + 1) end
    in iterate (initCentroids k) 1 end

  (* Compare *)
  fun compareArrays (a: int array, b: int array) : int * int =
    let val n = Array.length a
        fun count i matches mismatches = if i >= n then (matches, mismatches)
          else if Array.sub (a, i) = Array.sub (b, i) then count (i+1) (matches+1) mismatches
          else count (i+1) matches (mismatches+1)
    in count 0 0 0 end

  fun printResult (name: string, n: int, matches: int, mismatches: int) =
    let val matchPct = 100.0 * toReal matches / toReal n
    in print ("    Pixels:     " ^ Int.toString n ^ "\n");
       print ("    Matches:    " ^ Int.toString matches ^ " (" ^ Real.fmt (StringCvt.FIX (SOME 2)) matchPct ^ "%)\n");
       print ("    Mismatches: " ^ Int.toString mismatches ^ "\n");
       if mismatches = 0 then print "    Status:     PASS\n"
       else if matchPct > 99.9 then print "    Status:     PASS (>99.9%)\n"
       else if matchPct > 99.0 then print "    Status:     WARN (>99%)\n"
       else print "    Status:     FAIL\n" end

  (* Tests *)
  fun testOtsu (filename: string, numWorkers: int) =
    let val _ = print ("\n  [Otsu] " ^ filename ^ "\n")
        val (pix, h, w) = readImage filename val n = h * w
        val (seqT, seqOut) = otsuSeq pix val (parT, parOut) = otsuPar (pix, numWorkers)
        val _ = print ("    Seq threshold: " ^ Int.toString seqT ^ "\n")
        val _ = print ("    Par threshold: " ^ Int.toString parT ^ "\n")
        val (matches, mismatches) = compareArrays (seqOut, parOut)
    in printResult ("Otsu", n, matches, mismatches); mismatches = 0 orelse seqT = parT end

  fun testKMeans (filename: string, k: int, numWorkers: int) =
    let val _ = print ("\n  [K-Means K=" ^ Int.toString k ^ "] " ^ filename ^ "\n")
        val (pix, h, w) = readImage filename val n = h * w
        val seqLabels = kmeansSeq (pix, k) val parLabels = kmeansPar (pix, k, numWorkers)
        val (matches, mismatches) = compareArrays (seqLabels, parLabels)
    in printResult ("K-Means", n, matches, mismatches); mismatches = 0 orelse (100.0 * toReal matches / toReal n) > 99.0 end

  (* Main *)
  fun main () =
    let val numWorkers = 16
        val _ = print "============================================================\n"
        val _ = print "  CORRECTNESS TESTS: Sequential vs Parallel\n"
        val _ = print "============================================================\n"
        val _ = print ("  Workers: " ^ Int.toString numWorkers ^ "\n")
        val testFiles = ["test_images/test_bimodal.txt", "test_images/test_1024x1024.txt", "test_images/test_2048x2048.txt"]
        val _ = print "\n  OTSU THRESHOLDING\n"
        val otsuResults = List.map (fn f => (testOtsu (f, numWorkers) handle _ => (print ("  [Skipped: " ^ f ^ "]\n"); false))) testFiles
        val _ = print "\n  K-MEANS SEGMENTATION\n"
        val kmeansResults = List.concat [
          List.map (fn f => testKMeans (f, 2, numWorkers) handle _ => false) testFiles,
          List.map (fn f => testKMeans (f, 4, numWorkers) handle _ => false) testFiles]
        val allResults = otsuResults @ kmeansResults
        val passed = List.length (List.filter (fn x => x) allResults) val total = List.length allResults
        val _ = print "\n============================================================\n"
        val _ = print "  SUMMARY\n"
        val _ = print "============================================================\n"
        val _ = print ("  Tests passed: " ^ Int.toString passed ^ "/" ^ Int.toString total ^ "\n")
        val _ = if passed = total then print "  Overall: ALL TESTS PASSED\n" else print "  Overall: SOME TESTS FAILED\n"
        val _ = print "============================================================\n"
    in () end

  val _ = main ()

end
