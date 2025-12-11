structure KMeansBenchmark =
struct

  val MAX_ITER = 50
  val CONVERGE_THRESH = 0.5
  val NUM_RUNS = 3

  (* Utils *)
  fun tokens s = String.tokens Char.isSpace s
  fun toInt s = case Int.fromString s of SOME n => n | NONE => raise Fail "bad int"
  fun toReal n = Real.fromInt n
  fun realAbs x = if x < 0.0 then ~x else x

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

  (* Sequential *)
  fun initCentroids (k: int) : real array = Array.tabulate (k, fn i => toReal i * 255.0 / toReal (k - 1))

  fun nearestCentroid (centroids: real array, k: int, pixVal: int) : int =
    let val pv = toReal pixVal
        fun find i bestIdx bestDist = if i >= k then bestIdx
          else let val d = realAbs (pv - Array.sub (centroids, i))
               in if d < bestDist then find (i+1) i d else find (i+1) bestIdx bestDist end
    in find 0 0 1000.0 end

  fun kmeansSeqIter (pix: int array, centroids: real array, iter: int) : int =
    let val n = Array.length pix val k = Array.length centroids
        val sums = Array.array (k, 0.0) val counts = Array.array (k, 0)
        fun process i = if i >= n then ()
          else let val pixVal = Array.sub (pix, i) val lbl = nearestCentroid (centroids, k, pixVal)
               in Array.update (sums, lbl, Array.sub (sums, lbl) + toReal pixVal);
                  Array.update (counts, lbl, Array.sub (counts, lbl) + 1); process (i + 1) end
        val _ = process 0
        val newCentroids = Array.tabulate (k, fn i => let val c = Array.sub (counts, i)
            in if c = 0 then Array.sub (centroids, i) else Array.sub (sums, i) / toReal c end)
        fun maxMove i acc = if i >= k then acc
          else let val mv = realAbs (Array.sub (newCentroids, i) - Array.sub (centroids, i))
               in maxMove (i+1) (if mv > acc then mv else acc) end
        val movement = maxMove 0 0.0
    in if iter >= MAX_ITER orelse movement < CONVERGE_THRESH then iter else kmeansSeqIter (pix, newCentroids, iter + 1) end

  fun kmeansSeq (pix: int array, k: int) : int = kmeansSeqIter (pix, initCentroids k, 1)

  (* Parallel *)
  fun kmeansParIter (pix: int array, centroids: real array, numWorkers: int, iter: int) : int =
    let val n = Array.length pix val k = Array.length centroids
        val chunkSize = (n + numWorkers - 1) div numWorkers
        val workerSums = Array.tabulate (numWorkers, fn _ => Array.array (k, 0.0))
        val workerCounts = Array.tabulate (numWorkers, fn _ => Array.array (k, 0))
        val _ = Parallel.parfor (0, numWorkers) (fn w =>
          let val lo = w * chunkSize val hi = Int.min (n, lo + chunkSize)
              val mySums = Array.sub (workerSums, w) val myCounts = Array.sub (workerCounts, w)
              fun processPixel i = if i >= hi then ()
                else let val pixVal = Array.sub (pix, i) val lbl = nearestCentroid (centroids, k, pixVal)
                     in Array.update (mySums, lbl, Array.sub (mySums, lbl) + toReal pixVal);
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
    in if iter >= MAX_ITER orelse movement < CONVERGE_THRESH then iter else kmeansParIter (pix, newCentroids, numWorkers, iter + 1) end

  fun kmeansPar (pix: int array, k: int, numWorkers: int) : int = kmeansParIter (pix, initCentroids k, numWorkers, 1)

  (* Timing *)
  fun timeIt (f: unit -> 'a) : 'a * real =
    let val start = Time.now () val result = f () val stop = Time.now ()
    in (result, Time.toReal (Time.- (stop, start))) end

  (* Benchmark *)
  fun benchSingle (name: string, pix: int array, k: int, numWorkers: int) =
    let val _ = kmeansSeq (pix, k) val _ = kmeansPar (pix, k, numWorkers)
        fun runSeq i acc = if i >= NUM_RUNS then acc
          else let val (_, t) = timeIt (fn () => kmeansSeq (pix, k)) in runSeq (i+1) (t :: acc) end
        val seqTimes = runSeq 0 []
        val seqMin = List.foldl Real.min 999.0 seqTimes
        val seqAvg = (List.foldl (op +) 0.0 seqTimes) / toReal NUM_RUNS
        fun runPar i acc = if i >= NUM_RUNS then acc
          else let val (_, t) = timeIt (fn () => kmeansPar (pix, k, numWorkers)) in runPar (i+1) (t :: acc) end
        val parTimes = runPar 0 []
        val parMin = List.foldl Real.min 999.0 parTimes
        val parAvg = (List.foldl (op +) 0.0 parTimes) / toReal NUM_RUNS
        val speedup = seqMin / parMin
    in print ("  " ^ name ^ " (K=" ^ Int.toString k ^ ")\n");
       print ("    Seq:     " ^ Real.fmt (StringCvt.FIX (SOME 4)) seqMin ^ "s (min), " ^ Real.fmt (StringCvt.FIX (SOME 4)) seqAvg ^ "s (avg)\n");
       print ("    Par:     " ^ Real.fmt (StringCvt.FIX (SOME 4)) parMin ^ "s (min), " ^ Real.fmt (StringCvt.FIX (SOME 4)) parAvg ^ "s (avg)\n");
       print ("    Speedup: " ^ Real.fmt (StringCvt.FIX (SOME 2)) speedup ^ "x\n");
       (seqMin, parMin, speedup) end

  fun benchWorkerScaling (pix: int array, k: int) =
    let val _ = print "\n======================================\n"
        val _ = print "  WORKER SCALING (K-Means)\n"
        val _ = print "======================================\n"
        val _ = print ("  K = " ^ Int.toString k ^ "\n")
        val _ = print "Workers | Seq (s)  | Par (s)  | Speedup\n"
        val _ = print "--------+----------+----------+--------\n"
        val workerCounts = [1, 2, 4, 8, 16, 32, 64, 128, 256]
        fun test [] = ()
          | test (w :: ws) =
              let val _ = kmeansSeq (pix, k) val _ = kmeansPar (pix, k, w)
                  fun runSeq i acc = if i >= NUM_RUNS then acc
                    else let val (_, t) = timeIt (fn () => kmeansSeq (pix, k)) in runSeq (i+1) (t :: acc) end
                  val seqMin = List.foldl Real.min 999.0 (runSeq 0 [])
                  fun runPar i acc = if i >= NUM_RUNS then acc
                    else let val (_, t) = timeIt (fn () => kmeansPar (pix, k, w)) in runPar (i+1) (t :: acc) end
                  val parMin = List.foldl Real.min 999.0 (runPar 0 [])
                  val spd = seqMin / parMin
                  val wStr = Int.toString w
                  val pad = if w < 10 then "   " else if w < 100 then "  " else " "
              in print (wStr ^ pad ^ "    | " ^ Real.fmt (StringCvt.FIX (SOME 4)) seqMin ^ "   | " ^
                        Real.fmt (StringCvt.FIX (SOME 4)) parMin ^ "   | " ^ Real.fmt (StringCvt.FIX (SOME 2)) spd ^ "x\n");
                 test ws end
    in test workerCounts; print "======================================\n" end

  fun benchKScaling (pix: int array, numWorkers: int) =
    let val _ = print "\n======================================\n"
        val _ = print "  K VALUE SCALING\n"
        val _ = print "======================================\n"
        val _ = print ("  Workers = " ^ Int.toString numWorkers ^ "\n")
        val _ = print "K   | Seq (s)  | Par (s)  | Speedup\n"
        val _ = print "----+----------+----------+--------\n"
        val kValues = [2, 4, 8, 16]
        fun test [] = ()
          | test (k :: ks) =
              let val _ = kmeansSeq (pix, k) val _ = kmeansPar (pix, k, numWorkers)
                  fun runSeq i acc = if i >= NUM_RUNS then acc
                    else let val (_, t) = timeIt (fn () => kmeansSeq (pix, k)) in runSeq (i+1) (t :: acc) end
                  val seqMin = List.foldl Real.min 999.0 (runSeq 0 [])
                  fun runPar i acc = if i >= NUM_RUNS then acc
                    else let val (_, t) = timeIt (fn () => kmeansPar (pix, k, numWorkers)) in runPar (i+1) (t :: acc) end
                  val parMin = List.foldl Real.min 999.0 (runPar 0 [])
                  val spd = seqMin / parMin
                  val kStr = Int.toString k
                  val pad = if k < 10 then " " else ""
              in print (kStr ^ pad ^ "  | " ^ Real.fmt (StringCvt.FIX (SOME 4)) seqMin ^ "   | " ^
                        Real.fmt (StringCvt.FIX (SOME 4)) parMin ^ "   | " ^ Real.fmt (StringCvt.FIX (SOME 2)) spd ^ "x\n");
                 test ks end
    in test kValues; print "======================================\n" end

  (* Main *)
  fun main () =
    let val numWorkers = 64 val defaultK = 4
        val _ = print "============================================================\n"
        val _ = print "  K-MEANS IMAGE SEGMENTATION BENCHMARK\n"
        val _ = print "============================================================\n"
        val _ = print ("  Workers: " ^ Int.toString numWorkers ^ ", Runs: " ^ Int.toString NUM_RUNS ^ "\n")
        val _ = print "============================================================\n\n"
        val testFiles = [("test_images/test_bimodal.txt", "256x256"), ("test_images/test_1024x1024.txt", "1024x1024"),
                         ("test_images/test_2048x2048.txt", "2048x2048"), ("test_images/test_4096x4096.txt", "4096x4096"),
                         ("test_images/test_8192x8192.txt", "8192x8192")]
        fun runAll [] = ()
          | runAll ((f, sz) :: fs) =
              (let val _ = print ("\n" ^ f ^ "\n")
                   val (pix, h, w) = readImage f val n = h * w
                   val _ = print ("  Size: " ^ sz ^ " (" ^ Int.toString n ^ " pixels)\n")
                   val _ = benchSingle (sz, pix, defaultK, numWorkers)
               in () end handle _ => print ("  [Skipped: " ^ f ^ "]\n"); runAll fs)
        val _ = runAll testFiles
        val _ = (let val (pix, _, _) = readImage "test_images/test_4096x4096.txt"
                 in benchWorkerScaling (pix, defaultK) end handle _ => print "[Scaling skipped]\n")
        val _ = (let val (pix, _, _) = readImage "test_images/test_4096x4096.txt"
                 in benchKScaling (pix, 64) end handle _ => print "[K scaling skipped]\n")
        val _ = print "\n============================================================\n"
        val _ = print "  BENCHMARK COMPLETE\n"
        val _ = print "============================================================\n"
    in () end

  val _ = main ()

end
