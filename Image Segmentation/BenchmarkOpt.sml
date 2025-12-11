structure ThresholdBenchmarkOpt =
struct

  val NUM_BINS = 256

  (* Utils *)
  fun tokens s = String.tokens Char.isSpace s
  fun toInt s = case Int.fromString s of SOME n => n | NONE => raise Fail "bad int"
  fun toReal n = Real.fromInt n
  fun zeroHist () = Array.array (NUM_BINS, 0)

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
  fun histSeqFull (pix: int array) : int array =
    let val n = Array.length pix val h = zeroHist ()
        fun lp i = if i >= n then () else let val v = Array.sub (pix, i)
            in Array.update (h, v, Array.sub (h, v) + 1); lp (i+1) end
    in lp 0; h end

  fun otsuSeq (hist: int array, total: int) : int =
    let val n = toReal total
        val cumW = Array.array (NUM_BINS, 0.0) val cumM = Array.array (NUM_BINS, 0.0)
        fun build i = if i >= NUM_BINS then ()
          else let val p = toReal (Array.sub (hist, i)) / n
                   val pw = if i = 0 then 0.0 else Array.sub (cumW, i-1)
                   val pm = if i = 0 then 0.0 else Array.sub (cumM, i-1)
               in Array.update (cumW, i, pw + p); Array.update (cumM, i, pm + Real.fromInt i * p); build (i+1) end
        val _ = build 0 val mu = Array.sub (cumM, NUM_BINS - 1)
        fun findBest t (bestT, bestV) = if t >= NUM_BINS - 1 then bestT
          else let val w0 = Array.sub (cumW, t) val w1 = 1.0 - w0
               in if w0 < 1E~10 orelse w1 < 1E~10 then findBest (t+1) (bestT, bestV)
                  else let val m0 = Array.sub (cumW, t) / w0 val m1 = (mu - Array.sub (cumM, t)) / w1
                           val d = m0 - m1 val v = w0 * w1 * d * d
                       in if v > bestV then findBest (t+1) (t, v) else findBest (t+1) (bestT, bestV) end end
    in findBest 0 (0, ~1.0) end

  fun applySeq (pix: int array, t: int) : int array =
    Array.tabulate (Array.length pix, fn i => if Array.sub (pix, i) >= t then 255 else 0)

  fun fullSeq (pix: int array) = let val hist = histSeqFull pix val t = otsuSeq (hist, Array.length pix)
      val _ = applySeq (pix, t) in t end

  (* Parallel-Opt *)
  fun histParOpt (pix: int array, numChunks: int) : int array =
    let val n = Array.length pix val chunkSize = (n + numChunks - 1) div numChunks
        fun buildChunk k = let val lo = k * chunkSize val hi = Int.min (n, lo + chunkSize) val h = zeroHist ()
            fun lp i = if i >= hi then () else let val v = Array.sub (pix, i)
                in Array.update (h, v, Array.sub (h, v) + 1); lp (i+1) end in lp lo; h end
        val localHists = Parallel.tabulate (0, numChunks) buildChunk
        val finalHist = zeroHist ()
        val _ = Parallel.parfor (0, NUM_BINS) (fn bin =>
          let fun sumBin k acc = if k >= numChunks then acc
                else sumBin (k+1) (acc + Array.sub (Seq.nth localHists k, bin))
          in Array.update (finalHist, bin, sumBin 0 0) end)
    in finalHist end

  fun otsuParOpt (hist: int array, total: int) : int =
    let val n = toReal total
        val cumW = Array.array (NUM_BINS, 0.0) val cumM = Array.array (NUM_BINS, 0.0)
        fun build i = if i >= NUM_BINS then ()
          else let val p = toReal (Array.sub (hist, i)) / n
                   val pw = if i = 0 then 0.0 else Array.sub (cumW, i-1)
                   val pm = if i = 0 then 0.0 else Array.sub (cumM, i-1)
               in Array.update (cumW, i, pw + p); Array.update (cumM, i, pm + Real.fromInt i * p); build (i+1) end
        val _ = build 0 val mu = Array.sub (cumM, NUM_BINS - 1)
        fun variance t = let val w0 = Array.sub (cumW, t) val w1 = 1.0 - w0
            in if w0 < 1E~10 orelse w1 < 1E~10 then (t, ~1.0)
               else let val m0 = Array.sub (cumM, t) / w0 val m1 = (mu - Array.sub (cumM, t)) / w1 val d = m0 - m1
                    in (t, w0 * w1 * d * d) end end
        fun better ((t1, v1), (t2, v2)) = if v1 >= v2 then (t1, v1) else (t2, v2)
        val (best, _) = Parallel.reduce better (~1, ~1.0) (0, NUM_BINS - 1) variance
    in best end

  fun applyParOpt (pix: int array, t: int) : int Seq.t =
    Parallel.tabulate (0, Array.length pix) (fn i => if Array.sub (pix, i) >= t then 255 else 0)

  fun fullParOpt (pix: int array, numChunks: int) =
    let val hist = histParOpt (pix, numChunks) val t = otsuParOpt (hist, Array.length pix)
        val _ = applyParOpt (pix, t) in t end

  (* Timing *)
  fun timeIt (f: unit -> 'a) : 'a * real =
    let val start = Time.now () val result = f () val stop = Time.now ()
    in (result, Time.toReal (Time.- (stop, start))) end

  (* Benchmark *)
  fun runBenchmark (filename: string, numRuns: int, numChunks: int) =
    let val _ = print ("Loading: " ^ filename ^ "\n")
        val (pix, h, w) = readImage filename val n = h * w
        val _ = print ("  Size: " ^ Int.toString w ^ "x" ^ Int.toString h ^ " (" ^ Int.toString n ^ " pixels)\n")
        val _ = print ("  Chunks: " ^ Int.toString numChunks ^ "\n")
        val _ = fullSeq pix val _ = fullParOpt (pix, numChunks)
        fun runSeq i acc = if i >= numRuns then acc
          else let val (_, t) = timeIt (fn () => fullSeq pix) in runSeq (i+1) (t :: acc) end
        val seqTimes = runSeq 0 [] val seqAvg = (List.foldl (op +) 0.0 seqTimes) / Real.fromInt numRuns
        fun runPar i acc = if i >= numRuns then acc
          else let val (_, t) = timeIt (fn () => fullParOpt (pix, numChunks)) in runPar (i+1) (t :: acc) end
        val parTimes = runPar 0 [] val parAvg = (List.foldl (op +) 0.0 parTimes) / Real.fromInt numRuns
        val speedup = seqAvg / parAvg
    in print ("  Sequential avg: " ^ Real.fmt (StringCvt.FIX (SOME 4)) seqAvg ^ " s\n");
       print ("  Parallel avg:   " ^ Real.fmt (StringCvt.FIX (SOME 4)) parAvg ^ " s\n");
       print ("  Speedup:        " ^ Real.fmt (StringCvt.FIX (SOME 2)) speedup ^ "x\n\n");
       (seqAvg, parAvg, speedup) end

  fun runScalingExperiment (filename: string, numRuns: int) =
    let val _ = print "\n========================================\n"
        val _ = print "  CHUNK SCALING EXPERIMENT\n"
        val _ = print "========================================\n"
        val _ = print ("  File: " ^ filename ^ "\n")
        val _ = print "Chunks  | Seq (s)  | Par (s)  | Speedup\n"
        val _ = print "--------+----------+----------+--------\n"
        val chunkCounts = [4, 8, 16, 32, 64, 128, 256, 512]
        fun testChunks [] = ()
          | testChunks (c :: cs) =
              let val (pix, _, _) = readImage filename
                  val _ = fullSeq pix val _ = fullParOpt (pix, c)
                  fun runSeq i acc = if i >= numRuns then acc
                    else let val (_, t) = timeIt (fn () => fullSeq pix) in runSeq (i+1) (t :: acc) end
                  val seqAvg = (List.foldl (op +) 0.0 (runSeq 0 [])) / Real.fromInt numRuns
                  fun runPar i acc = if i >= numRuns then acc
                    else let val (_, t) = timeIt (fn () => fullParOpt (pix, c)) in runPar (i+1) (t :: acc) end
                  val parAvg = (List.foldl (op +) 0.0 (runPar 0 [])) / Real.fromInt numRuns
                  val spd = seqAvg / parAvg
                  val cStr = Int.toString c
                  val pad = if c < 10 then "   " else if c < 100 then "  " else " "
              in print (cStr ^ pad ^ "    | " ^ Real.fmt (StringCvt.FIX (SOME 4)) seqAvg ^ "   | " ^
                        Real.fmt (StringCvt.FIX (SOME 4)) parAvg ^ "   | " ^ Real.fmt (StringCvt.FIX (SOME 2)) spd ^ "x\n");
                 testChunks cs end
    in testChunks chunkCounts; print "========================================\n" end

  (* Main *)
  fun main () =
    let val numRuns = 5 val numChunks = 64
        val _ = print "============================================================\n"
        val _ = print "  OPTIMIZED Otsu's Thresholding Benchmark\n"
        val _ = print "============================================================\n\n"
        val testFiles = [("test_images/test_bimodal.txt", "256x256"), ("test_images/test_1024x1024.txt", "1024x1024"),
                         ("test_images/test_2048x2048.txt", "2048x2048"), ("test_images/test_4096x4096.txt", "4096x4096")]
        fun runAll [] results = List.rev results
          | runAll ((f, _) :: fs) results =
              let val r = runBenchmark (f, numRuns, numChunks) handle _ => (print "  [Skipped]\n\n"; (0.0, 0.0, 0.0))
              in runAll fs (r :: results) end
        val _ = runAll testFiles []
        val _ = runScalingExperiment ("test_images/test_4096x4096.txt", 3)
        val _ = print "\n============================================================\n"
        val _ = print "  BENCHMARK COMPLETE\n"
        val _ = print "============================================================\n"
    in () end

  val _ = main ()

end
