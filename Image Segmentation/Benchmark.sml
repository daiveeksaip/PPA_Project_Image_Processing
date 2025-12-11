structure ThresholdBenchmark =
struct

  val NUM_BINS = 256
  val GRAIN = 10000

  (* Utils *)
  fun tokens s = String.tokens Char.isSpace s
  fun toInt s = case Int.fromString s of SOME n => n | NONE => raise Fail "bad int"
  fun toReal n = Real.fromInt n
  fun zeroHist () = Array.array (NUM_BINS, 0)
  fun histMerge (a: int array, b: int array) : int array =
    Array.tabulate (NUM_BINS, fn i => Array.sub (a, i) + Array.sub (b, i))

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
  fun histSeq (pix: int array) : int array =
    let val n = Array.length pix val h = zeroHist ()
        fun lp i = if i >= n then () else let val v = Array.sub (pix, i)
            in Array.update (h, v, Array.sub (h, v) + 1); lp (i+1) end
    in lp 0; h end

  (* Parallel *)
  fun histSeqRange (pix: int array) (lo: int, hi: int) : int array =
    let val h = zeroHist ()
        fun lp i = if i >= hi then () else let val v = Array.sub (pix, i)
            in Array.update (h, v, Array.sub (h, v) + 1); lp (i+1) end
    in lp lo; h end

  fun histPar (pix: int array) : int array =
    let val n = Array.length pix val chunks = if n = 0 then 0 else (n + GRAIN - 1) div GRAIN
        fun make k = let val lo = k * GRAIN val hi = Int.min (n, lo + GRAIN) in histSeqRange pix (lo, hi) end
    in if n = 0 then zeroHist () else Parallel.reduce histMerge (zeroHist ()) (0, chunks) make end

  (* Otsu *)
  fun otsuFromHist (hist: int array, total: int) : int =
    let val n = toReal total
        val cumW = Array.array (NUM_BINS, 0.0) val cumM = Array.array (NUM_BINS, 0.0)
        fun build i = if i >= NUM_BINS then ()
          else let val p = toReal (Array.sub (hist, i)) / n
                   val pw = if i = 0 then 0.0 else Array.sub (cumW, i-1)
                   val pm = if i = 0 then 0.0 else Array.sub (cumM, i-1)
               in Array.update (cumW, i, pw + p); Array.update (cumM, i, pm + Real.fromInt i * p); build (i+1) end
        val _ = build 0 val mu = Array.sub (cumM, NUM_BINS - 1)
        fun findBestSeq t (bestT, bestV) = if t >= NUM_BINS - 1 then bestT
          else let val w0 = Array.sub (cumW, t) val w1 = 1.0 - w0
               in if w0 < 1E~10 orelse w1 < 1E~10 then findBestSeq (t+1) (bestT, bestV)
                  else let val m0 = Array.sub (cumM, t) / w0 val m1 = (mu - Array.sub (cumM, t)) / w1
                           val d = m0 - m1 val v = w0 * w1 * d * d
                       in if v > bestV then findBestSeq (t+1) (t, v) else findBestSeq (t+1) (bestT, bestV) end end
    in findBestSeq 0 (0, ~1.0) end

  fun otsuFromHistPar (hist: int array, total: int) : int =
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

  (* Apply *)
  fun applySeq (pix: int array, t: int) : int array =
    Array.tabulate (Array.length pix, fn i => if Array.sub (pix, i) >= t then 255 else 0)
  fun applyPar (pix: int array, t: int) : int Seq.t =
    Parallel.tabulate (0, Array.length pix) (fn i => if Array.sub (pix, i) >= t then 255 else 0)

  (* Pipelines *)
  fun otsuSequential (pix: int array) =
    let val hist = histSeq pix val t = otsuFromHist (hist, Array.length pix) val _ = applySeq (pix, t) in t end
  fun otsuParallel (pix: int array) =
    let val hist = histPar pix val t = otsuFromHistPar (hist, Array.length pix) val _ = applyPar (pix, t) in t end

  (* Timing *)
  fun timeIt (name: string) (f: unit -> 'a) : 'a * real =
    let val start = Time.now () val result = f () val stop = Time.now ()
    in (result, Time.toReal (Time.- (stop, start))) end

  (* Benchmark *)
  fun runBenchmark (filename: string, numRuns: int) =
    let val _ = print ("Loading: " ^ filename ^ "\n")
        val (pix, h, w) = readImage filename val n = h * w
        val _ = print ("  Size: " ^ Int.toString w ^ "x" ^ Int.toString h ^ " (" ^ Int.toString n ^ " pixels)\n")
        val _ = otsuSequential pix val _ = otsuParallel pix
        fun runSeq i acc = if i >= numRuns then acc
          else let val (_, t) = timeIt "seq" (fn () => otsuSequential pix) in runSeq (i+1) (t :: acc) end
        val seqTimes = runSeq 0 [] val seqAvg = (List.foldl (op +) 0.0 seqTimes) / Real.fromInt numRuns
        fun runPar i acc = if i >= numRuns then acc
          else let val (_, t) = timeIt "par" (fn () => otsuParallel pix) in runPar (i+1) (t :: acc) end
        val parTimes = runPar 0 [] val parAvg = (List.foldl (op +) 0.0 parTimes) / Real.fromInt numRuns
        val speedup = seqAvg / parAvg
    in print ("  Sequential avg: " ^ Real.fmt (StringCvt.FIX (SOME 4)) seqAvg ^ " s\n");
       print ("  Parallel avg:   " ^ Real.fmt (StringCvt.FIX (SOME 4)) parAvg ^ " s\n");
       print ("  Speedup:        " ^ Real.fmt (StringCvt.FIX (SOME 2)) speedup ^ "x\n\n");
       (seqAvg, parAvg, speedup) end

  (* Main *)
  fun main () =
    let val numRuns = 3
        val _ = print "============================================================\n"
        val _ = print "  Otsu's Thresholding Benchmark: Sequential vs Parallel\n"
        val _ = print "============================================================\n\n"
        val testFiles = ["test_images/test_bimodal.txt", "test_images/test_1024x1024.txt",
                         "test_images/test_2048x2048.txt", "test_images/test_4096x4096.txt"]
        val _ = print ("Running " ^ Int.toString numRuns ^ " iterations per test...\n\n")
        fun runAll [] results = List.rev results
          | runAll (f :: fs) results =
              let val r = runBenchmark (f, numRuns) handle _ => (print "  [Skipped]\n\n"; (0.0, 0.0, 0.0))
              in runAll fs (r :: results) end
        val results = runAll testFiles []
        val _ = print "============================================================\n"
        val _ = print "  BENCHMARK COMPLETE\n"
        val _ = print "============================================================\n"
    in () end

  val _ = main ()

end
