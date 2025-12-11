structure BenchmarkMax =
struct

  val NUM_BINS = 256

  (* Utils *)
  fun tokens s = String.tokens Char.isSpace s
  fun toInt s = case Int.fromString s of SOME n => n | NONE => raise Fail "bad int"
  fun toReal n = Real.fromInt n
  fun zeroHist () : int array = Array.array (NUM_BINS, 0)

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

  fun otsuSeq (hist: int array, total: int) : int =
    let val n = toReal total
        val cumW = Array.array (NUM_BINS, 0.0) val cumM = Array.array (NUM_BINS, 0.0)
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

  fun applySeq (pix: int array, t: int) : int array =
    let val n = Array.length pix val out = Array.array (n, 0)
        fun lp i = if i >= n then () else (Array.update (out, i, if Array.sub (pix, i) >= t then 255 else 0); lp (i+1))
    in lp 0; out end

  fun fullSeq (pix: int array) : int =
    let val hist = histSeq pix val t = otsuSeq (hist, Array.length pix) val _ = applySeq (pix, t) in t end

  (* Parallel-Opt *)
  fun histParOpt (pix: int array, numWorkers: int) : int array =
    let val n = Array.length pix val chunkSize = (n + numWorkers - 1) div numWorkers
        val localHists : int array array = Array.tabulate (numWorkers, fn _ => zeroHist ())
        val _ = Parallel.parfor (0, numWorkers) (fn w =>
          let val lo = w * chunkSize val hi = Int.min (n, lo + chunkSize) val myHist = Array.sub (localHists, w)
              fun lp i = if i >= hi then () else let val v = Array.sub (pix, i)
                  in Array.update (myHist, v, Array.sub (myHist, v) + 1); lp (i+1) end
          in lp lo end)
        val final = zeroHist ()
        fun mergeBin bin = let fun sumWorkers w acc = if w >= numWorkers then acc
              else sumWorkers (w+1) (acc + Array.sub (Array.sub (localHists, w), bin))
            in Array.update (final, bin, sumWorkers 0 0) end
        fun mergeAll bin = if bin >= NUM_BINS then () else (mergeBin bin; mergeAll (bin+1))
        val _ = mergeAll 0
    in final end

  val otsuParOpt = otsuSeq

  fun applyParOpt (pix: int array, t: int) : int array =
    let val n = Array.length pix
        val result = Parallel.tabulate (0, n) (fn i => if Array.sub (pix, i) >= t then 255 else 0)
    in Array.tabulate (n, fn i => Seq.nth result i) end

  fun fullParOpt (pix: int array, numWorkers: int) : int =
    let val hist = histParOpt (pix, numWorkers) val t = otsuParOpt (hist, Array.length pix)
        val _ = applyParOpt (pix, t) in t end

  (* Ultra-Parallel *)
  fun fullParUltra (pix: int array, numWorkers: int) : int =
    let val n = Array.length pix val chunkSize = (n + numWorkers - 1) div numWorkers
        val localHists = Array.tabulate (numWorkers, fn _ => zeroHist ())
        val _ = Parallel.parfor (0, numWorkers) (fn w =>
          let val lo = w * chunkSize val hi = Int.min (n, lo + chunkSize) val myHist = Array.sub (localHists, w)
              fun lp i = if i >= hi then () else let val v = Array.sub (pix, i)
                  in Array.update (myHist, v, Array.sub (myHist, v) + 1); lp (i+1) end
          in lp lo end)
        val final = zeroHist ()
        val _ = Parallel.parfor (0, NUM_BINS) (fn bin =>
          let fun sum w acc = if w >= numWorkers then acc
                else sum (w+1) (acc + Array.sub (Array.sub (localHists, w), bin))
          in Array.update (final, bin, sum 0 0) end)
        val t = otsuSeq (final, n)
        val _ = Parallel.tabulate (0, n) (fn i => if Array.sub (pix, i) >= t then 255 else 0)
    in t end

  (* Timing *)
  fun timeIt (f: unit -> 'a) : 'a * real =
    let val start = Time.now () val result = f () val stop = Time.now ()
    in (result, Time.toReal (Time.- (stop, start))) end

  (* Benchmark *)
  fun bench (name: string, pix: int array, numRuns: int, f: unit -> int) =
    let val _ = f () val _ = f ()
        fun run i acc = if i >= numRuns then acc
          else let val (_, t) = timeIt f in run (i+1) (t :: acc) end
        val times = run 0 []
        val avg = (List.foldl (op +) 0.0 times) / Real.fromInt numRuns
        val min = List.foldl Real.min 999.0 times
    in print ("  " ^ name ^ ": avg=" ^ Real.fmt (StringCvt.FIX (SOME 4)) avg ^ "s, min=" ^
              Real.fmt (StringCvt.FIX (SOME 4)) min ^ "s\n"); (avg, min) end

  fun runFullBenchmark (filename: string, numRuns: int, numWorkers: int) =
    let val _ = print ("\n" ^ filename ^ "\n")
        val (pix, h, w) = readImage filename val n = h * w
        val _ = print ("  Size: " ^ Int.toString w ^ "x" ^ Int.toString h ^ " (" ^ Int.toString n ^ " pixels)\n")
        val _ = print ("  Workers: " ^ Int.toString numWorkers ^ "\n")
        val (_, seqMin) = bench ("Sequential    ", pix, numRuns, fn () => fullSeq pix)
        val (_, parMin) = bench ("Par-Opt       ", pix, numRuns, fn () => fullParOpt (pix, numWorkers))
        val (_, ultMin) = bench ("Par-Ultra     ", pix, numRuns, fn () => fullParUltra (pix, numWorkers))
        val spdOpt = seqMin / parMin val spdUlt = seqMin / ultMin
    in print ("  Speedup (Opt):   " ^ Real.fmt (StringCvt.FIX (SOME 2)) spdOpt ^ "x\n");
       print ("  Speedup (Ultra): " ^ Real.fmt (StringCvt.FIX (SOME 2)) spdUlt ^ "x\n");
       (seqMin, parMin, ultMin) end

  fun runWorkerScaling (filename: string, numRuns: int) =
    let val _ = print "\n======================================\n"
        val _ = print "  WORKER COUNT SCALING\n"
        val _ = print "======================================\n"
        val (pix, h, w) = readImage filename
        val _ = print ("  Image: " ^ Int.toString w ^ "x" ^ Int.toString h ^ "\n")
        val _ = print "Workers | Seq (s)  | Par (s)  | Speedup\n"
        val _ = print "--------+----------+----------+--------\n"
        val workerCounts = [1, 2, 4, 8, 16, 32, 64]
        fun test [] = ()
          | test (w :: ws) =
              let val _ = fullSeq pix val _ = fullParUltra (pix, w)
                  fun runSeq i acc = if i >= numRuns then acc
                    else let val (_, t) = timeIt (fn () => fullSeq pix) in runSeq (i+1) (t :: acc) end
                  val seqMin = List.foldl Real.min 999.0 (runSeq 0 [])
                  fun runPar i acc = if i >= numRuns then acc
                    else let val (_, t) = timeIt (fn () => fullParUltra (pix, w)) in runPar (i+1) (t :: acc) end
                  val parMin = List.foldl Real.min 999.0 (runPar 0 [])
                  val spd = seqMin / parMin
                  val wStr = Int.toString w
                  val pad = if w < 10 then "   " else if w < 100 then "  " else " "
              in print (wStr ^ pad ^ "    | " ^ Real.fmt (StringCvt.FIX (SOME 4)) seqMin ^ "   | " ^
                        Real.fmt (StringCvt.FIX (SOME 4)) parMin ^ "   | " ^ Real.fmt (StringCvt.FIX (SOME 2)) spd ^ "x\n");
                 test ws end
    in test workerCounts; print "======================================\n" end

  (* Main *)
  fun main () =
    let val numRuns = 5 val numWorkers = 16
        val _ = print "============================================================\n"
        val _ = print "  MAXIMUM OPTIMIZATION Otsu Benchmark\n"
        val _ = print "============================================================\n"
        val testFiles = ["test_images/test_bimodal.txt", "test_images/test_1024x1024.txt",
                         "test_images/test_2048x2048.txt", "test_images/test_4096x4096.txt"]
        fun runAll [] = ()
          | runAll (f :: fs) = (runFullBenchmark (f, numRuns, numWorkers) handle _ => (print ("  [Skipped]\n"); (0.0,0.0,0.0)); runAll fs)
        val _ = runAll testFiles
        val _ = runWorkerScaling ("test_images/test_4096x4096.txt", 5)
        val _ = print "\n============================================================\n"
        val _ = print "  BENCHMARK COMPLETE\n"
        val _ = print "============================================================\n"
    in () end

  val _ = main ()

end
