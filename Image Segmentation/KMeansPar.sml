structure KMeansPar =
struct

  val MAX_ITER = 50
  val CONVERGE_THRESH = 0.5

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

  fun writeImage (file: string, pix: int array, h: int, w: int) =
    let val outs = TextIO.openOut file
        val _ = TextIO.output (outs, Int.toString h ^ " " ^ Int.toString w ^ " 1\n")
        val n = h * w
        fun wr i = if i >= n then ()
          else (TextIO.output (outs, Int.toString (Array.sub (pix, i)));
                TextIO.output (outs, if (i + 1) mod w = 0 then "\n" else " "); wr (i + 1))
        val _ = wr 0 val _ = TextIO.closeOut outs
    in () end

  (* KMeans-Par *)
  fun initCentroids (k: int) : real array = Array.tabulate (k, fn i => toReal i * 255.0 / toReal (k - 1))

  fun nearestCentroid (centroids: real array, k: int, pixVal: int) : int =
    let val pv = toReal pixVal
        fun find i bestIdx bestDist = if i >= k then bestIdx
          else let val d = realAbs (pv - Array.sub (centroids, i))
               in if d < bestDist then find (i+1) i d else find (i+1) bestIdx bestDist end
    in find 0 0 1000.0 end

  fun assignAndAccumulate (pix: int array, centroids: real array, numWorkers: int) : int array * real array * int array =
    let val n = Array.length pix val k = Array.length centroids
        val chunkSize = (n + numWorkers - 1) div numWorkers
        val labels = Array.array (n, 0)
        val workerSums = Array.tabulate (numWorkers, fn _ => Array.array (k, 0.0))
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
          let fun sumWorkers w sumAcc countAcc = if w >= numWorkers then (sumAcc, countAcc)
                else sumWorkers (w+1) (sumAcc + Array.sub (Array.sub (workerSums, w), c)) (countAcc + Array.sub (Array.sub (workerCounts, w), c))
              val (s, cnt) = sumWorkers 0 0.0 0
          in Array.update (finalSums, c, s); Array.update (finalCounts, c, cnt) end)
    in (labels, finalSums, finalCounts) end

  fun computeNewCentroids (sums: real array, counts: int array, oldCentroids: real array) : real array * real =
    let val k = Array.length oldCentroids
        val newCentroids = Array.tabulate (k, fn i => let val c = Array.sub (counts, i)
            in if c = 0 then Array.sub (oldCentroids, i) else Array.sub (sums, i) / toReal c end)
        fun maxMove i acc = if i >= k then acc
          else let val mv = realAbs (Array.sub (newCentroids, i) - Array.sub (oldCentroids, i))
               in maxMove (i+1) (if mv > acc then mv else acc) end
    in (newCentroids, maxMove 0 0.0) end

  fun kmeansLoopPar (pix: int array, centroids: real array, numWorkers: int, iter: int) : int array * int =
    let val (labels, sums, counts) = assignAndAccumulate (pix, centroids, numWorkers)
        val (newCentroids, movement) = computeNewCentroids (sums, counts, centroids)
    in if iter >= MAX_ITER orelse movement < CONVERGE_THRESH then (labels, iter)
       else kmeansLoopPar (pix, newCentroids, numWorkers, iter + 1) end

  fun kmeansPar (pix: int array, k: int, numWorkers: int) : int array * int =
    let val centroids = initCentroids k in kmeansLoopPar (pix, centroids, numWorkers, 1) end

  fun labelsToGrayPar (labels: int array, k: int) : int array =
    let val n = Array.length labels val scale = if k <= 1 then 255 else 255 div (k - 1)
        val result = Parallel.tabulate (0, n) (fn i => Int.min (255, Array.sub (labels, i) * scale))
    in Array.tabulate (n, fn i => Seq.nth result i) end

  fun segmentPar (pix: int array, k: int, numWorkers: int) : int array * int =
    let val (labels, iters) = kmeansPar (pix, k, numWorkers) val output = labelsToGrayPar (labels, k)
    in (output, iters) end

  (* Main *)
  fun main () =
    let val args = CommandLine.arguments ()
        val (inFile, outFile, k) = case args of
            [i, o, kStr] => (i, o, toInt kStr) | [i, o] => (i, o, 2)
          | _ => (print "Usage: kmeans_par <input.txt> <output.txt> [k]\n"; OS.Process.exit OS.Process.failure; ("", "", 0))
        val numWorkers = 16
        val _ = print ("K-Means Segmentation (Parallel)\n")
        val _ = print ("  Input:   " ^ inFile ^ "\n")
        val _ = print ("  Output:  " ^ outFile ^ "\n")
        val _ = print ("  K:       " ^ Int.toString k ^ "\n")
        val _ = print ("  Workers: " ^ Int.toString numWorkers ^ "\n")
        val (pix, h, w) = readImage inFile val n = h * w
        val _ = print ("  Size:    " ^ Int.toString w ^ "x" ^ Int.toString h ^ " (" ^ Int.toString n ^ " pixels)\n")
        val start = Time.now ()
        val (output, iters) = segmentPar (pix, k, numWorkers)
        val elapsed = Time.toReal (Time.- (Time.now (), start))
        val _ = print ("  Iterations: " ^ Int.toString iters ^ "\n")
        val _ = print ("  Time:    " ^ Real.fmt (StringCvt.FIX (SOME 4)) elapsed ^ " s\n")
        val _ = writeImage (outFile, output, h, w)
        val _ = print ("  Done.\n")
    in () end

  val _ = main ()

end
