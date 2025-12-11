structure KMeansSeq =
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

  (* KMeans *)
  fun initCentroids (k: int) : real array = Array.tabulate (k, fn i => toReal i * 255.0 / toReal (k - 1))

  fun nearestCentroid (centroids: real array, pixVal: int) : int =
    let val k = Array.length centroids val pv = toReal pixVal
        fun find i bestIdx bestDist = if i >= k then bestIdx
          else let val d = realAbs (pv - Array.sub (centroids, i))
               in if d < bestDist then find (i+1) i d else find (i+1) bestIdx bestDist end
    in find 0 0 1000.0 end

  fun assignPixels (pix: int array, centroids: real array) : int array =
    let val n = Array.length pix
    in Array.tabulate (n, fn i => nearestCentroid (centroids, Array.sub (pix, i))) end

  fun updateCentroids (pix: int array, labels: int array, k: int, oldCentroids: real array) : real array * real =
    let val n = Array.length pix val sums = Array.array (k, 0.0) val counts = Array.array (k, 0)
        fun accum i = if i >= n then ()
          else let val lbl = Array.sub (labels, i) val pv = toReal (Array.sub (pix, i))
               in Array.update (sums, lbl, Array.sub (sums, lbl) + pv);
                  Array.update (counts, lbl, Array.sub (counts, lbl) + 1); accum (i + 1) end
        val _ = accum 0
        val newCentroids = Array.tabulate (k, fn i => let val c = Array.sub (counts, i)
            in if c = 0 then Array.sub (oldCentroids, i) else Array.sub (sums, i) / toReal c end)
        fun maxMove i acc = if i >= k then acc
          else let val mv = realAbs (Array.sub (newCentroids, i) - Array.sub (oldCentroids, i))
               in maxMove (i+1) (if mv > acc then mv else acc) end
    in (newCentroids, maxMove 0 0.0) end

  fun kmeansLoop (pix: int array, centroids: real array, iter: int) : int array * int =
    let val k = Array.length centroids val labels = assignPixels (pix, centroids)
        val (newCentroids, movement) = updateCentroids (pix, labels, k, centroids)
    in if iter >= MAX_ITER orelse movement < CONVERGE_THRESH then (labels, iter)
       else kmeansLoop (pix, newCentroids, iter + 1) end

  fun kmeans (pix: int array, k: int) : int array * int =
    let val centroids = initCentroids k in kmeansLoop (pix, centroids, 1) end

  fun labelsToGray (labels: int array, k: int) : int array =
    let val scale = if k <= 1 then 255 else 255 div (k - 1)
    in Array.tabulate (Array.length labels, fn i => Int.min (255, Array.sub (labels, i) * scale)) end

  fun segment (pix: int array, k: int) : int array * int =
    let val (labels, iters) = kmeans (pix, k) val output = labelsToGray (labels, k) in (output, iters) end

  (* Main *)
  fun main () =
    let val args = CommandLine.arguments ()
        val (inFile, outFile, k) = case args of
            [i, o, kStr] => (i, o, toInt kStr) | [i, o] => (i, o, 2)
          | _ => (print "Usage: kmeans_seq <input.txt> <output.txt> [k]\n"; OS.Process.exit OS.Process.failure; ("", "", 0))
        val _ = print ("K-Means Segmentation (Sequential)\n")
        val _ = print ("  Input:  " ^ inFile ^ "\n")
        val _ = print ("  Output: " ^ outFile ^ "\n")
        val _ = print ("  K:      " ^ Int.toString k ^ "\n")
        val (pix, h, w) = readImage inFile val n = h * w
        val _ = print ("  Size:   " ^ Int.toString w ^ "x" ^ Int.toString h ^ " (" ^ Int.toString n ^ " pixels)\n")
        val start = Time.now ()
        val (output, iters) = segment (pix, k)
        val elapsed = Time.toReal (Time.- (Time.now (), start))
        val _ = print ("  Iterations: " ^ Int.toString iters ^ "\n")
        val _ = print ("  Time:   " ^ Real.fmt (StringCvt.FIX (SOME 4)) elapsed ^ " s\n")
        val _ = writeImage (outFile, output, h, w)
        val _ = print ("  Done.\n")
    in () end

  val _ = main ()

end
