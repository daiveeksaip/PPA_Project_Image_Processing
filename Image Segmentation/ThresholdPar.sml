structure ThresholdPar =
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

  fun writeImage (file: string, pix: int Seq.t, h: int, w: int) : unit =
    let val out = TextIO.openOut file val n = h * w
        val _ = TextIO.output (out, Int.toString h ^ " " ^ Int.toString w ^ " 1\n")
        fun wr i = if i >= n then ()
          else (if i mod w > 0 then TextIO.output (out, " ") else ();
                TextIO.output (out, Int.toString (Seq.nth pix i));
                if i mod w = w - 1 then TextIO.output (out, "\n") else (); wr (i+1))
        val _ = wr 0 val _ = TextIO.closeOut out
    in () end

  (* Histogram-Par *)
  fun histSeq (pix: int array) (lo: int, hi: int) : int array =
    let val h = zeroHist ()
        fun lp i = if i >= hi then () else let val v = Array.sub (pix, i)
            in Array.update (h, v, Array.sub (h, v) + 1); lp (i+1) end
    in lp lo; h end

  fun histPar (pix: int array) : int array =
    let val n = Array.length pix val chunks = if n = 0 then 0 else (n + GRAIN - 1) div GRAIN
        fun make k = let val lo = k * GRAIN val hi = Int.min (n, lo + GRAIN) in histSeq pix (lo, hi) end
    in if n = 0 then zeroHist () else Parallel.reduce histMerge (zeroHist ()) (0, chunks) make end

  (* Otsu-Par *)
  fun otsuThreshold (hist: int array, total: int) : int =
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

  (* Apply-Par *)
  fun applyThreshold (pix: int array, t: int) : int Seq.t =
    let val n = Array.length pix
    in Parallel.tabulate (0, n) (fn i => if Array.sub (pix, i) >= t then 255 else 0) end

  (* Main *)
  fun main () =
    let val args = CommandLine.arguments ()
        val (inF, outF, meth, thr) = case args of
            [] => ("image_matrix.txt", "out.txt", "otsu", 128)
          | [a] => (a, "out.txt", "otsu", 128) | [a,b] => (a, b, "otsu", 128)
          | [a,b,c] => (a, b, c, 128) | [a,b,c,d] => (a, b, c, toInt d)
          | _ => raise Fail "usage: prog <in> <out> <method> [thresh]"
        val (pix, h, w) = readImage inF val n = h * w
        val _ = print ("Image: " ^ Int.toString w ^ "x" ^ Int.toString h ^ "\n")
        val result = case meth of
            "otsu" => let val hist = histPar pix val t = otsuThreshold (hist, n)
                val _ = print ("Otsu threshold: " ^ Int.toString t ^ "\n") in applyThreshold (pix, t) end
          | "normal" => (print ("Threshold: " ^ Int.toString thr ^ "\n"); applyThreshold (pix, thr))
          | _ => raise Fail "method: otsu or normal"
        val _ = writeImage (outF, result, h, w) val _ = print "Done.\n"
    in () end

  val _ = main ()

end
