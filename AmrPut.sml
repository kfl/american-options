structure AmrPut =
struct

structure V = Vector
structure VS = VectorSlice

val i2s = Int.toString

fun zipWith opr v1 v2 = 
    let val (x, y) = if VS.length v2 < VS.length v1 
                     then (VS.subslice(v1, 0, SOME(VS.length v2)), v2)
                     else (v1, v2)
    in  VS.full(VS.mapi (fn (i,a) => opr(a, VS.sub(y, i))) x)
    end


(* Pointwise manipulation of vectors-slices and scalars *)
infix ^*^ ^+^ -^ *^

fun v1 ^*^ v2 = zipWith op* v1 v2
fun v1 ^+^ v2 = zipWith op+ v1 v2

fun c -^ v = VS.full(VS.map (fn x => c - x) v)
fun c *^ v = VS.full(VS.map(fn x => c * x) v)


fun pmax v c = VS.full (VS.map (fn x => Real.max(x, c)) v)
fun ppmax v1 v2 = zipWith Real.max v1 v2

fun vtake i v = VS.subslice(v, 0, SOME i)
fun vdrop i v = VS.subslice(v, i, NONE)
fun vtail v = vdrop 1 v
fun vinit v = vtake (VS.length v - 1) v


fun binom expiry = 
    let (* standard econ parameters *)
        val strike = 100.0
        val bankDays = 252
        val s0 = 100.0 
        val r = 0.03 val alpha = 0.07 val sigma = 0.20

        val n = expiry*bankDays
        val dt = real expiry / real n
        val u = Math.exp(alpha*dt+sigma* Math.sqrt dt)
        val d = Math.exp(alpha*dt-sigma* Math.sqrt dt)
        val stepR = Math.exp(r*dt)
        val q = (stepR-d)/(u-d)
        val qUR = q/stepR val qDR = (1.0-q)/stepR

        val uPow = VS.full(V.tabulate(n+1, fn i => Math.pow(u, real i)))
        val dPow = VS.full(V.tabulate(n+1, fn i => Math.pow(d, real (n-i))))
    
        val st = s0 *^ (uPow ^*^ dPow)
        val finalPut = pmax (strike -^ st) 0.0

       (* for (i in n:1) {
            St<-S0*u.pow[1:i]*d.pow[i:1]
            put[1:i]<-pmax(strike-St,(qUR*put[2:(i+1)]+qDR*put[1:i]))
          }
        *)
        fun prevPut (i, put) = 
            let val st = s0 *^ ((vtake i uPow) ^*^ (vdrop (n+1-i) dPow)) 
            in  ppmax(strike -^ st) ((qUR *^ vtail put) ^+^ (qDR *^ vinit put))
            end
        val first = V.foldl prevPut finalPut (V.tabulate(n, fn i => n-i))

    in VS.sub (first, 0)
    end

(* Expected results for binom:

      expiry    price 
           1      6.74543295135838
           8     13.94568883837488
          16     16.22259138591852
          30     17.65370590709356
          64     18.42993156506373
         128     18.573732615311993
*)


end
