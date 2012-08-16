functor AmrPutVec(V : VEC) =
struct

fun curry f x y = f(x,y)

(* Pointwise manipulation of vectors-slices and scalars *)
infix ^*^ ^+^ -^ *^

fun v1 ^*^ v2 = V.map2 (curry op*) v1 v2
fun v1 ^+^ v2 = V.map2 (curry op+) v1 v2
fun c -^ v = V.map (fn x => c - x) v
fun c *^ v = V.map (fn x => c * x) v

fun pmax v c = V.map (fn x => Real.max(x, c)) v
fun ppmax v1 v2 = V.map2 (curry Real.max) v1 v2

fun vtail v = V.dr 1 v
fun vinit v = V.tk (V.length v - 1) v

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

        val uPow = V.tabulate (n+1) (fn i => Math.pow(u, real i))
	val uPow = V.memoize uPow
        val dPow = V.tabulate (n+1) (fn i => Math.pow(d, real (n-i)))
	val dPow = V.memoize dPow

        val st = s0 *^ (uPow ^*^ dPow)
        val finalPut = pmax (strike -^ st) 0.0

       (* for (i in n:1) {
            St<-S0*u.pow[1:i]*d.pow[i:1]
            put[1:i]<-pmax(strike-St,(qUR*put[2:(i+1)]+qDR*put[1:i]))
          }
        *)
        fun prevPut (i, put) = 
            let val st = s0 *^ ((V.tk i uPow) ^*^ (V.dr (n+1-i) dPow)) 
		val put = V.memoize put
            in  ppmax(strike -^ st) ((qUR *^ vtail put) ^+^ (qDR *^ vinit put))
            end
        val first = V.foldl prevPut finalPut (V.tabulate n (fn i => n-i))

    in V.sub (first, 0)
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
