structure AP = AmrPutVec(Fvec)
(*
structure AP = AmrPutVec(ListVec)
structure AP = AmrPut
*)

fun pr n =
    let val _ = print ("AmrPut.binom(" ^ Int.toString n ^ ") = ")
        val r = AP.binom n
    in print (Real.toString r ^ "\n")
    end

val _ = List.app pr [1,8,16,30,64,128]
