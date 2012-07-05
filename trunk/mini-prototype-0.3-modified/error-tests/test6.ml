(* Type constructor clash.
  *)

type list : * -> * = Nil : forall a. list a
                | Cons : forall a. a * list a -> list a

type foldl : * -> *
 
let average weight list =
     let iterator (x,(sum,length)) = (sum + weight x, length + 1)
     and (sum,length) = foldl iterator (0,0) list
     in sum + length (* should be sum/length *)
 
let find_best weight lists =
     let average1 = average weight
         let iterator (list,(best,max)) =
             let avg_list = average1 list
             in if avg_list > max
                then (list,avg_list)
                else (best,max)
             end
         val (best,_) = foldl iterator (nil,0) lists
     in best
     end
 
val find_best_simple = find_best 1
