namespace Week4

module Starter =
    // 5.1
    let filter pred xs =
        List.foldBack (fun a b -> if (pred a) then a :: b else b) xs []
    
    // 5.2
    let revrev xs =
        let revInner ys =
            List.foldBack (fun a b -> b @ [a]) ys []
        List.foldBack (fun a b -> b @ [revInner a]) xs []
        
    // 5.5
    
    let areNb c1 c2 l = List.exists (fun y -> (c1, c2)=y || (c2,c1)=y) l
        
    let canBeExBy col c m = List.exists (fun c' -> not(areNb c' c m)) col
                          
    let extColoring cols c m =
        List.foldBack (fun col cols -> if (canBeExBy col c m)
                                        then (c::col)::cols
                                        else col::cols)
                  [] cols
    
    // 5.10
    
    
    // let nat = Seq.initInfinite (fun i -> i)
    // nat |> Seq.fold (fun s i -> let nFib = (Seq.item (i-1) nat) + i
    //                             Seq.append s nfib s)