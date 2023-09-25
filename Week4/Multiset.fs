module Week4.Multiset

type Multiset2<'a when 'a : comparison> = Map<'a, int>

let inv ms =
    ms
    |> Map.forall (fun k v -> v > 0)
    
let insert a c ms =
    match Map.containsKey a ms with
    | true ->
        let value = Map.find a ms
        Map.add a (c+value) ms
    | false -> Map.add a c ms


let union m1 m2 =
    let f = fun s a c -> insert a c s
    Map.fold f m1 m2
