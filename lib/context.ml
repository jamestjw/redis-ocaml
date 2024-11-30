module StringMap = Map.Make (String)

type context = { mutable store : string StringMap.t }

let mk () = { store = StringMap.empty }
let get { store } key = StringMap.find_opt key store
let set ({ store } as ctx) key value = ctx.store <- StringMap.add key value store
