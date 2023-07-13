// #import "../../ligo/main_fa2_fa2.mligo" "Cfmm"
#import "../../lib/cfmm/main.mligo" "Cfmm"

type storage = Cfmm.oracle_view_param

type call_observe_param = address * timestamp list

type parameter = 
  CallObserve of call_observe_param
| Reset of unit 
| ReceiveObserve of Cfmm.oracle_view_param

let reset(_p, _s : unit * storage) : operation list * storage =
    ([] : operation list), ([]: Cfmm.oracle_view_param)

let receive_observe(p, _s : Cfmm.oracle_view_param * storage) : operation list * storage =
    ([] : operation list), p

let call_observe (p, s : call_observe_param * storage) : operation list * storage =
    let (addr, _param) = p in
    let cfmm_entrypoint = match Tezos.get_entrypoint_opt "%observe" addr with
    | Some v -> v
    | None -> failwith "no entrypoint Observe on given address"
    in
    let now = Tezos.get_now() in
    let arg : Cfmm.observe_param = {
        times = [now]; //param;
        callback = match Tezos.get_entrypoint_opt "%receiveObserve" (Tezos.get_self_address()) with
            | Some v -> v
            | None -> failwith "no entrypoint ReceiveObserve on given address" 
    }
    in
    let op = Tezos.transaction arg 0tez cfmm_entrypoint in
    ([op], s)

let main (p, s : parameter * storage) : operation list * storage =
    match p with
      CallObserve p -> call_observe(p, s)
    |  Reset p -> reset(p, s)
    | ReceiveObserve p -> receive_observe(p, s)


