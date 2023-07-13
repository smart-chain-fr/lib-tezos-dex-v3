#import "../observe_consumer/observe_consumer.mligo" "ObserveConsumer"
#import "./assert.mligo" "Assert"

(* Some types for readability *)
type taddr = (ObserveConsumer.parameter, ObserveConsumer.storage) typed_address
type contr = ObserveConsumer.parameter contract
type originated = {
    addr: address;
    taddr: taddr;
    contr: contr;
}

(* Base storage *)
let base_storage () : ObserveConsumer.storage =  ([] : ObserveConsumer.Cfmm.oracle_view_param)


(* Originate a ObserveConsumer contract with given init_storage storage *)
let originate (init_storage, balance : ObserveConsumer.storage * tez) =
    let (taddr, _, _) = Test.originate_uncurried ObserveConsumer.main init_storage balance in
    let contr = Test.to_contract taddr in
    let addr = Tezos.address contr in
    {addr = addr; taddr = taddr; contr = contr}


(* Originate a Token contract with given init_storage storage *)
// let originate (init_storage : Token.storage) =
//     let (taddr, _, _) = Test.originate Token.main init_storage 0mutez in
//     let contr = Test.to_contract taddr in
//     let addr = Tezos.address contr in
//     {addr = addr; taddr = taddr; contr = contr}

// let originate_from_file (init_storage, balance : Token.storage * tez) =
//     let f = "../token/extended_fa2.mligo" in
//     let v_mich = Test.run (fun (x:Token.storage) -> x) init_storage in
//     let (addr, _, _) = Test.originate_from_file f "main" ["get_balance"] v_mich balance in
//     let taddr : taddr = Test.cast_address addr in
//     let contr = Test.to_contract taddr in
//     {addr = addr; taddr = taddr; contr = contr}

(* Call entry point of Token contr contract *)
let call (p, contr : ObserveConsumer.parameter * contr) =
    Test.transfer_to_contract contr (p) 0mutez

let call_observe (p, contr : ObserveConsumer.call_observe_param * contr) =
    call(CallObserve(p), contr)

let call_observe_success (p, contr : ObserveConsumer.call_observe_param * contr) =
    Assert.tx_success(call_observe(p, contr))

