#import "../token/fa12.mligo" "Token"
#import "./assert.mligo" "Assert"

(* Some types for readability *)
type taddr = (Token.parameter, Token.storage) typed_address
type contr = Token.parameter contract
type originated = {
    addr: address;
    taddr: taddr;
    contr: contr;
}

(* Base storage *)
let base_storage (ledger, token_metadata, total_supply, metadata : Token.FA12.Ledger.t * Token.FA12.TokenMetadata.t * nat * Token.FA12.Storage.Metadata.t) : Token.storage = {
    ledger = ledger;
    token_metadata = token_metadata;
    total_supply = total_supply;
    metadata = metadata;
}

(* Originate a Asset contract with given init_storage storage *)
let originate (init_storage, balance : Token.storage * tez) =
    let (taddr, _, _) = Test.originate_uncurried Token.main init_storage balance in
    let contr = Test.to_contract taddr in
    let addr = Tezos.address contr in
    {addr = addr; taddr = taddr; contr = contr}

let dummy_token_metadata (token_id : nat): Token.FA12.TokenMetadata.data = {
    token_id=token_id;
    token_info=Map.literal[("", 0x01)];
}



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
let call (p, contr : Token.parameter * contr) =
    Test.transfer_to_contract contr (p) 0mutez

let approve (p, contr : Token.FA12.approve * contr) =
    call(Approve(p), contr)

let approve_success (p, contr : Token.FA12.approve * contr) =
    Assert.tx_success(approve(p, contr))

let transfer (p, contr : Token.FA12.transfer * contr) =
    call(Transfer(p), contr)

let transfer_success (p, contr : Token.FA12.transfer * contr) =
    Assert.tx_success(transfer(p, contr))

let get_user_balance(taddr, owner : taddr * address) =
    let s = Test.get_storage taddr in
    let (user_balance, _) = Token.FA12.Ledger.get_for_user s.ledger owner in
    user_balance

let assert_user_balance(taddr, owner, expected_balance : taddr * address * nat) =
    let s = Test.get_storage taddr in
    let (user_balance, _) = Token.FA12.Ledger.get_for_user s.ledger owner in
    // let () = Test.log(s) in 
    assert(user_balance = expected_balance)

let assert_user_balance_in_range(taddr, owner, expected_balance, epsilon : taddr * address * nat * nat) =
    let s = Test.get_storage taddr in
    let (user_balance, _) = Token.FA12.Ledger.get_for_user s.ledger owner in
    assert(abs(user_balance - expected_balance) <= epsilon)


(* Verifies allowance amount for a given owner and spender *)
let assert_allowance
    (contract_address : (Token.parameter, Token.storage) typed_address )
    (owner : address)
    (spender : address)
    (expected_allowance : nat) =
    let storage = Test.get_storage contract_address in
    let ledger = storage.ledger in
    match (Big_map.find_opt owner ledger) with
    | Some amt_allow -> 
        let () = match (Map.find_opt spender amt_allow.1) with
        | Some v -> assert (v = expected_allowance)
        | None -> assert (expected_allowance = 0n)
        in
        () 
    | None -> failwith "incorret address"
    
(* Verifies balances of 3 accounts *)
let assert_balances
    (contract_address : (Token.parameter, Token.storage) typed_address )
    (a, b, c : (address * nat) * (address * nat) * (address * nat)) =
    let (owner1, balance1) = a in
    let (owner2, balance2) = b in
    let (owner3, balance3) = c in
    let storage = Test.get_storage contract_address in
    let ledger = storage.ledger in
    let () = match (Big_map.find_opt owner1 ledger) with
    Some amt -> assert (amt.0 = balance1)
    | None -> failwith "incorret address"
    in
    let () = match (Big_map.find_opt owner2 ledger) with
    Some amt ->  assert (amt.0 = balance2)
    | None -> failwith "incorret address"
    in
    let () = match (Big_map.find_opt owner3 ledger) with
    Some amt -> assert (amt.0 = balance3)
    | None -> failwith "incorret address"
    in
    ()
