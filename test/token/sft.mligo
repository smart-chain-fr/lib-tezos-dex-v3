#import "ligo-extendable-fa2/lib/multi_asset/fa2.mligo" "FA2"
#import "./sft_errors.mligo" "Errors"

type storage = FA2.storage

type extension = {
    admin : address;
    requested_admin : address option;
    total_supply : nat;
    next_token_id : nat;
    probationary_deadline : timestamp;
    is_paused : bool;
}

type extended_storage = extension storage

type return = operation list * extended_storage

let authorizeAdmin (s : extended_storage) : unit =
    assert_with_error ( (Tezos.get_sender()) = s.extension.admin) Errors.only_admin

let is_operator (operators, from_ : FA2.Operators.t * address) : bool =
  let sender_ = (Tezos.get_sender()) in
  if (sender_ = from_)
    then true
  else
    let authorized = match Big_map.find_opt (from_,sender_) operators with
        Some (a) -> a | None -> Set.empty
    in
    if Set.mem 0n authorized
        then true
        else false

let authorizeTransfer (s : extended_storage) : unit =
    let () = assert_with_error (s.extension.is_paused = false) Errors.paused in
    if (Tezos.get_now() > s.extension.probationary_deadline)
    then
        ()
    else
        if (s.extension.admin = Tezos.get_sender() )
        then 
            ()
        else
            if (is_operator(s.operators, s.extension.admin))
            then
                ()
            else failwith Errors.secondary_market_not_started_yet

let burn (s: extended_storage) : return =
    let () = assert_with_error (s.extension.is_paused = true) Errors.not_paused in
    (([]), { s with ledger = (Big_map.empty : FA2.Ledger.t); extension = { s.extension with total_supply = 0n } })

let changeAdmin (new_admin : address)(s: extended_storage) : return =
    let () = assert_with_error (s.extension.is_paused = false) Errors.paused in
    (([]), { s with extension = { s.extension with requested_admin = Some(new_admin) } })

let approveAdmin (s: extended_storage) : return =
    let () = assert_with_error (s.extension.is_paused = false) Errors.paused in
    let requested_admin = match s.extension.requested_admin with
        | Some addr -> addr
        | None -> failwith(Errors.no_requested_admin)
    in
    let () = assert_with_error( (Tezos.get_sender()) = requested_admin) Errors.sender_not_requested_admin in
    (([]), { s with extension = { s.extension with admin = requested_admin; requested_admin = (None : address option); } })

let pause (is_paused : bool)(s: extended_storage) : return =
    (([]), { s with extension = { s.extension with is_paused } })

type parameter = [@layout:comb]
    | Transfer of FA2.transfer
    | Balance_of of FA2.balance_of
    | Update_operators of FA2.update_operators
    | ChangeAdmin of address
    | ApproveAdmin of unit
    | Burn
    | Pause of bool

let main (p, s : parameter * extended_storage) : operation list * extended_storage =
    let _  = assert_with_error(Tezos.get_amount() = 0tez) Errors.expects_0_tez in
    match p with
        | Transfer                   p -> let () = authorizeTransfer s in
                                        FA2.transfer p s
        | Balance_of               p -> FA2.balance_of p s
        | Update_operators         p -> FA2.update_ops p s
        | ChangeAdmin              p -> let () = authorizeAdmin s in
                                       changeAdmin p s
        | ApproveAdmin               -> approveAdmin s
        | Burn                       -> let () = authorizeAdmin s in
                                        burn s
        | Pause                    p -> let () = authorizeAdmin s in
                                        pause p s

let assert_token_exist (s : extended_storage) (token_id : nat) : unit  =
    let _ = match Big_map.find_opt token_id s.token_metadata with
        | Some x -> x
        | None -> (failwith(Errors.undefined_token))
    in
    ()

let get_balance (s : extended_storage) (owner : address) (token_id : nat) : nat =
    let () = assert_token_exist s token_id in
    match Big_map.find_opt (owner, token_id) s.ledger with
        | Some x -> x
        | None -> (failwith(Errors.missing_quantity))
    
[@view] let get_balance (p, s : address * extended_storage) : nat =
    get_balance s p 0n
