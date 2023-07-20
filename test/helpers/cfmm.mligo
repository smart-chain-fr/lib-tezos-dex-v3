#import "../../lib/cfmm/main.mligo" "Cfmm"
// #import "../../ligo/main_fa2_fa2.mligo" "Cfmm"
#import "./assert.mligo" "Assert"
#import "./utils.mligo" "Utils"
#import "../../lib/cfmm/defaults.mligo" "Default"
#import "./maths.mligo" "Maths"
#import "./config.mligo" "Config_helper"
#import "./observe_consumer.mligo" "Observer_helper"

type taddr = (Cfmm.parameter, Cfmm.storage) typed_address
type contr = Cfmm.parameter contract
type originated = {
    addr: address;
    taddr: taddr;
    contr: contr;
}

let minTickIndex : int = -1048575
let maxTickIndex : int = 1048575

(* Some dummy values intended to be used as placeholders *)
let dummy_token_info =
    Map.literal [("",
      Bytes.pack "ipfs://QmbKq7QriWWU74NSq35sDSgUf24bYWTgpBq3Lea7A3d7jU")]

(* Base storage *)
let base_storage (x_token_address, y_token_address, tick_spacing, fee_bps, proto_fee_bps : address * address * nat * nat * nat) : Cfmm.storage = 
    let constants = { 
        fee_bps = fee_bps; 
        ctez_burn_fee_bps = proto_fee_bps;
        x_token_id = 0n;
        y_token_id = 0n;
        x_token_address = x_token_address; // ("KT1PWx2mnDueood7fEmfbBDKx1D9BAnnXitn" : address);
        y_token_address = y_token_address; // ("KT1PWx2mnDueood7fEmfbBDKx1D9BAnnXitn" : address);
        tick_spacing = tick_spacing;
	} in
    let init_cumulatives_buffer_extra_slots = 0n in
    let metadata_map = (Big_map.empty : Cfmm.metadata_map) in
    Default.default_storage constants init_cumulatives_buffer_extra_slots metadata_map

(* Originate a Cfmm contract with given init_storage storage (without on-chain views) *)
let originate (init_storage : Cfmm.storage) =
    let (taddr, _, _) = Test.originate_uncurried Cfmm.main init_storage 0mutez in
    let contr = Test.to_contract taddr in
    let addr = Tezos.address contr in
    {addr = addr; taddr = taddr; contr = contr}

(*
    Originate a Cfmm contract with given init_storage storage
    Use this one if you need access to on-chain views
*)
let originate_from_file_by_config (config, init_storage, balance : Config_helper.config * Cfmm.storage * tez) =
    let f = Config_helper.get_main_path(config) in
    let v_mich = Test.run (fun (x:Cfmm.storage) -> x) init_storage in
    // let (addr, _, _) = Test.originate_from_file f "main" ["v_get_position_info"; "v_observe"; "v_snapshot_cumulatives_inside"] v_mich balance in
    let (addr, _, _) = Test.originate_from_file f "main" ["v_get_position_info"; "v_get_constants"] v_mich balance in
    let taddr : taddr = Test.cast_address addr in
    let contr = Test.to_contract taddr in
    {addr = addr; taddr = taddr; contr = contr}

(* Call entry point of Cfmm contr contract *)
let call (p, contr : Cfmm.parameter * contr) =
    Test.transfer_to_contract contr p 0mutez

(* Call entry point of Cfmm contr contract with amount *)
let call_with_amount (p, amount_, contr : Cfmm.parameter * tez * contr) =
    Test.transfer_to_contract contr p amount_


(* Entry points call helpers *)
let set_position(p, amount_, contr : Cfmm.set_position_param * tez * contr) =
    call_with_amount(Set_position(p), amount_, contr)

let update_position(p, amount_, contr : Cfmm.update_position_param * tez * contr) =
    call_with_amount(Update_position(p), amount_, contr)

let x_to_y(p, amount_, contr : Cfmm.x_to_y_param * tez * contr) =
    call_with_amount(X_to_y(p), amount_, contr)

let y_to_x(p, amount_, contr : Cfmm.y_to_x_param * tez * contr) =
    call_with_amount(Y_to_x(p), amount_, contr)

let increase_observation_count(p, amount_, contr : Cfmm.increase_observation_count_param * tez * contr) =
    call_with_amount(Increase_observation_count(p), amount_, contr)

let observe(p, amount_, contr : Cfmm.observe_param * tez * contr) =
    call_with_amount(Observe(p), amount_, contr)

let set_position_success(p, amount_, contr : Cfmm.set_position_param * tez * contr) =
    Assert.tx_success (set_position(p, amount_, contr))

let update_position_success(p, amount_, contr : Cfmm.update_position_param * tez * contr) =
    Assert.tx_success (update_position(p, amount_, contr))

let x_to_y_success(p, amount_, contr : Cfmm.x_to_y_param * tez * contr) =
    Assert.tx_success (x_to_y(p, amount_, contr))

let y_to_x_success(p, amount_, contr : Cfmm.y_to_x_param * tez * contr) =
    Assert.tx_success (y_to_x(p, amount_, contr))

let increase_observation_count_success(p, amount_, contr : Cfmm.increase_observation_count_param * tez * contr) =
    Assert.tx_success (increase_observation_count(p, amount_, contr))

let observe_success(p, amount_, contr : Cfmm.observe_param * tez * contr) =
    Assert.tx_success (observe(p, amount_, contr))


// GENERATE PARAM HELPERS

let generate_set_position_param (liq, (lowerTick, upperTick) : nat * (int * int)) : Cfmm.set_position_param = 
{
    lower_tick_index = {i = lowerTick};
    upper_tick_index = {i = upperTick};
    lower_tick_witness = {i = -1048575};
    upper_tick_witness = {i = -1048575};
    liquidity = liq;
    deadline = Tezos.get_now() + 1000;
    maximum_tokens_contributed = {x = liq; y = liq};
}

let generate_update_position_param (position_id ,liquidity, recipient, max_token_x, max_token_y : nat * int * address * nat * nat) : Cfmm.update_position_param = 
{
    position_id = position_id ;
    liquidity_delta = liquidity ;
    to_x = recipient;
    to_y = recipient;
    deadline = Tezos.get_now() + 1000 ;
    maximum_tokens_contributed = {x=max_token_x; y=max_token_y };
}

let generate_x_to_y_param (dx, min_dy, to_dy : nat * nat * address) : Cfmm.x_to_y_param = 
{
    dx=dx;
    deadline=Tezos.get_now() + 1000 ;
    min_dy=min_dy;
    to_dy=to_dy;
}

let generate_y_to_x_param (dy, min_dx, to_dx : nat * nat * address) : Cfmm.y_to_x_param = 
{
    dy=dy;
    deadline=Tezos.get_now() + 1000 ;
    min_dx=min_dx;
    to_dx=to_dx;
}


let generate_increase_observation_count_param(n : nat) : Cfmm.increase_observation_count_param =
{
    added_observation_count=n
}

let advanceTime(contr : contr) : nat = 
    let before_time = Tezos.get_now() in
    let () = Test.log("advanceTime") in
    // INCREASE_OBSERVATION_COUNT : Add some slots to the buffers to make the tests more meaningful.
    // let param : Cfmm.increase_observation_count_param = generate_increase_observation_count_param(0n) in
    // let () = increase_observation_count_success(param, 0tez, contr) in
    let param : Cfmm.set_position_param = generate_set_position_param(0n, (-10, 10)) in
    let () = set_position_success(param, 0tez, contr) in
    let after_time = Tezos.get_now() in
    let waitTime = abs(after_time - before_time) in
    let () = Test.log("waitTime=",waitTime) in
    waitTime

// TICKS HELPERS

// traverse ticks through next pointer
let rec forAllTicks (type a) (acc, next_tick, stop_func, ticks, process_elt : a list * Cfmm.tick_index * (Cfmm.tick_index -> bool) * Cfmm.tick_map * ((Cfmm.tick_index * Cfmm.tick_state) -> a)) : a list =
    if stop_func(next_tick) then
        acc
    else
        match Big_map.find_opt next_tick ticks with
        | Some ts -> forAllTicks(process_elt((next_tick,ts)) :: acc, ts.next, stop_func, ticks, process_elt)
        | None -> Test.failwith "unknown tick"
// traverse ticks through prev pointer
let rec forAllTicks_reverse (type a) (acc, prev_tick, stop_func, ticks, process_elt : a list * Cfmm.tick_index * (Cfmm.tick_index -> bool) * Cfmm.tick_map * ((Cfmm.tick_index * Cfmm.tick_state) -> a)) : a list =
    if stop_func(prev_tick) then
        acc
    else
        match Big_map.find_opt prev_tick ticks with
        | Some ts -> forAllTicks_reverse(process_elt((prev_tick,ts)) :: acc, ts.prev, stop_func, ticks, process_elt)
        | None -> Test.failwith "unknown tick"

let rec get_positions(id, stop_id, positions, acc: nat * nat * (Cfmm.position_id, Cfmm.position_state) big_map * (Cfmm.position_id * Cfmm.position_state) list) : (Cfmm.position_id * Cfmm.position_state) list =
    if id >= stop_id then
        acc
    else
        match Big_map.find_opt id positions with
        | Some ps -> get_positions(id + 1n, stop_id, positions, (id, ps)::acc)
        | None -> get_positions(id + 1n, stop_id, positions, acc)

let collectFees(caller, pos_id, receiver, contr : address * nat * address * contr) : unit =
    let () = Test.set_source caller in
    let param_update : Cfmm.update_position_param = generate_update_position_param(pos_id, 0, receiver, 0n, 0n) in
    update_position_success(param_update, 0tez, contr)


let collectAllFees(taddr, contr, receiver : taddr * contr * address) : unit =
    let store = Test.get_storage taddr in
    let cfmm_positions = get_positions(0n, store.new_position_id, store.positions, ([]: (Cfmm.position_id * Cfmm.position_state) list)) in
    let collect_fees(acc, (pi, ps) : test_exec_result list * (Cfmm.position_id * Cfmm.position_state)) : test_exec_result list =
        let owner = ps.owner in 
        let () = Test.set_source owner in
        let param_update : Cfmm.update_position_param = generate_update_position_param(pi, 0, receiver, 0n, 0n) in
        let r = update_position(param_update, 0tez, contr) in
        r :: acc
    in  
    let _rrr = List.fold collect_fees cfmm_positions ([] : test_exec_result list) in
    ()


// CUMULATIVES BUFFER HELPERS
let rec apply_on_cumulatives_buffer_aux (type a) (id, last, m, f, acc : nat * nat * (nat, Cfmm.timed_cumulatives) big_map * (nat * Cfmm.timed_cumulatives -> a) * a list) : a list =
    if (id > last) then
        acc
    else
        match Big_map.find_opt id m with
        | Some tc -> apply_on_cumulatives_buffer_aux(id+1n, last, m, f, f(id,tc)::acc)
        | None -> failwith("unknown buffer id")
    
// Apply a given function f on each elements of the timed_cumulatives_buffer (first -> last)
let apply_on_cumulatives_buffer_reversed (type a) (buffer, f : Cfmm.timed_cumulatives_buffer * (nat * Cfmm.timed_cumulatives -> a)) : a list =
    apply_on_cumulatives_buffer_aux(buffer.first, buffer.last, buffer.map, f, ([]: a list))
    
let apply_on_cumulatives_buffer (type a) (buffer, f : Cfmm.timed_cumulatives_buffer * (nat * Cfmm.timed_cumulatives -> a)) : a list =
    let result_reversed = apply_on_cumulatives_buffer_reversed(buffer, f) in
    Utils.List.rev result_reversed

// Apply a given function f on each elements of the timed_cumulatives_buffer (first -> first + reserved_length)
let apply_on_cumulatives_buffer_by_reserved_reversed (type a) (buffer, f : Cfmm.timed_cumulatives_buffer * (nat * Cfmm.timed_cumulatives -> a)) : a list =
    apply_on_cumulatives_buffer_aux(buffer.first, abs(buffer.first + buffer.reserved_length - 1n), buffer.map, f, ([]: a list))


// ASSERTS

(* assert Cfmm contract at [taddr] have [expected_admin] address as admin *)
let assert_liquidity (taddr, expected_liquidity : taddr * nat) =
    let s = Test.get_storage taddr in
    assert(s.liquidity = expected_liquidity)

// Assert tick equality
let assert_tick_states_equal(a, b : Cfmm.tick_state * Cfmm.tick_state) : bool =
    let () = assert(a.prev = b.prev) in
    let () = assert(a.next = b.next) in
    let () = assert(a.liquidity_net = b.liquidity_net) in
    let () = assert(a.n_positions = b.n_positions) in
    // let () = Test.log(a.seconds_outside) in 
    // let () = Test.log(b.seconds_outside) in 
    // let () = assert(a.seconds_outside = b.seconds_outside) in  // TODO
    let () = assert(a.tick_cumulative_outside = b.tick_cumulative_outside) in
    let () = assert(a.fee_growth_outside = b.fee_growth_outside) in
    let () = assert(a.seconds_per_liquidity_outside = b.seconds_per_liquidity_outside) in
    let () = assert(a.sqrt_price = b.sqrt_price) in
    true

let assert_positions_equal(a, b : Cfmm.position_state * Cfmm.position_state) : bool =
    let () = assert(a.lower_tick_index.i = b.lower_tick_index.i) in
    let () = assert(a.upper_tick_index.i = b.upper_tick_index.i) in
    let () = assert(a.owner = b.owner) in
    let () = assert(a.liquidity = b.liquidity) in
    let () = assert(a.fee_growth_inside_last.x.x128 = b.fee_growth_inside_last.x.x128) in
    let () = assert(a.fee_growth_inside_last.y.x128 = b.fee_growth_inside_last.y.x128) in
    true

(* assert Cfmm contracts at [taddr1 , taddr2] have same tick_state at given [tick_index] *)
let assert_tick_equal (taddr1, index, taddr2 : taddr * Cfmm.tick_index * taddr) : bool =
    let s1 = Test.get_storage taddr1 in
    let value1_opt : Cfmm.tick_state option = Big_map.find_opt index s1.ticks in 
    let s2 = Test.get_storage taddr2 in
    let value2_opt : Cfmm.tick_state option = Big_map.find_opt index s2.ticks in 
    match value1_opt, value2_opt with
    | Some st1, Some st2 -> assert_tick_states_equal(st1, st2)
    | None, None -> true
    | _, _ -> false

let assert_timed_cumulatives_equal(a, b : Cfmm.timed_cumulatives * Cfmm.timed_cumulatives) : bool =
    let () = assert(a.time = b.time) in
    let () = assert(a.tick.sum = b.tick.sum) in
    let () = assert(a.tick.block_start_value.i = b.tick.block_start_value.i) in
    let () = assert(a.spl.sum.x128 = b.spl.sum.x128) in
    let () = assert(a.spl.block_start_liquidity_value = b.spl.block_start_liquidity_value) in
    true

let assert_constant_equal(a, b : Cfmm.constants * Cfmm.constants) : bool =
    let () = assert(a.fee_bps = b.fee_bps) in
    let () = assert(a.ctez_burn_fee_bps = b.ctez_burn_fee_bps) in
    let () = assert(a.x_token_id = b.x_token_id) in
    let () = assert(a.y_token_id = b.y_token_id) in
    let () = assert(a.x_token_address = b.x_token_address) in
    let () = assert(a.y_token_address = b.y_token_address) in
    let () = assert(a.tick_spacing = b.tick_spacing) in
    true

let assert_storage_equal(a, b : Cfmm.storage * Cfmm.storage) : unit =
    // Verify liquidity
    let () = assert(a.liquidity = b.liquidity) in
    // Verify sqrt_price
    let () = assert(a.sqrt_price.x80 = b.sqrt_price.x80) in
    // Verify cur_tick_index
    let () = assert(a.cur_tick_index.i = b.cur_tick_index.i) in
    // Verify cur_tick_witness
    let () = assert(a.cur_tick_witness.i = b.cur_tick_witness.i) in
    // Verify fee_growth
    let () = assert(a.fee_growth.x.x128 = b.fee_growth.x.x128) in
    let () = assert(a.fee_growth.y.x128 = b.fee_growth.y.x128) in
    // Verify ticks
    let startTickIndex : Cfmm.tick_index = { i=-1048575 } in
    let stop_condition(p: Cfmm.tick_index) : bool = (p.i = 1048576) in
    let get_identity(p: (Cfmm.tick_index * Cfmm.tick_state)) : (Cfmm.tick_index * Cfmm.tick_state) = p in
    let a_ticks_list = forAllTicks(([]: (Cfmm.tick_index * Cfmm.tick_state) list), startTickIndex, stop_condition, a.ticks, get_identity) in
    let b_ticks_list = forAllTicks(([]: (Cfmm.tick_index * Cfmm.tick_state) list), startTickIndex, stop_condition, b.ticks, get_identity) in
    let eq_func(a, b: (Cfmm.tick_index * Cfmm.tick_state) * (Cfmm.tick_index * Cfmm.tick_state)) : bool = (a.0.i = b.0.i && assert_tick_states_equal(a.1, b.1)) in
    let _ : unit = assert(Utils.List.equal(a_ticks_list, b_ticks_list, eq_func)) in
    // Verify positions
    let a_positions = get_positions(0n, a.new_position_id, a.positions, ([]: (Cfmm.position_id * Cfmm.position_state) list)) in
    let b_positions = get_positions(0n, b.new_position_id, b.positions, ([]: (Cfmm.position_id * Cfmm.position_state) list)) in
    let eq_func(a, b: (Cfmm.position_id * Cfmm.position_state) * (Cfmm.position_id * Cfmm.position_state)) : bool = (a.0 = b.0 && assert_positions_equal(a.1, b.1)) in
    let _ : unit = assert(Utils.List.equal(a_positions, b_positions, eq_func)) in
    // Verify Cumulative buffer values
    // let () = assert(a.cumulatives_buffer.first = b.cumulatives_buffer.first) in // TODO
    // let () = assert(a.cumulatives_buffer.last = b.cumulatives_buffer.last) in // TODO
    let () = assert(a.cumulatives_buffer.reserved_length = b.cumulatives_buffer.reserved_length) in
    // TODO
    // let get_identity_cumulatives_buffer(ti,tc : nat * Cfmm.timed_cumulatives) : nat * Cfmm.timed_cumulatives = (ti,tc) in
    // let a_cumulatives_buffer_reversed = apply_on_cumulatives_buffer_reversed(a.cumulatives_buffer, get_identity_cumulatives_buffer) in
    // let b_cumulatives_buffer_reversed = apply_on_cumulatives_buffer_reversed(b.cumulatives_buffer, get_identity_cumulatives_buffer) in
    // let eq_func(a, b: (nat * Cfmm.timed_cumulatives) * (nat * Cfmm.timed_cumulatives)) : bool = (a.0 = b.0 && assert_timed_cumulatives_equal(a.1, b.1)) in
    // // let () = Test.log(a_cumulatives_buffer_reversed) in
    // // let () = Test.log(b_cumulatives_buffer_reversed) in
    // let _ : unit = assert(Utils.List.equal(a_cumulatives_buffer_reversed, b_cumulatives_buffer_reversed, eq_func)) in

    // Verify new_position_id
    let () = assert(a.new_position_id = b.new_position_id) in
    // Verify Constants
    let _ = assert_constant_equal(a.constants, b.constants) in 
    // verify ladder
    let () = assert(a.ladder = b.ladder) in
    // Verify TZIP-16 metadata
    let () = assert(a.metadata = b.metadata) in
    // Verify operators ( FA2-related )
    let () = assert(a.operators = b.operators) in
    ()



// INVARIANTS HELPERS
type accumulator = {
    seconds : int;
    tick_cumulative : int;
    fee_growth : Cfmm.balance_int_x128;
    seconds_per_liquidity : int ;
}

let empty_accumulator : accumulator =
{
    seconds = 0;
    tick_cumulative = 0;
    fee_growth = {x={x128=0}; y={x128=0}};
    seconds_per_liquidity = 0;
}

// Tick helper
let initTickAccumulatorsForBuffer(timestamp_in_seconds, cumul_buffer, store, tickIndex : int * Cfmm.oracle_view_param * Cfmm.storage * int) : accumulator =
    if store.cur_tick_index.i >= tickIndex then
        let ret : Cfmm.cumulatives_value = Option.unopt (List.head_opt cumul_buffer) in
        let cvTickCumulative : int = ret.tick_cumulative in
        let cvSecondsPerLiquidityCumulative : Cfmm.x128n = ret.seconds_per_liquidity_cumulative in
        { 
            seconds = timestamp_in_seconds;
            tick_cumulative = cvTickCumulative;
            fee_growth = {x={x128=int(store.fee_growth.x.x128)}; y={x128=int(store.fee_growth.y.x128)}};
            seconds_per_liquidity = int(cvSecondsPerLiquidityCumulative.x128);
        }
    else
        empty_accumulator


let initTickAccumulators(cfmm_address, observer, store, tickIndex : address * Observer_helper.originated * Cfmm.storage * int) : accumulator =
    if store.cur_tick_index.i >= tickIndex then
    
        // DEBUG
        // GET TIME FROM CUMULATIVES BUFFER (but should use Tezos.get_now())
        // retrieve all timestamp from cumulatives_buffer and take the last 
        // let get_time_from_cumulatives_buffer(_ti, tc : nat * Cfmm.timed_cumulatives) : timestamp = tc.time in
        // let buffer_times_reversed = apply_on_cumulatives_buffer_reversed(store.cumulatives_buffer, get_time_from_cumulatives_buffer) in
        // let test_timestamp = Option.unopt (List.head_opt buffer_times_reversed) in
        // let test_timestamp_int : int = test_timestamp - (0: timestamp) in

        // OBSERVE
        let current_time = Tezos.get_now() in
        let () = Observer_helper.call_observe_success((cfmm_address, [current_time]), observer.contr) in
        let oracle_view_param = Test.get_storage observer.taddr in
        // let () = Test.log("[initTickAccumulators] at ", current_time, " => ", oracle_view_param) in

        let ret : Cfmm.cumulatives_value = Option.unopt (List.head_opt oracle_view_param) in
        let cvTickCumulative : int = ret.tick_cumulative in
        let cvSecondsPerLiquidityCumulative : Cfmm.x128n = ret.seconds_per_liquidity_cumulative in
        { 
            seconds = current_time - (0: timestamp); //test_timestamp_int;
            tick_cumulative = cvTickCumulative;
            fee_growth = {x={x128=int(store.fee_growth.x.x128)}; y={x128=int(store.fee_growth.y.x128)}};
            seconds_per_liquidity = int(cvSecondsPerLiquidityCumulative.x128);
        }
    else
        empty_accumulator


let add_accumulators(tc1, tc2: accumulator * accumulator) : accumulator =
{
    seconds = tc1.seconds + tc2.seconds;
    tick_cumulative = tc1.tick_cumulative + tc2.tick_cumulative;
    fee_growth = {x={x128=tc1.fee_growth.x.x128 + tc2.fee_growth.x.x128}; y={x128=tc1.fee_growth.y.x128 + tc2.fee_growth.y.x128}} ;
    seconds_per_liquidity = tc1.seconds_per_liquidity + tc2.seconds_per_liquidity;
}

let assert_equal_accumulators(tc1, tc2: accumulator * accumulator) : unit =
    let _ : unit = assert(tc1.seconds = tc2.seconds) in
    let _ : unit = assert(tc1.tick_cumulative = tc2.tick_cumulative) in
    let _ : unit = assert(tc1.fee_growth.x.x128 = tc2.fee_growth.x.x128 ) in
    let _ : unit = assert(tc1.fee_growth.y.x128 = tc2.fee_growth.y.x128) in
    let _ : unit = assert(tc1.seconds_per_liquidity = tc2.seconds_per_liquidity) in
    ()

let sub_accumulators(tc1, tc2: accumulator * accumulator) : accumulator =
{
    seconds = tc1.seconds - tc2.seconds;
    tick_cumulative = tc1.tick_cumulative - tc2.tick_cumulative;
    fee_growth = {x={x128=tc1.fee_growth.x.x128 - tc2.fee_growth.x.x128}; y={x128=tc1.fee_growth.y.x128 - tc2.fee_growth.y.x128}} ;
    seconds_per_liquidity = tc1.seconds_per_liquidity - tc2.seconds_per_liquidity;
}

let tickAccumulatorsInside(st, _timestamp_in_seconds, cumul_buffer: Cfmm.storage * int * Cfmm.oracle_view_param) (low_ti, high_ti: Cfmm.tick_index * Cfmm.tick_index) : accumulator =
    let low_state = match Big_map.find_opt low_ti st.ticks with
    | Some ts -> ts
    | None -> failwith("tickAccumulatorsInside: unknown low tick_index")
    in
    let high_state = match Big_map.find_opt high_ti st.ticks with
    | Some ts -> ts
    | None -> failwith("tickAccumulatorsInside: unknown high tick_index")
    in
    //     -- Equation 6.17
    let tickAccumulatorAbove (type a) (ti, ts, globalAcc, tickAccOutside, sub_func: Cfmm.tick_index * Cfmm.tick_state * a * (Cfmm.tick_state -> a) * (a * a -> a)) : a =
        if st.cur_tick_index >= ti then
            sub_func(globalAcc, tickAccOutside(ts))
        else
            tickAccOutside(ts)
    in
    //     -- Equation 6.18
    let tickAccumulatorBelow (type a) (ti, ts, globalAcc, tickAccOutside, sub_func: Cfmm.tick_index * Cfmm.tick_state * a * (Cfmm.tick_state -> a) * (a * a -> a)) : a =
        if st.cur_tick_index >= ti then
            tickAccOutside(ts)
        else
            sub_func(globalAcc, tickAccOutside(ts))
    in
    //     -- Equation 6.19
    let tickAccumulatorInside (type a) (low_ts, high_ts, globalAcc, tickAccOutside, sub_func:  Cfmm.tick_state *  Cfmm.tick_state * a * (Cfmm.tick_state -> a) * (a * a -> a)) : a =
        let acc_below : a = tickAccumulatorBelow(low_ti, low_ts, globalAcc, tickAccOutside, sub_func) in
        let acc_above : a = tickAccumulatorAbove(high_ti, high_ts, globalAcc, tickAccOutside, sub_func) in
        sub_func(sub_func(globalAcc, acc_below), acc_above)
    in

    // GET TIME FROM CUMULATIVES BUFFER (but should use Tezos.get_now())
    // retrieve all timestamp from cumulatives_buffer and take the last 
    let get_time_from_cumulatives_buffer(_ti, tc : nat * Cfmm.timed_cumulatives) : timestamp = tc.time in
    let buffer_times_reversed = apply_on_cumulatives_buffer_reversed(st.cumulatives_buffer, get_time_from_cumulatives_buffer) in
    let test_timestamp = Option.unopt (List.head_opt buffer_times_reversed) in
    let test_timestamp_int : int = test_timestamp - (0: timestamp) in
    // let () = Test.log("test_timestamp_int", test_timestamp_int) in
    // let () = Test.log("timestamp_in_seconds", timestamp_in_seconds) in

    // TODO ( should use Tezos.get_now())
    // GET CURRENT TIME
    // let currentTime = Tezos.get_now() in
    // let currentTimeInt : int = currentTime - (0: timestamp) in


    // calls the View "Observe" to get CumulativesValue (cvTickCumulative, cvSecondsPerLiquidityCumulative)
    // let current_time = Tezos.get_now() in
    // let () = Observer_helper.call_observe_success((cfmm_address, [current_time]), observer.contr) in
    // let ret_list = Test.get_storage observer.taddr in
    // let () = Test.log("[tickAccumulatorsInside] observer helper", (current_time - (0: timestamp)),  ret_list) in


    let ret : Cfmm.cumulatives_value = Option.unopt (List.head_opt cumul_buffer) in
    let cvTickCumulative = ret.tick_cumulative in
    let cvSecondsPerLiquidityCumulative = ret.seconds_per_liquidity_cumulative in
    // let () = Test.log("cvSecondsPerLiquidityCumulative") in
    // let () = Test.log(cvSecondsPerLiquidityCumulative) in

    let sub_int(a, b : int * int) : int = a - b in
    let sub_balance_int_x128(a, b : Cfmm.balance_int_x128 * Cfmm.balance_int_x128) : Cfmm.balance_int_x128 = {x={x128=(a.x.x128 - b.x.x128)}; y={x128=(a.y.x128 - b.y.x128)} } in
    let fee_growth_int = {x = {x128=int(st.fee_growth.x.x128)} ; y = {x128=int(st.fee_growth.y.x128)} } in
    {
        seconds = tickAccumulatorInside(low_state, high_state, test_timestamp_int, (fun (elt: Cfmm.tick_state) -> int(elt.seconds_outside)), sub_int);
        tick_cumulative = tickAccumulatorInside(low_state, high_state, cvTickCumulative, (fun (elt: Cfmm.tick_state) -> elt.tick_cumulative_outside), sub_int);
        fee_growth = tickAccumulatorInside(low_state, high_state, fee_growth_int, (fun (elt: Cfmm.tick_state) -> {x={x128=int(elt.fee_growth_outside.x.x128)}; y={x128=int(elt.fee_growth_outside.y.x128)}}), sub_balance_int_x128);
        seconds_per_liquidity = tickAccumulatorInside(low_state, high_state, int(cvSecondsPerLiquidityCumulative.x128), (fun (elt: Cfmm.tick_state) -> int(elt.seconds_per_liquidity_outside.x128)), sub_int);
    }

// INVARIANTS

// | Invariants:
// 1. @cur_tick_witness@ is the highest initialized tick lower than or equal to @cur_tick_index@.
// 2.1. Current liquidity is equal to the sum of all the tick's @liquidity_net@
//      from the lowest tick up to the current tick.
// 2.2. Current liquidity is also equal to the sum of liquidities of positions
//      that cover the current tick.
// 3. @sqrt_price@ is the correct price for @cur_tick_index@.
let checkStorageInvariants(s: Cfmm.storage) : unit =
    // Invariant 1
    let startTickIndex : Cfmm.tick_index = { i=-1048575 } in
    let stop_condition(p: Cfmm.tick_index) : bool = (p.i = 1048576) in
    let get_identity(p: (Cfmm.tick_index * Cfmm.tick_state)) : (Cfmm.tick_index * Cfmm.tick_state) = p in
    let tick_list = forAllTicks(([]: (Cfmm.tick_index * Cfmm.tick_state) list), startTickIndex, stop_condition, s.ticks, get_identity) in

    // Verify expected witness
    let curTickIndex = s.cur_tick_index in
    let compute_max_tick_lower_current_tick_index(ticks, current_tick_index : (Cfmm.tick_index * Cfmm.tick_state) list * Cfmm.tick_index) : Cfmm.tick_index =
        let compute_max(acc, i : (Cfmm.tick_index * Cfmm.tick_index) * (Cfmm.tick_index * Cfmm.tick_state)) : (Cfmm.tick_index * Cfmm.tick_index) = 
            let (max, current) = acc in
            if (i.0 <= current) then
                let new_max = if max < i.0 then i.0 else max in
                (new_max, current)
            else
                acc
        in
        let result = List.fold compute_max ticks (startTickIndex, current_tick_index) in
        result.0
    in
    let expectedCurTickWitness = compute_max_tick_lower_current_tick_index(tick_list, curTickIndex) in
    let _ : unit = assert(s.cur_tick_witness = expectedCurTickWitness) in
     
    // Invariant 2.1.
    // compute liquidity
    let compute_sum_liquidity(ticks, current_tick_index : (Cfmm.tick_index * Cfmm.tick_state) list * Cfmm.tick_index) : int =
        let compute_sum(acc, i : (int * Cfmm.tick_index) * (Cfmm.tick_index * Cfmm.tick_state)) : (int * Cfmm.tick_index) = 
            if (i.0 <= acc.1) then
                (i.1.liquidity_net + acc.0, acc.1)
            else
                acc
        in
        let result = List.fold compute_sum ticks (0, current_tick_index) in
        result.0
    in
    let liquidityAfterPriorTicks = compute_sum_liquidity(tick_list, curTickIndex) in
    let _ : unit = assert(s.liquidity = abs(liquidityAfterPriorTicks)) in

    // Invariant 2.2.
    let compute_sum_active_positions(positions, max_position_id, cur_tick_index: Cfmm.position_map * nat * Cfmm.tick_index) : nat =
        let rec sum_position(id, positions, current, sum : nat * (Cfmm.position_id, Cfmm.position_state)big_map * Cfmm.tick_index * nat) : nat =
            if id = 0n then
                match Big_map.find_opt id positions with
                | Some ps -> 
                    if (ps.lower_tick_index <= current) && (current < ps.upper_tick_index) then
                        ps.liquidity + sum
                    else
                        sum
                | None -> sum
            else
                match Big_map.find_opt id positions with
                | Some ps ->
                    // sums only active positions 
                    if (ps.lower_tick_index <= current) && (current < ps.upper_tick_index) then
                        sum_position(abs(id - 1n), positions, current, ps.liquidity + sum)
                    else
                        sum_position(abs(id - 1n), positions, current, sum)
                | None -> sum_position(abs(id - 1n), positions, current, sum)
        in
        sum_position(max_position_id, positions, cur_tick_index, 0n)
    in
    let liquidityOfActivePositions = compute_sum_active_positions(s.positions, s.new_position_id, curTickIndex) in
    let _ : unit = assert(s.liquidity = liquidityOfActivePositions) in

    // Invariant 3.
    // Note that the global @cur_tick_index@ does not always match the global @sqrt_price@ _exactly_.
    // A small swap may cause the @sqrt_price@ to move a tiny bit,
    // but it may not be enough to make the @cur_tick_index@ jump a whole unit (+1 or -1).
    let sqrtPrice_CurTickIndex = Maths.sqrtPriceFor(s.cur_tick_index.i, s.ladder) in
    let sqrtPrice_CurTickIndexPlus1 = Maths.sqrtPriceFor(s.cur_tick_index.i + 1, s.ladder) in
    let _ : unit = assert(sqrtPrice_CurTickIndex <= s.sqrt_price && s.sqrt_price < sqrtPrice_CurTickIndexPlus1) in
    ()

// | Invariants:
// 1. The sum of all the tick's liquidity_net must be 0
// 2. Scanning the ticks from left-to-right, the running sum of their liquidity_net must never drop below 0
//      (otherwise we'd have a tick range with negative liquidity)
// 3. All ticks must have n_positions > 0
let checkTickInvariants(s : Cfmm.storage) : unit =
    let startTickIndex : Cfmm.tick_index = { i=-1048575 } in
    let stop_condition(p: Cfmm.tick_index) : bool = (p.i = 1048576) in
    // Invariant 1
    let get_liquidityNet(p: (Cfmm.tick_index * Cfmm.tick_state)) : int = p.1.liquidity_net in
    let all_liquidityNets_rtl = forAllTicks(([]: int list), startTickIndex, stop_condition, s.ticks, get_liquidityNet) in
    let sum_liquidityNets(acc, i : int * int) : int = acc + i in
    let result = List.fold sum_liquidityNets all_liquidityNets_rtl 0 in
    let () = assert(result = 0) in

    // Invariant 2
    let all_liquidityNets_ltr = Utils.List.rev all_liquidityNets_rtl in
    let sum_and_assert_liquidityNet(acc, i : int * int) : int = 
        let () = assert (acc >= 0) in 
        acc + i 
    in
    let _ = List.fold sum_and_assert_liquidityNet all_liquidityNets_ltr 0 in

    // Invariant 3
    let check_n_positions(p: (Cfmm.tick_index * Cfmm.tick_state)) : unit = assert(p.1.n_positions > 0n) in
    let _all_n_positions = forAllTicks(([]: unit list), startTickIndex, stop_condition, s.ticks, check_n_positions) in
    ()


// | Invariants:
// 1. The bigmap always contains at least 2 entries.
// 2. The linked-list is acyclical.
// 3. Tick indices are in strictly increasing order.
// 4. All bigmap indices are reachable by traversing the linked-list front to back or back to front.
// 5. All @prev@ and @next@ pointers are valid,
//      except for the first tick's @prev@ pointer
//      and the last tick tick's @next@ pointer.
let checkTickMapInvariants(s : Cfmm.storage) : unit =
    // FOR LEFT TO RIGHT
    let startTickIndex : Cfmm.tick_index = { i=-1048575 } in
    let stop_condition(p: Cfmm.tick_index) : bool = (p.i = 1048576) in
    // Invariant 1
    let identity(p: (Cfmm.tick_index * Cfmm.tick_state)) = p in
    let all_ticks_inversed_ltr = forAllTicks(([]: (Cfmm.tick_index * Cfmm.tick_state) list), startTickIndex, stop_condition, s.ticks, identity) in
    let () = assert(List.length all_ticks_inversed_ltr >= 2n) in

    // retrieve only indices from ticks
    let all_indices_inversed_ltr = List.map (fun(i:(Cfmm.tick_index * Cfmm.tick_state)) -> i.0) all_ticks_inversed_ltr in
    let all_indices_ltr = Utils.List.rev all_indices_inversed_ltr in
    // Invariant 3
    let compare_index(a, b : Cfmm.tick_index * Cfmm.tick_index) : bool = a.i < b.i in
    let all_comparison = Utils.List.mapAdjacent(all_indices_ltr, compare_index) in
    let _ : unit = List.iter (fun (i:bool) -> assert (i)) all_comparison in

    // Invariant 2
    let all_indices_no_duplicate_ltr = Utils.List.nub all_indices_ltr in
    let eq_func(a, b: Cfmm.tick_index * Cfmm.tick_index) : bool = (a.i = b.i) in
    let _ : unit = assert(Utils.List.equal(all_indices_ltr, all_indices_no_duplicate_ltr, eq_func)) in

    // Invariant 4
    let sorted_indices : Cfmm.tick_index list = Utils.List.quicksort all_indices_ltr (fun (a,b: Cfmm.tick_index * Cfmm.tick_index) -> (a.i <= b.i)) in
    let () = assert(Utils.List.equal(all_indices_ltr, sorted_indices, (fun (a,b: Cfmm.tick_index * Cfmm.tick_index) -> (a.i = b.i)))) in


    // FOR RIGHT TO LEFT
    let startTickIndex_prev : Cfmm.tick_index = { i=1048575 } in
    let stop_condition(p: Cfmm.tick_index) : bool = (p.i = -1048576) in
    // Invariant 1
    let identity(p: (Cfmm.tick_index * Cfmm.tick_state)) = p in
    let all_ticks_inversed_rtl = forAllTicks_reverse(([]: (Cfmm.tick_index * Cfmm.tick_state) list), startTickIndex_prev, stop_condition, s.ticks, identity) in
    let () = assert(List.length all_ticks_inversed_rtl >= 2n) in
    let all_ticks_ltr = all_ticks_inversed_rtl in

    // retrieve only indices from ticks
    let all_indices_ltr = List.map (fun(i:(Cfmm.tick_index * Cfmm.tick_state)) -> i.0) all_ticks_ltr in
    // Invariant 3
    let compare_index(a, b : Cfmm.tick_index * Cfmm.tick_index) : bool = a.i < b.i in 
    let all_comparison = Utils.List.mapAdjacent(all_indices_ltr, compare_index) in
    let _ : unit = List.iter (fun (i:bool) -> assert (i)) all_comparison in

    // Invariant 2
    let all_indices_no_duplicate_ltr = Utils.List.nub all_indices_ltr in
    let eq_func(a, b: Cfmm.tick_index * Cfmm.tick_index) : bool = (a.i = b.i) in
    let _ : unit = assert(Utils.List.equal(all_indices_ltr, all_indices_no_duplicate_ltr, eq_func)) in

    // Invariant 4
    let sorted_indices : Cfmm.tick_index list = Utils.List.quicksort all_indices_ltr (fun (a,b: Cfmm.tick_index * Cfmm.tick_index) -> (a.i <= b.i)) in
    let () = assert(Utils.List.equal(all_indices_ltr, sorted_indices, (fun (a,b: Cfmm.tick_index * Cfmm.tick_index) -> (a.i = b.i)))) in
    ()

// | Invariants:
// 1. Non-map fields in the buffer are sensible.
//    1.1. The last index is greater or equal than the first index.
//    1.2. The reserved map size is not smaller than the actual number of records.
// 2. The map contains values under the appropriate keys.
// 3. Timestamps increase strictly monotonically.
// 4. Cumulative values increase strictly monotonically.
let checkCumulativesBufferInvariants(s: Cfmm.storage) : unit =
    let buffer : Cfmm.timed_cumulatives_buffer = s.cumulatives_buffer in
    // Invariant 1.1
    let () = assert(buffer.last >= buffer.first) in

    // Invariant 1.2
    let actual_length = abs(buffer.last - buffer.first) + 1n in
    let () = assert(buffer.reserved_length >= actual_length) in
    
    // Invariant 2
    let get_id_from_cumulatives_buffer(ti,_tc : nat * Cfmm.timed_cumulatives) : nat = ti in
    let buffer_keys_reversed = apply_on_cumulatives_buffer_by_reserved_reversed(s.cumulatives_buffer, get_id_from_cumulatives_buffer) in
    let rec fill_one(current, last, acc: nat * nat * nat list) : nat list =
        if current > last then
            acc
        else
            fill_one(current + 1n, last, current::acc)
    in
    let expected_keys_reversed = fill_one(buffer.first, buffer.first + abs(buffer.reserved_length - 1n), ([]: nat list)) in
    let () = assert(Utils.List.equal(expected_keys_reversed, buffer_keys_reversed, (fun (a,b: nat * nat) -> (a = b)))) in

    // Invariant 3
    let get_time_from_cumulatives_buffer(_ti, tc : nat * Cfmm.timed_cumulatives) : timestamp = tc.time in
    let buffer_times = apply_on_cumulatives_buffer(buffer, get_time_from_cumulatives_buffer) in
    let sorted_buffer_times : timestamp list = Utils.List.quicksort buffer_times (fun (a,b: timestamp * timestamp) -> (a <= b)) in
    let () = assert(Utils.List.equal(buffer_times, sorted_buffer_times, (fun (a,b: timestamp * timestamp) -> (a = b)))) in

    // Invariant 4
    let get_splsum_from_cumulatives_buffer(_ti, tc : nat * Cfmm.timed_cumulatives) : Cfmm.x128n = tc.spl.sum in
    let buffer_splsum = apply_on_cumulatives_buffer(buffer, get_splsum_from_cumulatives_buffer) in
    let sorted_buffer_splsum : Cfmm.x128n list = Utils.List.quicksort buffer_splsum (fun (a,b: Cfmm.x128n * Cfmm.x128n) -> (a.x128 <= b.x128)) in
    let () = assert(Utils.List.equal(buffer_splsum, sorted_buffer_splsum, (fun (a,b: Cfmm.x128n * Cfmm.x128n) -> (a.x128 = b.x128)))) in
    ()

// | Invariant:
// For all accumulators: the sum of all the values accumulated between any two consecutive ticks
// must equal the global accumulated value.
// E.g.: if the ticks [i1, i2, i3, i4] have been initialized, then:
//   fee_growth == fee_growth_inside(i1, i2) + fee_growth_inside(i2, i3) + fee_growth_inside(i3, i4)
let checkAccumulatorsInvariants(s, timestamp_in_seconds, cumul_buffer : Cfmm.storage * int * Cfmm.oracle_view_param) : unit =
    // retrieve ticks indices (FROM LEFT TO RIGHT)
    let startTickIndex : Cfmm.tick_index = { i=-1048575 } in
    let stop_condition(p: Cfmm.tick_index) : bool = (p.i = 1048576) in
    let get_indice(p: (Cfmm.tick_index * Cfmm.tick_state)) : Cfmm.tick_index = p.0 in
    let all_indices_rtl = forAllTicks(([]: Cfmm.tick_index list), startTickIndex, stop_condition, s.ticks, get_indice) in
    let all_indices_ltr = Utils.List.rev all_indices_rtl in

    // Compute sum of all accumulators inside
    let compute_accumulators_inside = tickAccumulatorsInside(s, timestamp_in_seconds, cumul_buffer) in
    let all_values = Utils.List.mapAdjacent(all_indices_ltr, compute_accumulators_inside) in
    let sum_accumulators_inside = List.fold add_accumulators all_values empty_accumulator in
    // let () = Test.log(sum_accumulators_inside) in

    // TODO
    // GET TIME FROM CUMULATIVES BUFFER (but should use Tezos.get_now())
    // retrieve all timestamp from cumulatives_buffer and take the last 
    let get_time_from_cumulatives_buffer(_ti, tc : nat * Cfmm.timed_cumulatives) : timestamp = tc.time in
    let buffer_times_reversed = apply_on_cumulatives_buffer_reversed(s.cumulatives_buffer, get_time_from_cumulatives_buffer) in
    let test_timestamp = Option.unopt (List.head_opt buffer_times_reversed) in
    let test_timestamp_int : int = test_timestamp - (0: timestamp) in
    
    /////////////////////////////////////////////////////////////////////////////
    // let current_time = Tezos.get_now() in
    // let () = Observer_helper.call_observe_success((cfmm_address, [current_time]), observer.contr) in
    // let ret_list = Test.get_storage observer.taddr in
    // let () = Test.log("observer helper", (current_time - (0: timestamp)),  ret_list) in
    ///////////////////////////////////////////////////////////////////////////////

    let ret : Cfmm.cumulatives_value = Option.unopt (List.head_opt cumul_buffer) in
    let cvTickCumulative = ret.tick_cumulative in
    let cvSecondsPerLiquidityCumulative = ret.seconds_per_liquidity_cumulative in

    let fee_growth_int = {x = {x128=int(s.fee_growth.x.x128)} ; y = {x128=int(s.fee_growth.y.x128)} } in
    let globalAccumulators  : accumulator = {
        seconds = test_timestamp_int; //timestamp_in_seconds; //test_timestamp_int;
        tick_cumulative = cvTickCumulative;
        fee_growth = fee_growth_int;
        seconds_per_liquidity = int(cvSecondsPerLiquidityCumulative.x128);
    } in
    // let () = Test.log(globalAccumulators) in
    // let () = Test.log(sum_accumulators_inside) in
    let () = assert_equal_accumulators(globalAccumulators, sum_accumulators_inside) in
    ()

// | Invariant:
// The contract always has enough balance to liquidite all positions (and pay any fees due).
let checkBalanceInvariants(_s : Cfmm.storage) : unit =
    // TODO 
    // Can we invoke the update_position entrypoint and rollback ???? 
    // ---> maybe simulate the update_position entrypoint
    ()

let check_all_invariants_with_buffer(taddr, timestamp_in_seconds, cumul_buffer : taddr * int * Cfmm.oracle_view_param) : unit =
    let s = Test.get_storage taddr in
    let _ : unit = checkStorageInvariants(s) in
    let _ : unit = checkTickInvariants(s) in 
    let _ : unit = checkTickMapInvariants(s) in
    let _ : unit = checkCumulativesBufferInvariants(s) in
    let _ : unit = checkAccumulatorsInvariants(s, timestamp_in_seconds, cumul_buffer) in
    let _ : unit = checkBalanceInvariants(s) in
    ()

let check_all_invariants(taddr, cfmm_address, observer : taddr * address * Observer_helper.originated) : unit =
    let current_time = Tezos.get_now() in
    let () = Observer_helper.call_observe_success((cfmm_address, [current_time]), observer.contr) in
    let cumul_buffer = Test.get_storage observer.taddr in
    // let () = Test.log("[check_all_invariants] observer helper", (current_time - (0: timestamp)),  cumul_buffer) in
    check_all_invariants_with_buffer(taddr, current_time - (0:timestamp), cumul_buffer)


let checkCumulativesBufferTimeInvariants(_s : Cfmm.storage) : unit =
    ()