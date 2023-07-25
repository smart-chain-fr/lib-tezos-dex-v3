#import "../helpers/assert.mligo" "Assert"
#import "../bootstrap/bootstrap.mligo" "Bootstrap"
#import "../helpers/log.mligo" "Log"
#import "../helpers/cfmm.mligo" "Cfmm_helper"
#import "../../lib/cfmm/main.mligo" "Cfmm"
#import "../helpers/fa12.mligo" "FA12_helper"
#import "../helpers/extended_fa2.mligo" "ExtendedFA2_helper"
#import "../helpers/maths.mligo" "Maths"
#import "../helpers/utils.mligo" "Utils"
#import "../helpers/config.mligo" "Config_helper"
#import "../helpers/observe_consumer.mligo" "Observer_helper"

let () = Log.describe("[FA12-FA2] [Cfmm.complex_position] test suite")

let config = ({x=FA12; y=FA2} : Config_helper.config)

//////////////////////////////////////////////////////////////////////
//                 HELPER FOR RANDOM NUMBER GENERATION              //
//////////////////////////////////////////////////////////////////////

// For random number generation
// Implements the "logistic suite" : x_n+1 = mu * x_n * (1 - x_n)
type float_0_1 = {
    value : int;
    exp : int;
}

let rec pow(acc, x, y : nat * nat * nat) : nat = if y = 0n then acc else pow(x * acc, x, abs(y-1n))

let rec get_pow(acc, n : nat * nat) : nat =
    if n >= 10n then
        get_pow(acc+1n, n / 10n)
    else
        acc 

let rec reduce(x : float_0_1) : float_0_1 =
    let (d,r) : int * nat = match ediv x.value 10n with
    | Some (a,b) -> (a,b)
    | None -> Test.failwith("Internal error: reduce float") 
    in
    if r = 0n then
        reduce({value=d; exp=x.exp+1})
    else
        x

let cut_mantiss(x, precision : float_0_1 * nat) : float_0_1 =
    if abs(x.exp) > precision then
        let denominator = pow(1n, 10n, abs(abs(x.exp) - precision)) in
        let new_val = x.value / denominator in 
        { value=new_val; exp = 0n-precision }
    else
        x

let next_random_number(xn : float_0_1) : float_0_1 = 
    let mu = 387n in // set value greater than 3,57 for choatic behavior
    let () = assert(xn.exp < 0) in
    let () = assert(xn.value > 0) in
    let one_pow_exp = pow(1n, 10n, abs(xn.exp)) in
    let one_minus_xn = one_pow_exp - xn.value in
    let numerator = mu * xn.value * one_minus_xn in
    let result = { value = numerator; exp = 0n - (abs(xn.exp) * 2n + 2n)} in
    let result = reduce(result) in
    let result = cut_mantiss(result, 80n) in
    result

let gen_number(prev_rand, min, max : float_0_1 * int * int) : int * float_0_1 =
    // get a float in [0; 1]
    let rand = next_random_number(prev_rand) in
    // re-center on given interval
    let l = max - min in
    let d = rand.value mod l in 
    let result = min + d in
    (result, rand)

//////////////////////////////////////////////////////////////////////
//                 HELPER FOR TEST CONFIG                           //
//////////////////////////////////////////////////////////////////////

type create_position_data =
    { 
        cpdLowerTickIndex : int;
        cpdUpperTickIndex : int;
        cpdLiquidityDelta : nat;
        cpdWaitTime : nat;
    }

let generate_swap_direction(nb : nat) : bool list =
    let gen_args(count : nat) : bool = (count mod 2 = 0n) in
    let gen_swap_dir(arg: bool) : bool = arg in
    Utils.generic_repeat(nb, gen_swap_dir, gen_args, ([]: bool list))

let generate_create_position_data(seed, lower_bound, upper_bound : float_0_1 * int * int) : create_position_data =
    // Setup random number generator
    let rand = next_random_number(seed) in 

    let (lower, rand) = gen_number(rand, lower_bound, upper_bound) in
    let (upper, rand) = gen_number(rand, lower + 1n, upper_bound + 1n) in
    let (cpdLiquidityDelta, rand) = gen_number(rand, 1, 100000) in
    let (cpdWaitTime, _rand) = gen_number(rand, 1, 10) in
    {
        cpdLowerTickIndex = lower;
        cpdUpperTickIndex = upper;
        cpdLiquidityDelta = abs(cpdLiquidityDelta);
        cpdWaitTime = abs(cpdWaitTime);
    }

let genNonOverlappingPositions(nb_positions, lower_bound, upper_bound : nat * int * int) : create_position_data list =
    let rand_seed = {value=87341358763514654866; exp=-20} in
    let rec rand_n_times(steps, last : nat * float_0_1) : float_0_1 = 
        if steps = 0n then
            last
        else
            let next = next_random_number(last) in
            rand_n_times(abs(steps - 1n), next)
    in
    let gen_args(count : nat) : (float_0_1 * int * int) = 
        let seed : float_0_1 = rand_n_times(count, rand_seed) in
        (seed, lower_bound, upper_bound) 
    in
    let positions = Utils.generic_repeat(nb_positions, generate_create_position_data, gen_args, ([]: create_position_data list)) in

    let boundsOverlap(this, others : create_position_data * create_position_data list) : bool = 
        let new_lower_tick = this.cpdLowerTickIndex in
        let new_upper_tick = this.cpdUpperTickIndex in
        let all_indices = List.fold (fun(acc,i: int list * create_position_data) -> i.cpdLowerTickIndex :: i.cpdUpperTickIndex :: acc) others [new_lower_tick; new_upper_tick] in
        let all_indices_nub = Utils.List.nub(all_indices) in
        (List.length all_indices <> List.length all_indices_nub)
        // let all_indices = List.map (fun(i: create_position_data) -> (i.cpdLowerTickIndex, i.cpdUpperTickIndex)) (this :: others) in
        // let all_indices_nub = Utils.List.nub(all_indices) in
        // Utils.List.equal(all_indices_nub, all_indices, cmp)
    in
    let remove_overlapping(acc, elt : create_position_data list * create_position_data) : create_position_data list = 
        if boundsOverlap(elt, acc) then
            acc
        else
            elt :: acc
    in
    List.fold remove_overlapping positions ([]: create_position_data list)

//////////////////////////////////////////////////////////////////////
//                 TESTS                                            //
//////////////////////////////////////////////////////////////////////

// test_position_initialization
let test_position_initialization = 
    let feeBps = 2000n in
    let protoFeeBps = 700n in
    let effectiveProtoFeeBps = if config.y = CTEZ then protoFeeBps else 0n in
    let liquidity = 10_000_000n in 
    let lowerTickIndex = -10000 in
    let upperTickIndex = 10000 in
    
    let swapperBalanceX : nat = 1000000n in
    let swapperBalanceY : nat = 1000000n in
    let accounts = Bootstrap.boot_accounts() in
    let (liquidityProvider, swapper, _swapReceiver, _feeReceiver, _) = accounts in
    // Deploy contracts
    let tokenX = Bootstrap.boot_token_fa12(0n, 0tez, liquidityProvider, liquidity + swapperBalanceX) in
    let tokenY = Bootstrap.boot_token(0n, 0tez, liquidityProvider, liquidity + swapperBalanceY) in
    let cfmm = Bootstrap.boot_cfmm_by_config(config, tokenX.addr, tokenY.addr, 1n, feeBps, effectiveProtoFeeBps, 0tez) in
    // Transfer some (token X and Y) to swapper
    let () = Test.set_source liquidityProvider in
    let () = FA12_helper.transfer_success((liquidityProvider, (swapper, swapperBalanceX)), tokenX.contr) in
    let () = ExtendedFA2_helper.transfer_success([{from_=liquidityProvider; txs=[{to_=swapper; token_id=0n; amount=swapperBalanceY}]}], tokenY.contr) in
    let () = FA12_helper.assert_user_balance(tokenX.taddr, swapper, swapperBalanceX) in
    let () = ExtendedFA2_helper.assert_user_balance(tokenY.taddr, swapper, swapperBalanceY) in
    // update_operators
    let () = Test.set_source liquidityProvider in
    let () = FA12_helper.approve_success((cfmm.addr, liquidity), tokenX.contr) in
    let () = ExtendedFA2_helper.update_operators_success([Add_operator({owner=liquidityProvider; operator=cfmm.addr; token_id=0n})], tokenY.contr) in
    let () = Test.set_source swapper in
    let () = FA12_helper.approve_success((cfmm.addr, swapperBalanceX), tokenX.contr) in
    let () = ExtendedFA2_helper.update_operators_success([Add_operator({owner=swapper; operator=cfmm.addr; token_id=0n})], tokenY.contr) in

    // CHECK INVARIANTS
    let observer = Bootstrap.boot_observe_consumer(0tez) in
    let () = Cfmm_helper.check_all_invariants(cfmm.taddr, cfmm.addr, observer) in

    // -- | Attemps to execute a swap.
    // -- If it fails with `tooBigPriceChangeErr`, try again with a smaller amount.
    let rec attempt_swapX2Y_rec_by_decreasing(amount, caller, receiver : nat * address * address) : unit =
        let () = Test.set_source caller in
        let param : Cfmm.x_to_y_param = Cfmm_helper.generate_x_to_y_param(amount, 0n, receiver) in
        let r = Cfmm_helper.x_to_y(param, 0tez, cfmm.contr) in
        match r with
            | Fail (Rejected (actual,_)) -> 
                if (actual = (Test.eval Cfmm.too_big_price_change_err)) then
                    attempt_swapX2Y_rec_by_decreasing(amount/3n, caller, receiver)
                else
                    let () = Test.log(r) in
                    Test.failwith r
            | Fail (Balance_too_low _err) -> Test.failwith "contract failed: balance too low"
            | Fail (Other s) -> Test.failwith s
            | Success _ -> ()
    in
    let rec attempt_swapY2X_rec_by_decreasing(amount, caller, receiver : nat * address * address) : unit =
        let () = Test.set_source caller in
        let param : Cfmm.y_to_x_param = Cfmm_helper.generate_y_to_x_param(amount, 0n, receiver) in
        let r = Cfmm_helper.y_to_x(param, 0tez, cfmm.contr) in
        match r with
            | Fail (Rejected (actual,_)) -> 
                if (actual = (Test.eval Cfmm.too_big_price_change_err)) then
                    attempt_swapY2X_rec_by_decreasing(amount/3n, caller, receiver)
                else
                    let () = Test.log(r) in
                    Test.failwith r
            | Fail (Balance_too_low _err) -> Test.failwith "contract failed: balance too low"
            | Fail (Other s) -> Test.failwith s
            | Success _ -> ()
    in

    // Create data set
    let datas = genNonOverlappingPositions(5n, lowerTickIndex, upperTickIndex) in
    let data_length = List.length datas in
    let swap_directions = generate_swap_direction(data_length) in

    let process(cpd, swap_dir : create_position_data * bool) : unit =

        // -- Perform a swap to move the tick a bit.
        // -- This ensures the global accumulators (like fee_growth) aren't always 0.
        let initialCfmmBalanceX = FA12_helper.get_user_balance(tokenX.taddr, cfmm.addr) in
        let initialCfmmBalanceY = ExtendedFA2_helper.get_user_balance(tokenY.taddr, cfmm.addr) in
        let swap_amount = if swap_dir then initialCfmmBalanceY / 2n else initialCfmmBalanceX / 2n in
        let swaps : (bool * nat) list = [(swap_dir, swap_amount)] in
        let apply_attempt_swap(elt : (bool * nat)) : unit =
            if elt.0 then
                attempt_swapY2X_rec_by_decreasing(elt.1, swapper, swapper)
            else
                attempt_swapX2Y_rec_by_decreasing(elt.1, swapper, swapper)
        in
        let () = List.iter apply_attempt_swap swaps in

        // CHECK INVARIANTS
        let observer = Bootstrap.boot_observe_consumer(0tez) in
        let () = Cfmm_helper.check_all_invariants(cfmm.taddr, cfmm.addr, observer) in

        let s_init = Test.get_storage cfmm.taddr in
        let initialCfmmBalanceX = FA12_helper.get_user_balance(tokenX.taddr, cfmm.addr) in
        let initialCfmmBalanceY = ExtendedFA2_helper.get_user_balance(tokenY.taddr, cfmm.addr) in

        // SET POSITION by liquidityProvider
        let () = Test.set_source liquidityProvider in
        let param : Cfmm.set_position_param = Cfmm_helper.generate_set_position_param(cpd.cpdLiquidityDelta, (cpd.cpdLowerTickIndex, cpd.cpdUpperTickIndex)) in
        let () = Cfmm_helper.set_position_success(param, 0tez, cfmm.contr) in

        // CHECK INVARIANTS
        // let observer = Bootstrap.boot_observe_consumer(0tez) in
        let () = Cfmm_helper.check_all_invariants(cfmm.taddr, cfmm.addr, observer) in

        let s_after = Test.get_storage cfmm.taddr in

        // -- Ticks were initialized
        let lower_tick = match Big_map.find_opt {i=cpd.cpdLowerTickIndex} s_after.ticks with
        | Some ts -> ts
        | None -> Test.failwith("lower tick not initialized")
        in
        let upper_tick = match Big_map.find_opt {i=cpd.cpdUpperTickIndex} s_after.ticks with
        | Some ts -> ts
        | None -> Test.failwith("upper tick not initialized")
        in

        // -- `sqrtPriceFor` uses floating point math in Haskell, so we lose a lot of precision.
        // -- Therefore, we must accept a +/-1 margin of error.
        let sqrt_price_lower_minus_one = Maths.sqrtPriceFor(cpd.cpdLowerTickIndex - 1, s_after.ladder) in
        let sqrt_price_lower_plus_one = Maths.sqrtPriceFor(cpd.cpdLowerTickIndex + 1, s_after.ladder) in
        let sqrt_price_upper_minus_one = Maths.sqrtPriceFor(cpd.cpdUpperTickIndex - 1, s_after.ladder) in
        let sqrt_price_upper_plus_one = Maths.sqrtPriceFor(cpd.cpdUpperTickIndex + 1, s_after.ladder) in
        let () = assert(sqrt_price_lower_minus_one <= lower_tick.sqrt_price && lower_tick.sqrt_price < sqrt_price_lower_plus_one) in
        let () = assert(sqrt_price_upper_minus_one <= upper_tick.sqrt_price && upper_tick.sqrt_price < sqrt_price_upper_plus_one) in

        let () = assert(lower_tick.liquidity_net = int(cpd.cpdLiquidityDelta)) in
        let () = assert(upper_tick.liquidity_net = 0n - cpd.cpdLiquidityDelta) in
        let () = assert(lower_tick.n_positions = 1n) in
        let () = assert(upper_tick.n_positions = 1n) in
        
        let expected_lower_accumulator = Cfmm_helper.initTickAccumulators(cfmm.addr, observer, s_after, cpd.cpdLowerTickIndex) in
        let s_after = Test.get_storage cfmm.taddr in
        // -- Ticks were initialized
        let lower_tick = match Big_map.find_opt {i=cpd.cpdLowerTickIndex} s_after.ticks with
        | Some ts -> ts
        | None -> Test.failwith("lower tick not initialized")
        in
        // TODO : uncomment asserts
        // let () = assert(int(lower_tick.seconds_outside) = expected_lower_accumulator.seconds) in
        // let () = assert(lower_tick.tick_cumulative_outside = expected_lower_accumulator.tick_cumulative) in
        let () = assert(int(lower_tick.fee_growth_outside.x.x128) = expected_lower_accumulator.fee_growth.x.x128) in
        let () = assert(int(lower_tick.fee_growth_outside.y.x128) = expected_lower_accumulator.fee_growth.y.x128) in
        // let () = assert(int(lower_tick.seconds_per_liquidity_outside.x128) = expected_lower_accumulator.seconds_per_liquidity) in

        let expected_upper_accumulator = Cfmm_helper.initTickAccumulators(cfmm.addr, observer, s_after, cpd.cpdUpperTickIndex) in
        let s_after = Test.get_storage cfmm.taddr in
        let upper_tick = match Big_map.find_opt {i=cpd.cpdUpperTickIndex} s_after.ticks with
        | Some ts -> ts
        | None -> Test.failwith("upper tick not initialized")
        in
        // TODO : uncomment asserts
        // let () = assert(int(upper_tick.seconds_outside) = expected_upper_accumulator.seconds) in
        // let () = assert(upper_tick.tick_cumulative_outside = expected_upper_accumulator.tick_cumulative) in
        let () = assert(int(upper_tick.fee_growth_outside.x.x128) = expected_upper_accumulator.fee_growth.x.x128) in
        let () = assert(int(upper_tick.fee_growth_outside.y.x128) = expected_upper_accumulator.fee_growth.y.x128) in
        // let () = assert(int(upper_tick.seconds_per_liquidity_outside.x128) = expected_upper_accumulator.seconds_per_liquidity) in

        // -- Check global state updates
        let positionIsActive = (cpd.cpdLowerTickIndex <= s_after.cur_tick_index.i && s_after.cur_tick_index.i < cpd.cpdUpperTickIndex) in
        let () = 
            if positionIsActive then
                assert(s_after.liquidity = s_init.liquidity + cpd.cpdLiquidityDelta)
            else
                assert(s_after.liquidity = s_init.liquidity)
        in
        let positionId = s_init.new_position_id in
        let () = assert(s_after.new_position_id = positionId + 1n) in 

        // -- Check position's state
        let position = match Big_map.find_opt positionId s_after.positions with
        | Some p -> p
        | None -> Test.failwith("position not found")
        in
        let () = assert(position.liquidity = cpd.cpdLiquidityDelta) in
        let () = assert(position.owner = liquidityProvider) in
        let () = assert(position.lower_tick_index.i = cpd.cpdLowerTickIndex) in
        let () = assert(position.upper_tick_index.i = cpd.cpdUpperTickIndex) in

        let current_time = Tezos.get_now() in
        let () = Observer_helper.call_observe_success((cfmm.addr, [current_time]), observer.contr) in
        let cumul_buffer = Test.get_storage observer.taddr in

        let expectedAccumulatorInside = Cfmm_helper.tickAccumulatorsInside(s_after, current_time - (0: timestamp), cumul_buffer) ({i=cpd.cpdLowerTickIndex}, {i=cpd.cpdUpperTickIndex}) in
        let expectedFeeGrowthInside = expectedAccumulatorInside.fee_growth in
        let () = assert(position.fee_growth_inside_last.x.x128 = expectedFeeGrowthInside.x.x128) in
        let () = assert(position.fee_growth_inside_last.y.x128 = expectedFeeGrowthInside.y.x128) in

        // -- Check FA2 transfers
        let (delta_x, delta_y) = Maths.liquidityDeltaToTokensDelta(int(cpd.cpdLiquidityDelta), cpd.cpdLowerTickIndex, cpd.cpdUpperTickIndex, s_after.cur_tick_index.i, s_after.sqrt_price, s_after.ladder) in
        let finalCfmmBalanceX = FA12_helper.get_user_balance(tokenX.taddr, cfmm.addr) in
        let finalCfmmBalanceY = ExtendedFA2_helper.get_user_balance(tokenY.taddr, cfmm.addr) in
        let epsilon_x_min = 1 in
        let epsilon_y_min = 1 in
        let epsilon_x_max = 1 in 
        let epsilon_y_max = 1 in 
        let expected_min_bound_cfmm_balance_x = if epsilon_x_min <= initialCfmmBalanceX + delta_x then initialCfmmBalanceX + delta_x - epsilon_x_min else 0 in
        let expected_min_bound_cfmm_balance_y = if epsilon_y_min <= initialCfmmBalanceY + delta_y then initialCfmmBalanceY + delta_y - epsilon_y_min else 0 in
        let () = assert(expected_min_bound_cfmm_balance_x <= int(finalCfmmBalanceX) && int(finalCfmmBalanceX) <= initialCfmmBalanceX + delta_x + epsilon_x_max) in
        let () = assert(expected_min_bound_cfmm_balance_y <= int(finalCfmmBalanceY) && int(finalCfmmBalanceY) <= initialCfmmBalanceY + delta_y + epsilon_y_max) in
        ()
    in
    let _ret = Utils.List.zipWith process datas swap_directions in
    ()
