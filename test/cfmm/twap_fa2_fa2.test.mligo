#import "../helpers/assert.mligo" "Assert"
#import "../bootstrap/bootstrap.mligo" "Bootstrap"
#import "../helpers/log.mligo" "Log"
#import "../helpers/cfmm.mligo" "Cfmm_helper"
#import "../../lib/cfmm/main.mligo" "Cfmm"
#import "../helpers/extended_fa2.mligo" "ExtendedFA2_helper"
#import "../helpers/maths.mligo" "Maths"
#import "../helpers/utils.mligo" "Utils"
#import "../helpers/config.mligo" "Config_helper"
#import "../helpers/observe_consumer.mligo" "Observer_helper"


let () = Log.describe("[FA2-FA2] [Cfmm.twap] test suite")

let config = ({x=FA2; y=FA2} : Config_helper.config)

// test_twap_simple
let test_twap_simple = 
    let feeBps = 2000n in
    let protoFeeBps = 700n in
    let effectiveProtoFeeBps = if config.y = CTEZ then protoFeeBps else 0n in
    let liquidity = 10_000_000n in 
    let lowerTickIndex = -10000 in
    let upperTickIndex = 10000 in
    
    let swapperBalanceX : nat = 1000000n in
    let swapperBalanceY : nat = 1000000n in
    let accounts = Bootstrap.boot_accounts() in
    let (liquidityProvider, swapper, swapReceiver, _feeReceiver, _) = accounts in
    // Deploy contracts
    let tokenX = Bootstrap.boot_token(0n, 0tez, liquidityProvider, liquidity + swapperBalanceX) in
    let tokenY = Bootstrap.boot_token(0n, 0tez, liquidityProvider, liquidity + swapperBalanceY) in
    let cfmm = Bootstrap.boot_cfmm_by_config(config, tokenX.addr, tokenY.addr, 1n, feeBps, effectiveProtoFeeBps, 0tez) in
    // Transfer some (token X and Y) to swapper
    let () = Test.set_source liquidityProvider in
    let () = ExtendedFA2_helper.transfer_success([{from_=liquidityProvider; txs=[{to_=swapper; token_id=0n; amount=swapperBalanceX}]}], tokenX.contr) in
    let () = ExtendedFA2_helper.transfer_success([{from_=liquidityProvider; txs=[{to_=swapper; token_id=0n; amount=swapperBalanceY}]}], tokenY.contr) in
    let () = ExtendedFA2_helper.assert_user_balance(tokenX.taddr, swapper, swapperBalanceX) in
    let () = ExtendedFA2_helper.assert_user_balance(tokenY.taddr, swapper, swapperBalanceY) in
    // update_operators
    let () = Test.set_source liquidityProvider in
    let () = ExtendedFA2_helper.update_operators_success([Add_operator({owner=liquidityProvider; operator=cfmm.addr; token_id=0n})], tokenX.contr) in
    let () = ExtendedFA2_helper.update_operators_success([Add_operator({owner=liquidityProvider; operator=cfmm.addr; token_id=0n})], tokenY.contr) in
    let () = Test.set_source swapper in
    let () = ExtendedFA2_helper.update_operators_success([Add_operator({owner=swapper; operator=cfmm.addr; token_id=0n})], tokenX.contr) in
    let () = ExtendedFA2_helper.update_operators_success([Add_operator({owner=swapper; operator=cfmm.addr; token_id=0n})], tokenY.contr) in

    // CHECK INVARIANTS
    let observer = Bootstrap.boot_observe_consumer(0tez) in
    // let () = Cfmm_helper.check_all_invariants(cfmm.taddr, cfmm.addr, observer) in

    let t_before_set_position = Tezos.get_now() in 
    let liquidity_delta = 1_000_000n in 
    // SET POSITION by liquidityProvider
    let () = Test.log("SET_POSITION") in
    let () = Test.set_source liquidityProvider in
    let param : Cfmm.set_position_param = Cfmm_helper.generate_set_position_param(liquidity_delta, (lowerTickIndex, upperTickIndex)) in
    let () = Cfmm_helper.set_position_success(param, 0tez, cfmm.contr) in

    // DEBUG
    let s_after = Test.get_storage cfmm.taddr in
    // let () = Test.log(s_after.cumulatives_buffer) in
    let lower_tick = match Big_map.find_opt {i=lowerTickIndex} s_after.ticks with
    | Some ts -> ts
    | None -> Test.failwith("lower tick not initialized")
    in
    let lowerCumulativesOutside = lower_tick.tick_cumulative_outside in
    let upper_tick = match Big_map.find_opt {i=upperTickIndex} s_after.ticks with
    | Some ts -> ts
    | None -> Test.failwith("lower tick not initialized")
    in
    let upperCumulativesOutside = upper_tick.tick_cumulative_outside in
    let () = Test.log("lowerCumulativesOutside", lowerCumulativesOutside) in
    let () = Test.log("upperCumulativesOutside", upperCumulativesOutside) in

    let t_before_swap = Tezos.get_now() in 
    let () = Test.log(t_before_swap - (0: timestamp)) in
    // SWAP X
    let () = Test.log("SWAP") in
    let () = Test.set_source swapper in
    let param : Cfmm.x_to_y_param = Cfmm_helper.generate_x_to_y_param(1000n, 0n, swapReceiver) in
    let r = Cfmm_helper.x_to_y(param, 0tez, cfmm.contr) in
    let () = Test.log(r) in
    let duration = t_before_swap - t_before_set_position in 
    let () = Test.log("duration", duration) in

   // DEBUG
    let s_after = Test.get_storage cfmm.taddr in
    // let () = Test.log(s_after.cumulatives_buffer) in
    let lower_tick = match Big_map.find_opt {i=lowerTickIndex} s_after.ticks with
    | Some ts -> ts
    | None -> Test.failwith("lower tick not initialized")
    in
    let lowerCumulativesOutside = lower_tick.tick_cumulative_outside in
    let upper_tick = match Big_map.find_opt {i=upperTickIndex} s_after.ticks with
    | Some ts -> ts
    | None -> Test.failwith("lower tick not initialized")
    in
    let upperCumulativesOutside = upper_tick.tick_cumulative_outside in
    let () = Test.log("lowerCumulativesOutside", lowerCumulativesOutside) in
    let () = Test.log("upperCumulativesOutside", upperCumulativesOutside) in

    let spl_x128 = (Bitwise.shift_left (abs(duration)) 128n) / liquidity_delta in
    let () = Test.log("spl_x128", spl_x128) in


    // // CHECK INVARIANTS
    // let observer = Bootstrap.boot_observe_consumer(0tez) in
    // let () = Cfmm_helper.check_all_invariants(cfmm.taddr, cfmm.addr, observer) in

    // OBSERVE
    let t_before_observe = Tezos.get_now() in 
    let s_after = Test.get_storage cfmm.taddr in
    let expected_lower_accumulator = Cfmm_helper.initTickAccumulators(cfmm.addr, observer, s_after, lowerTickIndex) in
    let () = Test.log("expected_lower_accumulator", expected_lower_accumulator) in

    // DEBUG
    let s_after = Test.get_storage cfmm.taddr in
    // let () = Test.log(s_after.cumulatives_buffer) in
    let lower_tick = match Big_map.find_opt {i=lowerTickIndex} s_after.ticks with
    | Some ts -> ts
    | None -> Test.failwith("lower tick not initialized")
    in
    let lowerCumulativesOutside = lower_tick.tick_cumulative_outside in
    let upper_tick = match Big_map.find_opt {i=upperTickIndex} s_after.ticks with
    | Some ts -> ts
    | None -> Test.failwith("lower tick not initialized")
    in
    let upperCumulativesOutside = upper_tick.tick_cumulative_outside in
    let () = Test.log("lowerCumulativesOutside", lowerCumulativesOutside) in
    let () = Test.log("upperCumulativesOutside", upperCumulativesOutside) in

    let spl_x128 = (Bitwise.shift_left (abs(duration)) 128n) / liquidity_delta in
    let () = Test.log("spl_x128", spl_x128) in

    let duration_obs_swap = t_before_observe - t_before_swap in
        let () = Test.log("duration_obs_swap", duration_obs_swap) in
    let spl_obs_swap_x128 = (Bitwise.shift_left (abs(duration_obs_swap)) 128n) / liquidity_delta in
    let () = Test.log("spl_obs_swap_x128", spl_obs_swap_x128) in

    let total_spl = spl_x128 + spl_obs_swap_x128 in
    let () = Test.log("total_spl", total_spl) in

    // let result = total / (duration_obs_swap + duration) in
    // let () = Test.log("result", result) in
    // let temp = (Bitwise.shift_right (total_spl * liquidity_delta)  128n) in
    // let () = Test.log("temp", temp) in
    // let res = (Bitwise.shift_right (total_spl * liquidity_delta)  128n) / (duration_obs_swap + duration) in
    // let () = Test.log("res", res) in

    // RECOMPUTE PRICE
    // tick = exp(0.0001) ^ ( (tick_cumulative_t2 - tick_cumulative_t1) / (t2 - t1) )
    let duration_total = t_before_observe - t_before_set_position in
    let () = assert(duration_total > 0) in
    let tick = if expected_lower_accumulator.tick_cumulative >= 0 then
        (expected_lower_accumulator.tick_cumulative - 0) / duration_total
    else
        0 - abs(expected_lower_accumulator.tick_cumulative - 0) / abs(duration_total)
    in
    // let tick2 = if expected_lower_accumulator.tick_cumulative > 0 then
    //     int((Bitwise.shift_left (abs(expected_lower_accumulator.tick_cumulative)) 128n) / (liquidity_delta * total_spl))
    // else 
    //     0 - (Bitwise.shift_left (abs(expected_lower_accumulator.tick_cumulative)) 128n) / (liquidity_delta * total_spl) 
    // in
    let () = Test.log("tick", tick) in
    let () = assert(tick = -9) in   // verify
    // let () = Test.log("tick2", tick2) in
    let price_x80 = Cfmm.half_bps_pow(tick, s_after.ladder) in
    // let () = Test.log("price_x80", price_x80) in
    let precision = 1000000n in
    let final_sqrt_price = Bitwise.shift_right (precision * price_x80.x80) 80n in
    // let () = Test.log("sqrt_price (1/1000000)", final_sqrt_price) in
    let final_price = final_sqrt_price * final_sqrt_price / precision in
    let () = Test.log("price (1/1000000)", final_price) in
   
    ()



// test_twap_simple (SET ->  SWAP -> OBS)
let test_twap_2 = 
    let () = Test.log(" ////////////////////////////////// test_twap_2 ////////////////////////////////// ") in
    let feeBps = 2000n in
    let protoFeeBps = 700n in
    let effectiveProtoFeeBps = if config.y = CTEZ then protoFeeBps else 0n in
    let liquidity = 10_000_000n in 
    let lowerTickIndex = -10000 in
    let upperTickIndex = 10000 in
    
    let swapperBalanceX : nat = 1000000n in
    let swapperBalanceY : nat = 1000000n in
    let accounts = Bootstrap.boot_accounts() in
    let (liquidityProvider, swapper, swapReceiver, _feeReceiver, _) = accounts in
    // Deploy contracts
    let tokenX = Bootstrap.boot_token(0n, 0tez, liquidityProvider, liquidity + swapperBalanceX) in
    let tokenY = Bootstrap.boot_token(0n, 0tez, liquidityProvider, liquidity + swapperBalanceY) in
    let cfmm = Bootstrap.boot_cfmm_by_config(config, tokenX.addr, tokenY.addr, 1n, feeBps, effectiveProtoFeeBps, 0tez) in
    // Transfer some (token X and Y) to swapper
    let () = Test.set_source liquidityProvider in
    let () = ExtendedFA2_helper.transfer_success([{from_=liquidityProvider; txs=[{to_=swapper; token_id=0n; amount=swapperBalanceX}]}], tokenX.contr) in
    let () = ExtendedFA2_helper.transfer_success([{from_=liquidityProvider; txs=[{to_=swapper; token_id=0n; amount=swapperBalanceY}]}], tokenY.contr) in
    let () = ExtendedFA2_helper.assert_user_balance(tokenX.taddr, swapper, swapperBalanceX) in
    let () = ExtendedFA2_helper.assert_user_balance(tokenY.taddr, swapper, swapperBalanceY) in
    // update_operators
    let () = Test.set_source liquidityProvider in
    let () = ExtendedFA2_helper.update_operators_success([Add_operator({owner=liquidityProvider; operator=cfmm.addr; token_id=0n})], tokenX.contr) in
    let () = ExtendedFA2_helper.update_operators_success([Add_operator({owner=liquidityProvider; operator=cfmm.addr; token_id=0n})], tokenY.contr) in
    let () = Test.set_source swapper in
    let () = ExtendedFA2_helper.update_operators_success([Add_operator({owner=swapper; operator=cfmm.addr; token_id=0n})], tokenX.contr) in
    let () = ExtendedFA2_helper.update_operators_success([Add_operator({owner=swapper; operator=cfmm.addr; token_id=0n})], tokenY.contr) in

    // CHECK INVARIANTS
    let observer = Bootstrap.boot_observe_consumer(0tez) in
    // let () = Cfmm_helper.check_all_invariants(cfmm.taddr, cfmm.addr, observer) in

    let t_before_set_position = Tezos.get_now() in 
    let liquidity_delta = 1_000_000n in 
    // SET POSITION by liquidityProvider
    let () = Test.log("SET_POSITION") in
    let () = Test.set_source liquidityProvider in
    let param : Cfmm.set_position_param = Cfmm_helper.generate_set_position_param(liquidity_delta, (lowerTickIndex, upperTickIndex)) in
    let () = Cfmm_helper.set_position_success(param, 0tez, cfmm.contr) in

    // DEBUG
    let s_after = Test.get_storage cfmm.taddr in
    let () = Test.log("CUMUL after set_position", s_after.cumulatives_buffer) in
    let lower_tick = match Big_map.find_opt {i=lowerTickIndex} s_after.ticks with
    | Some ts -> ts
    | None -> Test.failwith("lower tick not initialized")
    in
    let lowerCumulativesOutside = lower_tick.tick_cumulative_outside in
    let upper_tick = match Big_map.find_opt {i=upperTickIndex} s_after.ticks with
    | Some ts -> ts
    | None -> Test.failwith("lower tick not initialized")
    in
    let upperCumulativesOutside = upper_tick.tick_cumulative_outside in
    let () = Test.log("lowerCumulativesOutside", lowerCumulativesOutside) in
    let () = Test.log("upperCumulativesOutside", upperCumulativesOutside) in

    let t_before_swap = Tezos.get_now() in 
    let () = Test.log(t_before_swap - (0: timestamp)) in
    // SWAP X
    let () = Test.log("SWAP") in
    let () = Test.set_source swapper in
    let param : Cfmm.x_to_y_param = Cfmm_helper.generate_x_to_y_param(500000n, 0n, swapReceiver) in
    let r = Cfmm_helper.x_to_y(param, 0tez, cfmm.contr) in
    let () = Test.log(r) in
    let duration = t_before_swap - t_before_set_position in 
    let () = Test.log("duration", duration) in

   // DEBUG
    let s_after = Test.get_storage cfmm.taddr in
    let () = Test.log("CUMUL after swap", s_after.cumulatives_buffer) in
    let lower_tick = match Big_map.find_opt {i=lowerTickIndex} s_after.ticks with
    | Some ts -> ts
    | None -> Test.failwith("lower tick not initialized")
    in
    let lowerCumulativesOutside = lower_tick.tick_cumulative_outside in
    let upper_tick = match Big_map.find_opt {i=upperTickIndex} s_after.ticks with
    | Some ts -> ts
    | None -> Test.failwith("lower tick not initialized")
    in
    let upperCumulativesOutside = upper_tick.tick_cumulative_outside in
    let () = Test.log("lowerCumulativesOutside", lowerCumulativesOutside) in
    let () = Test.log("upperCumulativesOutside", upperCumulativesOutside) in

    let spl_x128 = (Bitwise.shift_left (abs(duration)) 128n) / liquidity_delta in
    let () = Test.log("spl_x128", spl_x128) in

    // SET POSITION
    let _t_before_pos2 = Tezos.get_now() in 
    let () = Test.log("SET_POSITION 2") in
    let () = Test.set_source liquidityProvider in
    let param : Cfmm.set_position_param = Cfmm_helper.generate_set_position_param(liquidity_delta, (lowerTickIndex, upperTickIndex)) in
    let () = Cfmm_helper.set_position_success(param, 0tez, cfmm.contr) in

   // DEBUG
    let s_after = Test.get_storage cfmm.taddr in
    let () = Test.log("CUMUL after pos2", s_after.cumulatives_buffer) in

    // OBSERVE
    let t_before_observe = Tezos.get_now() in 
    let s_after = Test.get_storage cfmm.taddr in
    let expected_lower_accumulator = Cfmm_helper.initTickAccumulators(cfmm.addr, observer, s_after, lowerTickIndex) in
    let () = Test.log("expected_lower_accumulator", expected_lower_accumulator) in

    // DEBUG
    let s_after = Test.get_storage cfmm.taddr in
    // let () = Test.log(s_after.cumulatives_buffer) in
    let () = Test.log("CUMUL after observe", s_after.cumulatives_buffer) in

    // RECOMPUTE PRICE
    // exp(0.0001) ^ ( (tick_cumulative_t2 - tick_cumulative_t1) / (t2 - t1) )
    let duration_total = t_before_observe - t_before_set_position in
    let () = assert(duration_total > 0) in
    let tick = if expected_lower_accumulator.tick_cumulative >= 0 then
        (expected_lower_accumulator.tick_cumulative - 0) / duration_total
    else
        0 - abs(expected_lower_accumulator.tick_cumulative - 0) / abs(duration_total)
    in
    let () = Test.log("tick", tick) in
    let price_x80 = Cfmm.half_bps_pow(tick, s_after.ladder) in
    // let () = Test.log("price_x80", price_x80) in
    let precision = 1000000n in
    let final_sqrt_price = Bitwise.shift_right (precision * price_x80.x80) 80n in
    // let () = Test.log("sqrt_price (1/1000000)", final_sqrt_price) in
    let final_price = final_sqrt_price * final_sqrt_price / precision in
    let () = Test.log("price (1/1000000)", final_price) in


    ()
