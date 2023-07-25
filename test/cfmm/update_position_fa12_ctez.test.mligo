#import "../helpers/assert.mligo" "Assert"
#import "../bootstrap/bootstrap.mligo" "Bootstrap"
#import "../helpers/log.mligo" "Log"
#import "../helpers/cfmm.mligo" "Cfmm_helper"
#import "../../lib/cfmm/main.mligo" "Cfmm"
#import "../helpers/fa12.mligo" "FA12_helper"
#import "../helpers/maths.mligo" "Maths"
#import "../helpers/utils.mligo" "Utils"
#import "../helpers/config.mligo" "Config_helper"

let () = Log.describe("[FA12-CTEZ] [Cfmm.update_position] test suite")

let config = ({x=FA12; y=CTEZ} : Config_helper.config)

let generate_cumulativesBuffer1 (tt: timestamp) : Cfmm.timed_cumulatives_buffer =
  let initVal = Cfmm.init_cumulatives_buffer 0n in
  let initTimedCumul : Cfmm.timed_cumulatives = Cfmm.init_timed_cumulatives in 
  let initTimedCumul = { initTimedCumul with time = tt } in
  { initVal with 
    map = Big_map.literal([
        (1n, initTimedCumul);
    ]);
    first = 1n;
    last = 1n;
}

(* Successful Set_position  + Update_position *)
// test_deposit_and_withdrawal_is_a_noop
let test_success_set_and_update =
    let accounts = Bootstrap.boot_accounts() in
    let (user1, _user2, _, _, _) = accounts in
    let tokenX = Bootstrap.boot_token_fa12(0n, 0tez, user1, 1000000n) in
    let tokenY = Bootstrap.boot_token_fa12(0n, 0tez, user1, 1000000n) in
    let cfmm = Bootstrap.boot_cfmm_by_config(config, tokenX.addr, tokenY.addr, 1n, 10n, 10n, 0tez) in
    let () = Test.set_source user1 in
    let () = FA12_helper.approve_success((cfmm.addr, 1000000n), tokenX.contr) in
    let () = FA12_helper.approve_success((cfmm.addr, 1000000n), tokenY.contr) in
    let s_initial = Test.get_storage cfmm.taddr in

    // SET POSITION 1000000
    let param : Cfmm.set_position_param = Cfmm_helper.generate_set_position_param(1000000n, (-15, 10)) in
    let () = Cfmm_helper.set_position_success(param, 0tez, cfmm.contr) in
    // UPDATE POSITION -1000000
    let param_update : Cfmm.update_position_param = Cfmm_helper.generate_update_position_param(0n, -1000000, user1, 500n, 750n) in
    let () = Cfmm_helper.update_position_success(param_update, 0tez, cfmm.contr) in
    
    // Verify Storage
    let s_after = Test.get_storage cfmm.taddr in
    let buffer_1 = generate_cumulativesBuffer1 (Tezos.get_now()) in
    let s_initial_modified = { s_initial with new_position_id=s_initial.new_position_id+1n; cumulatives_buffer=buffer_1 } in
    let () = Cfmm_helper.assert_storage_equal(s_initial_modified, s_after) in
    // let () = Cfmm_helper.assert_liquidity(cfmm.taddr, 0n) in

    // The contract's balance should be 0.
    // There is a margin of error, so the contract may end up with at most 1 token.
    let () = FA12_helper.assert_user_balance_in_range(tokenX.taddr, user1, 1000000n, 1n) in 
    let () = FA12_helper.assert_user_balance_in_range(tokenX.taddr, cfmm.addr, 0n, 1n) in 
    let () = FA12_helper.assert_user_balance_in_range(tokenY.taddr, user1, 1000000n, 1n) in 
    FA12_helper.assert_user_balance_in_range(tokenY.taddr, cfmm.addr, 0n, 1n) 

let test_failure_update_position_with_invalid_deadline =
    let accounts = Bootstrap.boot_accounts() in
    let (user1, _user2, _, _, _) = accounts in
    let tokenX = Bootstrap.boot_token_fa12(0n, 0tez, user1, 1000000n) in
    let tokenY = Bootstrap.boot_token_fa12(0n, 0tez, user1, 1000000n) in
    let cfmm = Bootstrap.boot_cfmm_by_config(config, tokenX.addr, tokenY.addr, 1n, 10n, 10n, 0tez) in
    let () = Test.set_source user1 in
    let () = FA12_helper.approve_success((cfmm.addr, 1000000n), tokenX.contr) in
    let () = FA12_helper.approve_success((cfmm.addr, 1000000n), tokenY.contr) in

    let param : Cfmm.set_position_param = Cfmm_helper.generate_set_position_param(1000000n, (-15, 10)) in
    let () = Cfmm_helper.set_position_success(param, 0tez, cfmm.contr) in
    let param_update : Cfmm.update_position_param = Cfmm_helper.generate_update_position_param(0n, -1000000, user1, 500n, 750n) in
    let param_update = { param_update with deadline=Tezos.get_now() - 1 } in
    let r = Cfmm_helper.update_position(param_update, 0tez, cfmm.contr) in
    Assert.deadline_failure r Cfmm.past_deadline_err

// test_adding_liquidity_twice
let test_success_adding_twice =
    let accounts = Bootstrap.boot_accounts() in
    let (user1, _user2, _, _, _) = accounts in
    let tokenX = Bootstrap.boot_token_fa12(0n, 0tez, user1, 10000000n) in
    let tokenY = Bootstrap.boot_token_fa12(0n, 0tez, user1, 10000000n) in

    let cfmm1 = Bootstrap.boot_cfmm_by_config(config, tokenX.addr, tokenY.addr, 1n, 10n, 10n, 0tez) in
    let cfmm2 = Bootstrap.boot_cfmm_by_config(config, tokenX.addr, tokenY.addr, 1n, 10n, 10n, 0tez) in

    let () = Test.set_source user1 in
    let () = FA12_helper.approve_success((cfmm1.addr, 1000000n), tokenX.contr) in
    let () = FA12_helper.approve_success((cfmm1.addr, 1000000n), tokenY.contr) in
    let () = FA12_helper.approve_success((cfmm2.addr, 1000000n), tokenX.contr) in
    let () = FA12_helper.approve_success((cfmm2.addr, 1000000n), tokenY.contr) in

    // CFMM1 : set_position with 1000000 and update_position with 1000000
    let liquidity_delta = 1000000n in 
    let time_before_cfmm1_set = Tezos.get_now() in
    let param : Cfmm.set_position_param = Cfmm_helper.generate_set_position_param(liquidity_delta, (-15, 10)) in
    let () = Cfmm_helper.set_position_success(param, 0tez, cfmm1.contr) in
    let time_before_cfmm1_update = Tezos.get_now() in
    let param_update : Cfmm.update_position_param = Cfmm_helper.generate_update_position_param(0n, int(liquidity_delta), user1, 500n, 750n) in
    let () = Cfmm_helper.update_position_success(param_update, 0tez, cfmm1.contr) in
    // CFMM2 : set_position with 2000000
    let param : Cfmm.set_position_param = Cfmm_helper.generate_set_position_param(2n * liquidity_delta, (-15, 10)) in
    let () = Cfmm_helper.set_position_success(param, 0tez, cfmm2.contr) in
    
    let s_cfmm1 = Test.get_storage cfmm1.taddr in
    let s_cfmm2 = Test.get_storage cfmm2.taddr in


    // Verify Storage
    let () = Cfmm_helper.assert_storage_equal(s_cfmm1, s_cfmm2) in

    //  (Test framework cannot batch transactions so cumulative buffers are different)
    // let () = assert(s_cfmm1.cumulatives_buffer = s_cfmm2.cumulatives_buffer) in 

    // Verify CFMM1 Cumulative Buffer (spl.sum and spl.block_start_liquidity_value)
    let time_elapsed = abs(time_before_cfmm1_update - time_before_cfmm1_set) in
    let expected_spl_sum = (Bitwise.shift_left time_elapsed 128n) / liquidity_delta in
    let cfmm1_spl = match Big_map.find_opt s_cfmm1.cumulatives_buffer.first s_cfmm1.cumulatives_buffer.map with
    | Some v -> v
    | None -> failwith "failed to parse cumulative buffer"
    in
    let () = assert(expected_spl_sum = cfmm1_spl.spl.sum.x128) in
    let () = assert(liquidity_delta = cfmm1_spl.spl.block_start_liquidity_value) in

    let cfmm_1_balance_x = FA12_helper.get_user_balance(tokenX.taddr, cfmm1.addr) in
    let () = FA12_helper.assert_user_balance_in_range(tokenX.taddr, cfmm2.addr, cfmm_1_balance_x, 1n) in
    let cfmm_1_balance_y = FA12_helper.get_user_balance(tokenY.taddr, cfmm1.addr) in
    let () = FA12_helper.assert_user_balance_in_range(tokenY.taddr, cfmm2.addr, cfmm_1_balance_y, 1n) in
    ()


// test_maximum_tokens_contributed
let test_failure_set_and_update_lower_max_token_contributed =
    let accounts = Bootstrap.boot_accounts() in
    let (user1, _user2, _, _, _) = accounts in
    let tokenX = Bootstrap.boot_token_fa12(0n, 0tez, user1, 1000000n) in
    let tokenY = Bootstrap.boot_token_fa12(0n, 0tez, user1, 1000000n) in
    let cfmm = Bootstrap.boot_cfmm_by_config(config, tokenX.addr, tokenY.addr, 1n, 10n, 10n, 0tez) in

    let () = Test.set_source user1 in
    let () = FA12_helper.approve_success((cfmm.addr, 1000000n), tokenX.contr) in
    let () = FA12_helper.approve_success((cfmm.addr, 1000000n), tokenY.contr) in

    let param : Cfmm.set_position_param = Cfmm_helper.generate_set_position_param(1000n, (-15, 10)) in
    let () = Cfmm_helper.set_position_success(param, 0tez, cfmm.contr) in
    let param_update : Cfmm.update_position_param = Cfmm_helper.generate_update_position_param(0n, 500000, user1, 500n, 750n) in
    let param_update = { param_update with maximum_tokens_contributed = {x=1n; y=1n } } in
    let r = Cfmm_helper.update_position(param_update, 0tez, cfmm.contr) in
    Assert.maximum_tokens_contributed_failure r Cfmm.high_tokens_err


// test_lowest_and_highest_ticks_cannot_be_garbage_collected
let test_lowest_and_highest_ticks_cannot_be_garbage_collected =
    let accounts = Bootstrap.boot_accounts() in
    let (user1, _user2, _, _, _) = accounts in
    let tokenX = Bootstrap.boot_token_fa12(0n, 0tez, user1, 10000000n) in
    let tokenY = Bootstrap.boot_token_fa12(0n, 0tez, user1, 10000000n) in
    let cfmm = Bootstrap.boot_cfmm_by_config(config, tokenX.addr, tokenY.addr, 1n, 10n, 10n, 0tez) in
    let () = Test.set_source user1 in
    let () = FA12_helper.approve_success((cfmm.addr, 1000000n), tokenX.contr) in
    let () = FA12_helper.approve_success((cfmm.addr, 1000000n), tokenY.contr) in

    let s_init = Test.get_storage cfmm.taddr in
    // cfmm : set_position with 1 and update_position with -1
    let param : Cfmm.set_position_param = Cfmm_helper.generate_set_position_param(1n, (Cfmm_helper.minTickIndex, Cfmm_helper.maxTickIndex)) in
    let () = Cfmm_helper.set_position_success(param, 0tez, cfmm.contr) in
    let param_update : Cfmm.update_position_param = Cfmm_helper.generate_update_position_param(0n, -1, user1, 500n, 750n) in
    let () = Cfmm_helper.update_position_success(param_update, 0tez, cfmm.contr) in

    let s_after = Test.get_storage cfmm.taddr in
    let buffer_1 = generate_cumulativesBuffer1 (Tezos.get_now()) in
    let s_test = { s_init with new_position_id=s_init.new_position_id + 1n; cumulatives_buffer=buffer_1 } in
    // let () = assert(s_test.cumulatives_buffer = s_after.cumulatives_buffer) in 
    Cfmm_helper.assert_storage_equal(s_after, s_test)


// test_withdrawal_overflow
let test_withdrawal_overflow = 
    let accounts = Bootstrap.boot_accounts() in
    let (user1, user2, _, _, _) = accounts in
    let tokenX = Bootstrap.boot_token_fa12(0n, 0tez, user1, 10000000n) in
    let tokenY = Bootstrap.boot_token_fa12(0n, 0tez, user1, 10000000n) in
    let cfmm = Bootstrap.boot_cfmm_by_config(config, tokenX.addr, tokenY.addr, 1n, 10n, 10n, 0tez) in
    //let s_init = Test.get_storage cfmm.taddr in
    let () = Test.set_source user1 in
    // Transfer 5_000_000 to user2
    let () = FA12_helper.transfer_success((user1, (user2, 5000000n)), tokenX.contr) in
    let () = FA12_helper.transfer_success((user1, (user2, 5000000n)), tokenY.contr) in
    // update_operators
    let () = FA12_helper.approve_success((cfmm.addr, 5000000n), tokenX.contr) in
    let () = FA12_helper.approve_success((cfmm.addr, 5000000n), tokenY.contr) in
    let () = Test.set_source user2 in
    let () = FA12_helper.approve_success((cfmm.addr, 5000000n), tokenX.contr) in
    let () = FA12_helper.approve_success((cfmm.addr, 5000000n), tokenY.contr) in

    // cfmm : set_position with 1 and update_position with -1
    let () = Test.set_source user1 in
    let param : Cfmm.set_position_param = Cfmm_helper.generate_set_position_param(1000000n, (-15, 10)) in
    let () = Cfmm_helper.set_position_success(param, 0tez, cfmm.contr) in

    let () = Test.set_source user2 in
    let param : Cfmm.set_position_param = Cfmm_helper.generate_set_position_param(1000000n, (-15, 10)) in
    let () = Cfmm_helper.set_position_success(param, 0tez, cfmm.contr) in

    let param_update : Cfmm.update_position_param = Cfmm_helper.generate_update_position_param(1n, -1000001, user2, 500n, 750n) in
    let r = Cfmm_helper.update_position(param_update, 0tez, cfmm.contr) in
    Assert.nat_failure r Cfmm.position_liquidity_below_zero_err


// test_LPs_get_fees
let test_LPs_get_fees = 
    let feeBps = 2000n in
    let protoFeeBps = 700n in
    let effectiveProtoFeeBps = if config.y = CTEZ then protoFeeBps else 0n in
    let liquidity = 10_000_000n in 
    let lowerTickIndex = -10000 in
    let upperTickIndex = 10000 in
    
    let swapperBalanceX : nat = 1400n in
    let swapperBalanceY : nat = 3000n in
    let accounts = Bootstrap.boot_accounts() in
    let (liquidityProvider, swapper, swapReceiver, feeReceiver, _) = accounts in
    // Deploy contracts
    let tokenX = Bootstrap.boot_token_fa12(0n, 0tez, liquidityProvider, liquidity + swapperBalanceX) in
    let tokenY = Bootstrap.boot_token_fa12(0n, 0tez, liquidityProvider, liquidity + swapperBalanceY) in
    let cfmm = Bootstrap.boot_cfmm_by_config(config, tokenX.addr, tokenY.addr, 1n, feeBps, effectiveProtoFeeBps, 0tez) in
    // Transfer some (token X and Y) to swapper
    let () = Test.set_source liquidityProvider in
    let () = FA12_helper.transfer_success((liquidityProvider, (swapper, swapperBalanceX)), tokenX.contr) in
    let () = FA12_helper.transfer_success((liquidityProvider, (swapper, swapperBalanceY)), tokenY.contr) in
    let () = FA12_helper.assert_user_balance(tokenX.taddr, swapper, swapperBalanceX) in
    let () = FA12_helper.assert_user_balance(tokenY.taddr, swapper, swapperBalanceY) in
    // update_operators
    let () = Test.set_source liquidityProvider in
    let () = FA12_helper.approve_success((cfmm.addr, liquidity), tokenX.contr) in
    let () = FA12_helper.approve_success((cfmm.addr, liquidity), tokenY.contr) in
    let () = Test.set_source swapper in
    let () = FA12_helper.approve_success((cfmm.addr, swapperBalanceX), tokenX.contr) in
    let () = FA12_helper.approve_success((cfmm.addr, swapperBalanceY), tokenY.contr) in

    // SET POSITION
    let () = Test.set_source liquidityProvider in
    let param : Cfmm.set_position_param = Cfmm_helper.generate_set_position_param(liquidity, (lowerTickIndex, upperTickIndex)) in
    let () = Cfmm_helper.set_position_success(param, 0tez, cfmm.contr) in
    let () = Cfmm_helper.assert_liquidity(cfmm.taddr, liquidity) in

    let () = FA12_helper.assert_user_balance(tokenX.taddr, swapReceiver, 0n) in
    let () = FA12_helper.assert_user_balance(tokenY.taddr, swapReceiver, 0n) in
    let () = FA12_helper.assert_user_balance(tokenX.taddr, swapper, swapperBalanceX) in
    let () = FA12_helper.assert_user_balance(tokenY.taddr, swapper, swapperBalanceY) in

    // SWAP 1000 X , SWAP 3000 Y, SWAP 400 X
    // Apply multi SWAPS , compute swap fees and sum fees for all swaps
    let () = Test.set_source swapper in
    let swaps : (bool * nat) list = [(false, 1000n); (true, 3000n); (false, 400n)] in
    let apply_swap_sum_fees(acc, elt : (nat * nat) * (bool * nat)) : (nat * nat) =
        if elt.0 then
            let param : Cfmm.y_to_x_param = Cfmm_helper.generate_y_to_x_param(elt.1, 0n, swapReceiver) in
            let () = Cfmm_helper.y_to_x_success(param, 0tez, cfmm.contr) in
            (acc.0, acc.1 + Maths.calcSwapFee(feeBps, elt.1))
        else
            let param : Cfmm.x_to_y_param = Cfmm_helper.generate_x_to_y_param(elt.1, 0n, swapReceiver) in
            let () = Cfmm_helper.x_to_y_success(param, 0tez, cfmm.contr) in
            (acc.0 + Maths.calcSwapFee(feeBps, elt.1), acc.1) 
    in
    let (fee_sum_x, fee_sum_y) = List.fold apply_swap_sum_fees swaps (0n, 0n) in

    let () = Cfmm_helper.collectFees(liquidityProvider, 0n, feeReceiver, cfmm.contr) in
    let () = FA12_helper.assert_user_balance_in_range(tokenX.taddr, feeReceiver, fee_sum_x, 1n) in
    let () = FA12_helper.assert_user_balance_in_range(tokenY.taddr, feeReceiver, fee_sum_y, 1n) in
    ()

// test_fees_are_proportional_to_liquidity
let test_fees_are_proportional_to_liquidity = 
    let feeBps = 2000n in
    let protoFeeBps = 700n in
    let effectiveProtoFeeBps = if config.y = CTEZ then protoFeeBps else 0n in
    let position_1_liquidity = 10_000_000n in
    let position_2_liquidity = position_1_liquidity * 3n in
    let lowerTickIndex = -10000 in
    let upperTickIndex = 10000 in
    let swapperBalanceX : nat = 1400n in
    let swapperBalanceY : nat = 3000n in
    let total_supply_x = position_1_liquidity + position_2_liquidity + swapperBalanceX in
    let total_supply_y = position_1_liquidity + position_2_liquidity + swapperBalanceY in

    let accounts = Bootstrap.boot_accounts() in
    let (liquidityProvider, swapper, feeReceiver1, feeReceiver2, liquidityProvider2) = accounts in
    // Deploy contracts
    let tokenX = Bootstrap.boot_token_fa12(0n, 0tez, liquidityProvider, total_supply_x) in
    let tokenY = Bootstrap.boot_token_fa12(0n, 0tez, liquidityProvider, total_supply_y) in
    let cfmm = Bootstrap.boot_cfmm_by_config(config, tokenX.addr, tokenY.addr, 1n, feeBps, effectiveProtoFeeBps, 0tez) in
    // Transfer some (token X and Y) to swapper
    let () = Test.set_source liquidityProvider in
    let () = FA12_helper.transfer_success((liquidityProvider, (swapper, swapperBalanceX)), tokenX.contr) in
    let () = FA12_helper.transfer_success((liquidityProvider, (swapper, swapperBalanceY)), tokenY.contr) in
    let () = FA12_helper.assert_user_balance(tokenX.taddr, swapper, swapperBalanceX) in
    let () = FA12_helper.assert_user_balance(tokenY.taddr, swapper, swapperBalanceY) in
    // Transfer some (token X and Y) to liquidityProvider2
    let () = Test.set_source liquidityProvider in
    let () = FA12_helper.transfer_success((liquidityProvider, (liquidityProvider2, position_2_liquidity)), tokenX.contr) in
    let () = FA12_helper.transfer_success((liquidityProvider, (liquidityProvider2, position_2_liquidity)), tokenY.contr) in
    let () = FA12_helper.assert_user_balance(tokenX.taddr, liquidityProvider2, position_2_liquidity) in
    let () = FA12_helper.assert_user_balance(tokenY.taddr, liquidityProvider2, position_2_liquidity) in
    // update_operators
    let () = Test.set_source liquidityProvider in
    let () = FA12_helper.approve_success((cfmm.addr, position_1_liquidity), tokenX.contr) in
    let () = FA12_helper.approve_success((cfmm.addr, position_1_liquidity), tokenY.contr) in
    let () = Test.set_source swapper in
    let () = FA12_helper.approve_success((cfmm.addr, swapperBalanceX), tokenX.contr) in
    let () = FA12_helper.approve_success((cfmm.addr, swapperBalanceY), tokenY.contr) in
    let () = Test.set_source liquidityProvider2 in
    let () = FA12_helper.approve_success((cfmm.addr, position_2_liquidity), tokenX.contr) in
    let () = FA12_helper.approve_success((cfmm.addr, position_2_liquidity), tokenY.contr) in

    // SET POSITION position_1_liquidity on [-10000; 10000] by liquidityProvider
    let () = Test.set_source liquidityProvider in
    let param : Cfmm.set_position_param = Cfmm_helper.generate_set_position_param(position_1_liquidity, (lowerTickIndex, upperTickIndex)) in
    let () = Cfmm_helper.set_position_success(param, 0tez, cfmm.contr) in
    let () = Cfmm_helper.assert_liquidity(cfmm.taddr, position_1_liquidity) in

    // SET POSITION position_2_liquidity on [-10000; 10000] by liquidityProvider2
    let () = Test.set_source liquidityProvider2 in
    let param : Cfmm.set_position_param = Cfmm_helper.generate_set_position_param(position_2_liquidity, (lowerTickIndex, upperTickIndex)) in
    let () = Cfmm_helper.set_position_success(param, 0tez, cfmm.contr) in
    let () = Cfmm_helper.assert_liquidity(cfmm.taddr, position_2_liquidity + position_1_liquidity) in

    // MULTI SWAP
    let () = Test.set_source swapper in
    let swaps : (bool * nat) list = [(false, 1000n); (true, 3000n); (false, 400n)] in
    let apply_swap_sum_fees(acc, elt : (nat * nat) * (bool * nat)) : (nat * nat) =
        if elt.0 then
            let param : Cfmm.y_to_x_param = Cfmm_helper.generate_y_to_x_param(elt.1, 0n, swapper) in
            let () = Cfmm_helper.y_to_x_success(param, 0tez, cfmm.contr) in
            (acc.0, acc.1 + Maths.calcSwapFee(feeBps, elt.1))
        else
            let param : Cfmm.x_to_y_param = Cfmm_helper.generate_x_to_y_param(elt.1, 0n, swapper) in
            let () = Cfmm_helper.x_to_y_success(param, 0tez, cfmm.contr) in
            (acc.0 + Maths.calcSwapFee(feeBps, elt.1), acc.1) 
    in
    let (fee_sum_x, fee_sum_y) = List.fold apply_swap_sum_fees swaps (0n, 0n) in

    // CHECK INVARIANTS
    let observer = Bootstrap.boot_observe_consumer(0tez) in
    let () = Cfmm_helper.check_all_invariants(cfmm.taddr, cfmm.addr, observer) in

    // COLLECT FEES
    let () = Cfmm_helper.collectFees(liquidityProvider, 0n, feeReceiver1, cfmm.contr) in
    let () = Cfmm_helper.collectFees(liquidityProvider2, 1n, feeReceiver2, cfmm.contr) in

    // Position 2 has triple the liquidity of Position 1,
    // so `feeReceiver1` should get 1/4 of all earned fees and `feeReceiver2` should get 3/4.
    // Note: Fees are rounded down when being distributed to LPs, so a margin of error of -1 is acceptable.
    let () = FA12_helper.assert_user_balance_in_range(tokenX.taddr, feeReceiver1, fee_sum_x / 4n, 1n) in
    let () = FA12_helper.assert_user_balance_in_range(tokenY.taddr, feeReceiver1, fee_sum_y / 4n, 1n) in
    let () = FA12_helper.assert_user_balance_in_range(tokenX.taddr, feeReceiver2, fee_sum_x * 3n / 4n, 1n) in
    let () = FA12_helper.assert_user_balance_in_range(tokenY.taddr, feeReceiver2, fee_sum_y * 3n / 4n, 1n) in

    // CHECK INVARIANTS
    let observer = Bootstrap.boot_observe_consumer(0tez) in
    let () = Cfmm_helper.check_all_invariants(cfmm.taddr, cfmm.addr, observer) in
    ()

// test_LPs_do_not_receive_past_fees
let test_LPs_do_not_receive_past_fees = 
    let feeBps = 2000n in
    let protoFeeBps = 700n in
    let effectiveProtoFeeBps = if config.y = CTEZ then protoFeeBps else 0n in
    let position_1_liquidity = 10_000_000n in
    let position_2_liquidity = 10_000_000n in
    let lowerTickIndex = -10000 in
    let upperTickIndex = 10000 in
    let swapperBalanceX : nat = 1400n * 2n in
    let swapperBalanceY : nat = 3000n * 2n in
    let total_supply_x = position_1_liquidity + position_2_liquidity + swapperBalanceX in
    let total_supply_y = position_1_liquidity + position_2_liquidity + swapperBalanceY in

    let accounts = Bootstrap.boot_accounts() in
    let (liquidityProvider, swapper, feeReceiver1, feeReceiver2, liquidityProvider2) = accounts in
    // Deploy contracts
    let tokenX = Bootstrap.boot_token_fa12(0n, 0tez, liquidityProvider, total_supply_x) in
    let tokenY = Bootstrap.boot_token_fa12(0n, 0tez, liquidityProvider, total_supply_y) in
    let cfmm = Bootstrap.boot_cfmm_by_config(config, tokenX.addr, tokenY.addr, 1n, feeBps, effectiveProtoFeeBps, 0tez) in
    // Transfer some (token X and Y) to swapper
    let () = Test.set_source liquidityProvider in
    let () = FA12_helper.transfer_success((liquidityProvider, (swapper, swapperBalanceX)), tokenX.contr) in
    let () = FA12_helper.transfer_success((liquidityProvider, (swapper, swapperBalanceY)), tokenY.contr) in
    let () = FA12_helper.assert_user_balance(tokenX.taddr, swapper, swapperBalanceX) in
    let () = FA12_helper.assert_user_balance(tokenY.taddr, swapper, swapperBalanceY) in
    // Transfer some (token X and Y) to liquidityProvider2
    let () = Test.set_source liquidityProvider in
    let () = FA12_helper.transfer_success((liquidityProvider, (liquidityProvider2, position_2_liquidity)), tokenX.contr) in
    let () = FA12_helper.transfer_success((liquidityProvider, (liquidityProvider2, position_2_liquidity)), tokenY.contr) in
    let () = FA12_helper.assert_user_balance(tokenX.taddr, liquidityProvider2, position_2_liquidity) in
    let () = FA12_helper.assert_user_balance(tokenY.taddr, liquidityProvider2, position_2_liquidity) in
    // update_operators
    let () = Test.set_source liquidityProvider in
    let () = FA12_helper.approve_success((cfmm.addr, position_1_liquidity), tokenX.contr) in
    let () = FA12_helper.approve_success((cfmm.addr, position_1_liquidity), tokenY.contr) in
    let () = Test.set_source swapper in
    let () = FA12_helper.approve_success((cfmm.addr, swapperBalanceX), tokenX.contr) in
    let () = FA12_helper.approve_success((cfmm.addr, swapperBalanceY), tokenY.contr) in
    let () = Test.set_source liquidityProvider2 in
    let () = FA12_helper.approve_success((cfmm.addr, position_2_liquidity), tokenX.contr) in
    let () = FA12_helper.approve_success((cfmm.addr, position_2_liquidity), tokenY.contr) in
    

    // SET POSITION position_1_liquidity on [-10000; 10000] by liquidityProvider
    let () = Test.set_source liquidityProvider in
    let param : Cfmm.set_position_param = Cfmm_helper.generate_set_position_param(position_1_liquidity, (lowerTickIndex, upperTickIndex)) in
    let () = Cfmm_helper.set_position_success(param, 0tez, cfmm.contr) in
    let () = Cfmm_helper.assert_liquidity(cfmm.taddr, position_1_liquidity) in

    // MULTI SWAP
    let () = Test.set_source swapper in
    let swaps : (bool * nat) list = [(false, 1000n); (true, 3000n); (false, 400n)] in   
    let apply_swap_sum_fees(acc, elt : (nat * nat) * (bool * nat)) : (nat * nat) =
        if elt.0 then
            let param : Cfmm.y_to_x_param = Cfmm_helper.generate_y_to_x_param(elt.1, 0n, swapper) in
            let () = Cfmm_helper.y_to_x_success(param, 0tez, cfmm.contr) in
            (acc.0, acc.1 + Maths.calcSwapFee(feeBps, elt.1))
        else
            let param : Cfmm.x_to_y_param = Cfmm_helper.generate_x_to_y_param(elt.1, 0n, swapper) in
            let () = Cfmm_helper.x_to_y_success(param, 0tez, cfmm.contr) in
            (acc.0 + Maths.calcSwapFee(feeBps, elt.1), acc.1) 
    in
    let (fee_sum_x_before, fee_sum_y_before) = List.fold apply_swap_sum_fees swaps (0n, 0n) in

    // SET POSITION position_2_liquidity on [-10000; 10000] by liquidityProvider2
    let () = Test.set_source liquidityProvider2 in
    let param : Cfmm.set_position_param = Cfmm_helper.generate_set_position_param(position_2_liquidity, (lowerTickIndex, upperTickIndex)) in
    let () = Cfmm_helper.set_position_success(param, 0tez, cfmm.contr) in
    let () = Cfmm_helper.assert_liquidity(cfmm.taddr, position_2_liquidity + position_1_liquidity) in

    // MULTI SWAP
    let () = Test.set_source swapper in
    let swaps : (bool * nat) list = [(false, 1000n); (true, 3000n); (false, 400n)] in   
    let apply_swap_sum_fees(acc, elt : (nat * nat) * (bool * nat)) : (nat * nat) =
        if elt.0 then
            let param : Cfmm.y_to_x_param = Cfmm_helper.generate_y_to_x_param(elt.1, 0n, swapper) in
            let () = Cfmm_helper.y_to_x_success(param, 0tez, cfmm.contr) in
            (acc.0, acc.1 + Maths.calcSwapFee(feeBps, elt.1))
        else
            let param : Cfmm.x_to_y_param = Cfmm_helper.generate_x_to_y_param(elt.1, 0n, swapper) in
            let () = Cfmm_helper.x_to_y_success(param, 0tez, cfmm.contr) in
            (acc.0 + Maths.calcSwapFee(feeBps, elt.1), acc.1) 
    in
    let (fee_sum_x_after, fee_sum_y_after) = List.fold apply_swap_sum_fees swaps (0n, 0n) in

    // CHECK INVARIANTS
    let observer = Bootstrap.boot_observe_consumer(0tez) in
    let () = Cfmm_helper.check_all_invariants(cfmm.taddr, cfmm.addr, observer) in

    // COLLECT FEES
    let () = Cfmm_helper.collectFees(liquidityProvider, 0n, feeReceiver1, cfmm.contr) in
    let () = Cfmm_helper.collectFees(liquidityProvider2, 1n, feeReceiver2, cfmm.contr) in

    // Fees from `beforeSwaps` should all go to Position 1.
    // Fees from `afterSwaps` should be evenly split between Position 1 and Position 2.
    // Note: Fees are rounded down when being distributed to LPs, so a margin of error of -1 is acceptable.
    let () = FA12_helper.assert_user_balance_in_range(tokenX.taddr, feeReceiver1, fee_sum_x_before + (fee_sum_x_after / 2n), 1n) in
    let () = FA12_helper.assert_user_balance_in_range(tokenY.taddr, feeReceiver1, fee_sum_y_before + (fee_sum_y_after / 2n), 1n) in
    let () = FA12_helper.assert_user_balance_in_range(tokenX.taddr, feeReceiver2, fee_sum_x_after / 2n, 1n) in
    let () = FA12_helper.assert_user_balance_in_range(tokenY.taddr, feeReceiver2, fee_sum_y_after / 2n, 1n) in

    // CHECK INVARIANTS
    let observer = Bootstrap.boot_observe_consumer(0tez) in
    let () = Cfmm_helper.check_all_invariants(cfmm.taddr, cfmm.addr, observer) in
    ()

// test_fees_are_discounted
let test_fees_are_discounted = 
    let feeBps = 2000n in
    let protoFeeBps = 700n in
    let effectiveProtoFeeBps = if config.y = CTEZ then protoFeeBps else 0n in
    let liquidity = 10_000_000n in 
    let lowerTickIndex = -10000 in
    let upperTickIndex = 10000 in
    
    let swapperBalanceX : nat = 1400n in
    let swapperBalanceY : nat = 3000n in
    let total_supply_x = liquidity * 2n + swapperBalanceX in
    let total_supply_y = liquidity * 2n + swapperBalanceY in

    let accounts = Bootstrap.boot_accounts() in
    let (liquidityProvider, swapper, swapReceiver, feeReceiver, _) = accounts in
    // Deploy contracts
    let tokenX = Bootstrap.boot_token_fa12(0n, 0tez, liquidityProvider, total_supply_x) in
    let tokenY = Bootstrap.boot_token_fa12(0n, 0tez, liquidityProvider, total_supply_y) in
    let cfmm = Bootstrap.boot_cfmm_by_config(config, tokenX.addr, tokenY.addr, 1n, feeBps, effectiveProtoFeeBps, 0tez) in
    // Transfer some (token X and Y) to swapper
    let () = Test.set_source liquidityProvider in
    let () = FA12_helper.transfer_success((liquidityProvider, (swapper, swapperBalanceX)), tokenX.contr) in
    let () = FA12_helper.transfer_success((liquidityProvider, (swapper, swapperBalanceY)), tokenY.contr) in
    let () = FA12_helper.assert_user_balance(tokenX.taddr, swapper, swapperBalanceX) in
    let () = FA12_helper.assert_user_balance(tokenY.taddr, swapper, swapperBalanceY) in
    // update_operators
    let () = Test.set_source liquidityProvider in
    let () = FA12_helper.approve_success((cfmm.addr, liquidity * 2n), tokenX.contr) in
    let () = FA12_helper.approve_success((cfmm.addr, liquidity * 2n), tokenY.contr) in
    let () = Test.set_source swapper in
    let () = FA12_helper.approve_success((cfmm.addr, swapperBalanceX), tokenX.contr) in
    let () = FA12_helper.approve_success((cfmm.addr, swapperBalanceY), tokenY.contr) in

    // SET POSITION
    let () = Test.set_source liquidityProvider in
    let param : Cfmm.set_position_param = Cfmm_helper.generate_set_position_param(liquidity, (lowerTickIndex, upperTickIndex)) in
    let () = Cfmm_helper.set_position_success(param, 0tez, cfmm.contr) in
    let () = Cfmm_helper.assert_liquidity(cfmm.taddr, liquidity) in

    let () = FA12_helper.assert_user_balance(tokenX.taddr, swapReceiver, 0n) in
    let () = FA12_helper.assert_user_balance(tokenY.taddr, swapReceiver, 0n) in
    let () = FA12_helper.assert_user_balance(tokenX.taddr, swapper, swapperBalanceX) in
    let () = FA12_helper.assert_user_balance(tokenY.taddr, swapper, swapperBalanceY) in

    // SWAP 1000 X , SWAP 3000 Y, SWAP 400 X
    // Apply multi SWAPS , compute swap fees and sum fees for all swaps
    let () = Test.set_source swapper in
    let swaps : (bool * nat) list = [(false, 1000n); (true, 3000n); (false, 400n)] in
    let apply_swap_sum_fees(acc, elt : (nat * nat) * (bool * nat)) : (nat * nat) =
        if elt.0 then
            let param : Cfmm.y_to_x_param = Cfmm_helper.generate_y_to_x_param(elt.1, 0n, swapper) in
            let () = Cfmm_helper.y_to_x_success(param, 0tez, cfmm.contr) in
            (acc.0, acc.1 + Maths.calcSwapFee(feeBps, elt.1))
        else
            let param : Cfmm.x_to_y_param = Cfmm_helper.generate_x_to_y_param(elt.1, 0n, swapper) in
            let () = Cfmm_helper.x_to_y_success(param, 0tez, cfmm.contr) in
            (acc.0 + Maths.calcSwapFee(feeBps, elt.1), acc.1) 
    in
    let (fee_sum_x, fee_sum_y) = List.fold apply_swap_sum_fees swaps (0n, 0n) in

    let initialBalanceLpX = FA12_helper.get_user_balance(tokenX.taddr, liquidityProvider) in
    let initialBalanceLpY = FA12_helper.get_user_balance(tokenY.taddr, liquidityProvider) in

    // UPDATE POSITION
    let () = Test.set_source liquidityProvider in
    let param_update : Cfmm.update_position_param = Cfmm_helper.generate_update_position_param(0n, int(liquidity), feeReceiver, 5000000n, 5000000n) in
    let () = Cfmm_helper.update_position_success(param_update, 0tez, cfmm.contr) in
    let () = Cfmm_helper.assert_liquidity(cfmm.taddr, liquidity * 2n) in

    // VERIFY BALANCES
    let finalBalanceLpX = FA12_helper.get_user_balance(tokenX.taddr, liquidityProvider) in
    let finalBalanceLpY = FA12_helper.get_user_balance(tokenY.taddr, liquidityProvider) in
    let finalBalanceFeeReceiverX = FA12_helper.get_user_balance(tokenX.taddr, feeReceiver) in
    let finalBalanceFeeReceiverY = FA12_helper.get_user_balance(tokenY.taddr, feeReceiver) in

    // -- The fees earned during the swaps should be discounted from the
    // -- tokens needed to make the deposit.
    // -- Due to rounding, it's possible the LP will receive 1 fewer tokens than expected.
    let s_after = Test.get_storage cfmm.taddr in
    let (delta_x, delta_y) = Maths.liquidityDeltaToTokensDelta(int(liquidity), lowerTickIndex, upperTickIndex, s_after.cur_tick_index.i, s_after.sqrt_price, s_after.ladder) in

    // -- Note: Fees are rounded down when being distributed to LPs, so a margin of error of -1 is acceptable.
    // -- Due to the floating-point math used in `liquidityDeltaToTokensDelta`, it's possible there
    // -- will be an additional +/- 1 error.
    let expected_val_x = initialBalanceLpX + fee_sum_x - delta_x in
    let expected_val_y = initialBalanceLpY + fee_sum_y - delta_y in
    let () = assert((abs(expected_val_x - 2n) <= finalBalanceLpX) && (finalBalanceLpX <= abs(expected_val_x + 1n))) in
    let () = assert((abs(expected_val_y - 2n) <= finalBalanceLpY) && (finalBalanceLpY <= abs(expected_val_y + 1n))) in
    // -- `feeReceiver` should not receive any fees.
    let () = assert(finalBalanceFeeReceiverX = 0n) in
    let () = assert(finalBalanceFeeReceiverY = 0n) in
    ()

// test_ticks_are_updated
let test_ticks_are_updated = 
    let feeBps = 5000n in
    let protoFeeBps = 700n in
    let effectiveProtoFeeBps = if config.y = CTEZ then protoFeeBps else 0n in
    let liquidity = 100_000n in 
    let tick_indices = (0, 50, 100, 150) in

    let swapperBalanceX : nat = 1400n in
    let swapperBalanceY : nat = 3000n in
    let total_supply_x = liquidity * 2n + swapperBalanceX in
    let total_supply_y = liquidity * 2n + swapperBalanceY in

    let accounts = Bootstrap.boot_accounts() in
    let (liquidityProvider, swapper, _swapReceiver, _feeReceiver, _) = accounts in
    // Deploy contracts
    let tokenX = Bootstrap.boot_token_fa12(0n, 0tez, liquidityProvider, total_supply_x) in
    let tokenY = Bootstrap.boot_token_fa12(0n, 0tez, liquidityProvider, total_supply_y) in
    let cfmm = Bootstrap.boot_cfmm_by_config(config, tokenX.addr, tokenY.addr, 1n, feeBps, effectiveProtoFeeBps, 0tez) in
    // Transfer some (token X and Y) to swapper
    let () = Test.set_source liquidityProvider in
    let () = FA12_helper.transfer_success((liquidityProvider, (swapper, swapperBalanceX)), tokenX.contr) in
    let () = FA12_helper.transfer_success((liquidityProvider, (swapper, swapperBalanceY)), tokenY.contr) in
    let () = FA12_helper.assert_user_balance(tokenX.taddr, swapper, swapperBalanceX) in
    let () = FA12_helper.assert_user_balance(tokenY.taddr, swapper, swapperBalanceY) in
    // update_operators
    let () = Test.set_source liquidityProvider in
    let () = FA12_helper.approve_success((cfmm.addr, liquidity * 2n), tokenX.contr) in
    let () = FA12_helper.approve_success((cfmm.addr, liquidity * 2n), tokenY.contr) in
    let () = Test.set_source swapper in
    let () = FA12_helper.approve_success((cfmm.addr, swapperBalanceX), tokenX.contr) in
    let () = FA12_helper.approve_success((cfmm.addr, swapperBalanceY), tokenY.contr) in

    // SET 2 POSITION 100000 on [0; 100] and 100000 on [50; 150]
    let () = Test.set_source liquidityProvider in
    let param : Cfmm.set_position_param = Cfmm_helper.generate_set_position_param(liquidity, (tick_indices.0, tick_indices.2)) in
    let () = Cfmm_helper.set_position_success(param, 0tez, cfmm.contr) in
    let param : Cfmm.set_position_param = Cfmm_helper.generate_set_position_param(liquidity, (tick_indices.1, tick_indices.3)) in
    let () = Cfmm_helper.set_position_success(param, 0tez, cfmm.contr) in
    let () = Cfmm_helper.assert_liquidity(cfmm.taddr, liquidity) in

    // -- Place a small swap to move the tick a little bit
    // -- and make sure `tick_cumulative` is not 0.
    // SWAP 100 Y
    let () = Test.set_source swapper in
    let param : Cfmm.y_to_x_param = Cfmm_helper.generate_y_to_x_param(100n, 0n, swapper) in
    let () = Cfmm_helper.y_to_x_success(param, 0tez, cfmm.contr) in

    // -- Place a swap big enough to cross tick `50` and therefore
    // -- change the value of the `*_outside` fields to something other than zero.
    // SWAP 1000 Y
    let param : Cfmm.y_to_x_param = Cfmm_helper.generate_y_to_x_param(1000n, 0n, swapper) in
    let () = Cfmm_helper.y_to_x_success(param, 0tez, cfmm.contr) in

    let s_initial = Test.get_storage cfmm.taddr in
    let initial_state_tick1 = match Big_map.find_opt {i=tick_indices.1} s_initial.ticks with
    | Some ts -> ts
    | None -> Test.failwith("should never occur: Tick1 not found !")
    in
    
    // -- Place a new position on `ti2` in order to update its state.
    // SET POSITION 100000 on [50;100]
    let () = Test.set_source liquidityProvider in
    let param : Cfmm.set_position_param = Cfmm_helper.generate_set_position_param(liquidity, (tick_indices.1, tick_indices.2)) in
    let () = Cfmm_helper.set_position_success(param, 0tez, cfmm.contr) in

    // -- Check that `tick1`'s state has been updated.
    let s_final = Test.get_storage cfmm.taddr in
    let final_state_tick1 = match Big_map.find_opt {i=tick_indices.1} s_final.ticks with
    | Some ts -> ts
    | None -> Test.failwith("should never occur: Tick1 not found !")
    in
    let () = assert(final_state_tick1.n_positions = initial_state_tick1.n_positions + 1n) in
    let () = assert(final_state_tick1.liquidity_net = initial_state_tick1.liquidity_net + liquidity) in
    let () = assert(final_state_tick1.sqrt_price = initial_state_tick1.sqrt_price) in

    // -- Accumulators should stay unchanged.
    let () = assert(final_state_tick1.fee_growth_outside = initial_state_tick1.fee_growth_outside) in
    let () = assert(final_state_tick1.seconds_outside = initial_state_tick1.seconds_outside) in
    let () = assert(final_state_tick1.seconds_per_liquidity_outside = initial_state_tick1.seconds_per_liquidity_outside) in
    let () = assert(final_state_tick1.tick_cumulative_outside = initial_state_tick1.tick_cumulative_outside) in
    ()

// test_many_small_liquidations
let test_many_small_liquidations = 
    let feeBps = 5000n in
    let protoFeeBps = 700n in
    let effectiveProtoFeeBps = if config.y = CTEZ then protoFeeBps else 0n in
    let liquidity = 10_000_000n in 
    let lowerTickIndex = -10000 in
    let upperTickIndex = 10000 in
    
    let swapperBalanceX : nat = 1400n in
    let swapperBalanceY : nat = 3000n in
    let total_supply_x = liquidity * 2n + swapperBalanceX in
    let total_supply_y = liquidity * 2n + swapperBalanceY in

    let accounts = Bootstrap.boot_accounts() in
    let (liquidityProvider, liquidityProvider2, swapper, receiver1, receiver2) = accounts in
    // Deploy contracts
    let tokenX = Bootstrap.boot_token_fa12(0n, 0tez, liquidityProvider, total_supply_x) in
    let tokenY = Bootstrap.boot_token_fa12(0n, 0tez, liquidityProvider, total_supply_y) in
    let cfmm = Bootstrap.boot_cfmm_by_config(config, tokenX.addr, tokenY.addr, 1n, feeBps, effectiveProtoFeeBps, 0tez) in
    // Transfer some (token X and Y) to swapper
    let () = Test.set_source liquidityProvider in
    let () = FA12_helper.transfer_success((liquidityProvider, (swapper, swapperBalanceX)), tokenX.contr) in
    let () = FA12_helper.transfer_success((liquidityProvider, (swapper, swapperBalanceY)), tokenY.contr) in
    let () = FA12_helper.assert_user_balance(tokenX.taddr, swapper, swapperBalanceX) in
    let () = FA12_helper.assert_user_balance(tokenY.taddr, swapper, swapperBalanceY) in
    // Transfer some (token X and Y) to liquidityProvider2
    let () = FA12_helper.transfer_success((liquidityProvider, (liquidityProvider2, liquidity)), tokenX.contr) in
    let () = FA12_helper.transfer_success((liquidityProvider, (liquidityProvider2, liquidity)), tokenY.contr) in
    let () = FA12_helper.assert_user_balance(tokenX.taddr, liquidityProvider2, liquidity) in
    let () = FA12_helper.assert_user_balance(tokenY.taddr, liquidityProvider2, liquidity) in

    // update_operators
    let () = Test.set_source liquidityProvider in
    let () = FA12_helper.approve_success((cfmm.addr, liquidity), tokenX.contr) in
    let () = FA12_helper.approve_success((cfmm.addr, liquidity), tokenY.contr) in
    let () = Test.set_source liquidityProvider2 in
    let () = FA12_helper.approve_success((cfmm.addr, liquidity), tokenX.contr) in
    let () = FA12_helper.approve_success((cfmm.addr, liquidity), tokenY.contr) in
    let () = Test.set_source swapper in
    let () = FA12_helper.approve_success((cfmm.addr, swapperBalanceX), tokenX.contr) in
    let () = FA12_helper.approve_success((cfmm.addr, swapperBalanceY), tokenY.contr) in

    // SET POSITION by liquidityProvider
    let () = Test.set_source liquidityProvider in
    let param : Cfmm.set_position_param = Cfmm_helper.generate_set_position_param(liquidity, (lowerTickIndex, upperTickIndex)) in
    let () = Cfmm_helper.set_position_success(param, 0tez, cfmm.contr) in
    // SET POSITION by liquidityProvider2
    let () = Test.set_source liquidityProvider2 in
    let param : Cfmm.set_position_param = Cfmm_helper.generate_set_position_param(liquidity, (lowerTickIndex, upperTickIndex)) in
    let () = Cfmm_helper.set_position_success(param, 0tez, cfmm.contr) in
    let () = Cfmm_helper.assert_liquidity(cfmm.taddr, liquidity * 2n) in

    // SWAP 1000 X , SWAP 3000 Y, SWAP 400 X
    // Apply multi SWAPS , compute swap fees and sum fees for all swaps
    let () = Test.set_source swapper in
    let swaps : (bool * nat) list = [(false, 1000n); (true, 3000n); (false, 400n)] in
    let apply_swap_sum_fees(acc, elt : (nat * nat) * (bool * nat)) : (nat * nat) =
        if elt.0 then
            let param : Cfmm.y_to_x_param = Cfmm_helper.generate_y_to_x_param(elt.1, 0n, swapper) in
            let () = Cfmm_helper.y_to_x_success(param, 0tez, cfmm.contr) in
            (acc.0, acc.1 + Maths.calcSwapFee(feeBps, elt.1))
        else
            let param : Cfmm.x_to_y_param = Cfmm_helper.generate_x_to_y_param(elt.1, 0n, swapper) in
            let () = Cfmm_helper.x_to_y_success(param, 0tez, cfmm.contr) in
            (acc.0 + Maths.calcSwapFee(feeBps, elt.1), acc.1) 
    in
    let (_fee_sum_x, _fee_sum_y) = List.fold apply_swap_sum_fees swaps (0n, 0n) in

    //   -- Liquidate the position all at once
    // UPDATE POSITION
    let () = Test.set_source liquidityProvider in
    let param_update : Cfmm.update_position_param = Cfmm_helper.generate_update_position_param(0n, 0n-liquidity, receiver1, 0n, 0n) in
    let () = Cfmm_helper.update_position_success(param_update, 0tez, cfmm.contr) in
      
    // -- Liquidate the position in small steps
    // UPDATE 10 times -1000000 
    let () = Test.set_source liquidityProvider2 in
    let gen_args(_count : nat) : (nat * int * address) = (1n, (0n-liquidity)/10, receiver2) in
    let apply_liquidate(position_id, liq, receiver: nat * int * address) : test_exec_result = 
        let param_update : Cfmm.update_position_param = Cfmm_helper.generate_update_position_param(position_id, liq, receiver, 0n, 0n) in
        let r = Cfmm_helper.update_position(param_update, 0tez, cfmm.contr) in
        let () = Assert.tx_success r in
        r
    in
    let _ = Utils.repeat(10n, apply_liquidate, gen_args, ([]: test_exec_result list)) in

    // VERIFY BALANCES
    let receiver1_x = FA12_helper.get_user_balance(tokenX.taddr, receiver1) in
    let receiver1_y = FA12_helper.get_user_balance(tokenY.taddr, receiver1) in
    let receiver2_x = FA12_helper.get_user_balance(tokenX.taddr, receiver2) in
    let receiver2_y = FA12_helper.get_user_balance(tokenY.taddr, receiver2) in
    //   -- Liquidating in 10 smaller steps may lead
    //   -- to `receiver2` receiving up to 10 fewer tokens due to rounding errors.
    let () = assert(receiver1_x - 10n <= int(receiver2_x) && receiver2_x <= receiver1_x ) in
    let () = assert(receiver1_y - 10n <= int(receiver2_y) && receiver2_y <= receiver1_y ) in
    ()

// test_updating_nonexisting_position
let test_updating_nonexisting_position = 
    let feeBps = 5000n in
    let protoFeeBps = 700n in
    let effectiveProtoFeeBps = if config.y = CTEZ then protoFeeBps else 0n in
    let liquidity = 10_000_000n in 
    let lowerTickIndex = -10000 in
    let upperTickIndex = 10000 in
    
    let accounts = Bootstrap.boot_accounts() in
    let (liquidityProvider, _, _, _, _) = accounts in
    // Deploy contracts
    let tokenX = Bootstrap.boot_token_fa12(0n, 0tez, liquidityProvider, liquidity) in
    let tokenY = Bootstrap.boot_token_fa12(0n, 0tez, liquidityProvider, liquidity) in
    let cfmm = Bootstrap.boot_cfmm_by_config(config, tokenX.addr, tokenY.addr, 1n, feeBps, effectiveProtoFeeBps, 0tez) in
    // update_operators
    let () = Test.set_source liquidityProvider in
    let () = FA12_helper.approve_success((cfmm.addr, liquidity), tokenX.contr) in
    let () = FA12_helper.approve_success((cfmm.addr, liquidity), tokenY.contr) in

    // SET POSITION by liquidityProvider
    let () = Test.set_source liquidityProvider in
    let param : Cfmm.set_position_param = Cfmm_helper.generate_set_position_param(liquidity, (lowerTickIndex, upperTickIndex)) in
    let () = Cfmm_helper.set_position_success(param, 0tez, cfmm.contr) in

    // UPDATE POSITION with invalid position_id
    let param_update : Cfmm.update_position_param = Cfmm_helper.generate_update_position_param(3n, int(liquidity), liquidityProvider, 0n, 0n) in
    let r = Cfmm_helper.update_position(param_update, 0tez, cfmm.contr) in
    Assert.position_failure r "FA2_TOKEN_UNDEFINED"