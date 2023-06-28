#import "../helpers/assert.mligo" "Assert"
#import "../helpers/utils.mligo" "Utils"
#import "../bootstrap/bootstrap.mligo" "Bootstrap"
#import "../helpers/log.mligo" "Log"
#import "../helpers/cfmm.mligo" "Cfmm_helper"
#import "../../lib/cfmm/main.mligo" "Cfmm"
#import "../helpers/fa12.mligo" "FA12_helper"
#import "../helpers/extended_fa2.mligo" "ExtendedFA2_helper"
#import "../helpers/maths.mligo" "Maths"
#import "../helpers/config.mligo" "Config_helper"

let () = Log.describe("[FA2-CTEZ] [Cfmm.x_to_y] test suite")

let config = ({x=FA2; y=CTEZ} : Config_helper.config)

(* Successful swap X -> Y *)
let test_swapping_100_within_a_single_tick_range =
    let feeBps = 700n in
    let protoFeeBps = 700n in
    let effectiveProtoFeeBps = if config.y = CTEZ then protoFeeBps else 0n in
    let liquidity = 10_000_000n in 
    let lowerTickIndex = -1000 in
    let upperTickIndex = 1000 in
    
    let swapperBalanceX : nat = 1000000n in
    let swapperBalanceY : nat = swapperBalanceX in
    let accounts = Bootstrap.boot_accounts() in
    let (liquidityProvider, swapper, swapReceiver, feeReceiver, _) = accounts in
    // Deploy contracts
    let tokenX = Bootstrap.boot_token(0n, 0tez, liquidityProvider, liquidity + swapperBalanceX) in
    let tokenY = Bootstrap.boot_token_fa12(0n, 0tez, liquidityProvider, liquidity + swapperBalanceY) in
    let cfmm = Bootstrap.boot_cfmm_by_config(config, tokenX.addr, tokenY.addr, 1n, feeBps, effectiveProtoFeeBps, 0tez) in
    // Transfer 1_000_000 (token X and Y) to swapper
    let () = Test.set_source liquidityProvider in
    let () = ExtendedFA2_helper.transfer_success([{from_=liquidityProvider; txs=[{to_=swapper; token_id=0n; amount=swapperBalanceX}]}], tokenX.contr) in
    let () = FA12_helper.transfer_success((liquidityProvider, (swapper, swapperBalanceY)), tokenY.contr) in
    let () = ExtendedFA2_helper.assert_user_balance(tokenX.taddr, swapper, swapperBalanceX) in
    let () = FA12_helper.assert_user_balance(tokenY.taddr, swapper, swapperBalanceY) in
    // update_operators
    let () = Test.set_source liquidityProvider in
    let () = ExtendedFA2_helper.update_operators_success([Add_operator({owner=liquidityProvider; operator=cfmm.addr; token_id=0n})], tokenX.contr) in
    let () = FA12_helper.approve_success((cfmm.addr, liquidity), tokenY.contr) in

    let () = Test.set_source swapper in
    let () = ExtendedFA2_helper.update_operators_success([Add_operator({owner=swapper; operator=cfmm.addr; token_id=0n})], tokenX.contr) in
    let () = FA12_helper.approve_success((cfmm.addr, swapperBalanceY), tokenY.contr) in

    // INCREASE_OBSERVATION_COUNT : Add some slots to the buffers to make the tests more meaningful.
    let param : Cfmm.increase_observation_count_param = Cfmm_helper.generate_increase_observation_count_param(10n) in
    let () = Cfmm_helper.increase_observation_count_success(param, 0tez, cfmm.contr) in

    // SET POSITION
    let () = Test.set_source liquidityProvider in
    let param : Cfmm.set_position_param = Cfmm_helper.generate_set_position_param(liquidity, (lowerTickIndex, upperTickIndex)) in
    let () = Cfmm_helper.set_position_success(param, 0tez, cfmm.contr) in
    let () = Cfmm_helper.assert_liquidity(cfmm.taddr, liquidity) in
    let expected_x_initial_balance_for_10000000 = 487706n in
    let () = ExtendedFA2_helper.assert_user_balance(tokenX.taddr, cfmm.addr, expected_x_initial_balance_for_10000000) in 
    let () = ExtendedFA2_helper.assert_user_balance(tokenX.taddr, liquidityProvider, abs(10000000 - expected_x_initial_balance_for_10000000)) in 
    let expected_y_initial_balance_for_10000000 = 487706n in
    let () = FA12_helper.assert_user_balance(tokenY.taddr, cfmm.addr, expected_y_initial_balance_for_10000000) in 
    let () = FA12_helper.assert_user_balance(tokenY.taddr, liquidityProvider, abs(10000000 - expected_y_initial_balance_for_10000000)) in 

    let s_init = Test.get_storage cfmm.taddr in
    let () = ExtendedFA2_helper.assert_user_balance(tokenX.taddr, swapReceiver, 0n) in
    let () = ExtendedFA2_helper.assert_user_balance(tokenX.taddr, swapper, swapperBalanceX) in
    let () = FA12_helper.assert_user_balance(tokenY.taddr, swapper, swapperBalanceY) in

    // SWAP 100 X_TOKEN 
    let () = Test.set_source swapper in
    let x_amount_swap = 100n in
    let param : Cfmm.x_to_y_param = Cfmm_helper.generate_x_to_y_param(x_amount_swap, 50n, swapReceiver) in
    let () = Cfmm_helper.x_to_y_success(param, 0tez, cfmm.contr) in 
    let s_after = Test.get_storage cfmm.taddr in

    // VERIFY The contract's `sqrt_price` has moved accordingly.
    let fee_dx = Maths.calcSwapFee(feeBps, x_amount_swap) in
    let new_dx  = abs(x_amount_swap - fee_dx) in 
    let expected_sqrt_price = Maths.calcNewPriceX(s_init.sqrt_price, liquidity, new_dx) in
    let () = assert(s_after.sqrt_price.x80 = expected_sqrt_price) in 

    // VERIFY fee growth
    let expected_fee =  Bitwise.shift_left (fee_dx) 128n in 
    let expected_fee_growth = expected_fee / liquidity in
    let () = assert(s_after.fee_growth.x.x128 = expected_fee_growth) in

    // VERIFY expected DY
    let expectedDy = Maths.calcReceivedY(s_init.sqrt_price.x80, expected_sqrt_price, liquidity, effectiveProtoFeeBps) in

    // VERIFY TOKEN TRANSFER
    let () = ExtendedFA2_helper.assert_user_balance(tokenX.taddr, cfmm.addr, expected_x_initial_balance_for_10000000 + x_amount_swap) in
    let () = ExtendedFA2_helper.assert_user_balance(tokenX.taddr, swapper, abs(swapperBalanceX - x_amount_swap)) in
    // y_received = (sqrt_price - sqrt_price_new) * liquidity / 2^80 
    //            = ((1208914576709065780394418 - 1208925819614629174706176) * 10000000) / 2^80  
    //            = -92.999135108
    let expected_y_received_for_100 = expectedDy in // 92n in

    let () = FA12_helper.assert_user_balance(tokenY.taddr, swapReceiver, expected_y_received_for_100) in
    let () = FA12_helper.assert_user_balance(tokenY.taddr, cfmm.addr, abs(expected_y_initial_balance_for_10000000 - expected_y_received_for_100)) in

    // // VERIFY FEES: `feeReceiver` receives the expected fees.
    //collectFees cfmm feeReceiver 0 liquidityProvider => update_position
    let () = ExtendedFA2_helper.assert_user_balance(tokenX.taddr, feeReceiver, 0n) in
    // COLLECT FEES
    let () = Cfmm_helper.collectFees(liquidityProvider, 0n, feeReceiver, cfmm.contr) in

    
    let expected_fee = Maths.calcSwapFee(feeBps, x_amount_swap) in
    //let () = ExtendedFA2_helper.assert_user_balance(tokenX.taddr, feeReceiver, expected_fee) in
    let () = ExtendedFA2_helper.assert_user_balance_in_range(tokenX.taddr, feeReceiver, expected_fee, 1n) in
    FA12_helper.assert_user_balance(tokenY.taddr, feeReceiver, 0n)

let test_swapping_1_within_a_single_tick_range =
    let feeBps = 700n in
    let protoFeeBps = 700n in
    let effectiveProtoFeeBps = if config.y = CTEZ then protoFeeBps else 0n in
    let liquidity = 10_000_000n in 
    let lowerTickIndex = -1000 in
    let upperTickIndex = 1000 in
    
    let swapperBalanceX : nat = 1000000n in
    let swapperBalanceY : nat = swapperBalanceX in
    let accounts = Bootstrap.boot_accounts() in
    let (liquidityProvider, swapper, swapReceiver, feeReceiver, _) = accounts in
    // Deploy contracts
    let tokenX = Bootstrap.boot_token(0n, 0tez, liquidityProvider, liquidity + swapperBalanceX) in
    let tokenY = Bootstrap.boot_token_fa12(0n, 0tez, liquidityProvider, liquidity + swapperBalanceY) in
    let cfmm = Bootstrap.boot_cfmm_by_config(config, tokenX.addr, tokenY.addr, 1n, feeBps, effectiveProtoFeeBps, 0tez) in
    // Transfer 1_000_000 (token X and Y) to swapper
    let () = Test.set_source liquidityProvider in
    let () = ExtendedFA2_helper.transfer_success([{from_=liquidityProvider; txs=[{to_=swapper; token_id=0n; amount=swapperBalanceX}]}], tokenX.contr) in
    let () = FA12_helper.transfer_success((liquidityProvider, (swapper, swapperBalanceY)), tokenY.contr) in
    let () = ExtendedFA2_helper.assert_user_balance(tokenX.taddr, swapper, swapperBalanceX) in
    let () = FA12_helper.assert_user_balance(tokenY.taddr, swapper, swapperBalanceY) in
    // update_operators
    let () = Test.set_source liquidityProvider in
    let () = ExtendedFA2_helper.update_operators_success([Add_operator({owner=liquidityProvider; operator=cfmm.addr; token_id=0n})], tokenX.contr) in
    let () = FA12_helper.approve_success((cfmm.addr, liquidity), tokenY.contr) in
    let () = Test.set_source swapper in
    let () = ExtendedFA2_helper.update_operators_success([Add_operator({owner=swapper; operator=cfmm.addr; token_id=0n})], tokenX.contr) in
    let () = FA12_helper.approve_success((cfmm.addr, swapperBalanceY), tokenY.contr) in

    // SET POSITION
    let () = Test.set_source liquidityProvider in
    let param : Cfmm.set_position_param = Cfmm_helper.generate_set_position_param(liquidity, (lowerTickIndex, upperTickIndex)) in
    let () = Cfmm_helper.set_position_success(param, 0tez, cfmm.contr) in
    let () = Cfmm_helper.assert_liquidity(cfmm.taddr, liquidity) in
    let expected_x_initial_balance_for_10000000 = 487706n in
    let () = ExtendedFA2_helper.assert_user_balance(tokenX.taddr, cfmm.addr, expected_x_initial_balance_for_10000000) in 
    let () = ExtendedFA2_helper.assert_user_balance(tokenX.taddr, liquidityProvider, abs(10000000 - expected_x_initial_balance_for_10000000)) in 
    let expected_y_initial_balance_for_10000000 = 487706n in
    let () = FA12_helper.assert_user_balance(tokenY.taddr, cfmm.addr, expected_y_initial_balance_for_10000000) in 
    let () = FA12_helper.assert_user_balance(tokenY.taddr, liquidityProvider, abs(10000000 - expected_y_initial_balance_for_10000000)) in 


    let s_init = Test.get_storage cfmm.taddr in
    let () = ExtendedFA2_helper.assert_user_balance(tokenX.taddr, swapReceiver, 0n) in
    let () = ExtendedFA2_helper.assert_user_balance(tokenX.taddr, swapper, swapperBalanceX) in
    let () = FA12_helper.assert_user_balance(tokenY.taddr, swapper, swapperBalanceY) in
    // let (swapperBalX, swapperBalY) = Cfmm_helper.balanceOf swapper in
    // let (swapReceiverBalX, swapReceiverBalY) = Cfmm_helper.balanceOf swapReceiver in

    // SWAP 1 X_TOKEN (beware min_dy = 0 means you just offer 1 token_X)
    let () = Test.set_source swapper in
    let x_amount_swap = 1n in
    let param : Cfmm.x_to_y_param = Cfmm_helper.generate_x_to_y_param(x_amount_swap, 0n, swapReceiver) in
    let () = Cfmm_helper.x_to_y_success(param, 0tez, cfmm.contr) in
    let s_after = Test.get_storage cfmm.taddr in

    // VERIFY The contract's `sqrt_price` has moved accordingly.
    let fee_dx = Maths.calcSwapFee(feeBps, x_amount_swap) in
    let new_dx  = abs(x_amount_swap - fee_dx) in 
    let expected_sqrt_price = Maths.calcNewPriceX(s_init.sqrt_price, liquidity, new_dx) in
    let () = assert(s_after.sqrt_price.x80 = expected_sqrt_price) in 

    // VERIFY fee growth
    let expected_fee =  Bitwise.shift_left (fee_dx) 128n in 
    let expected_fee_growth = expected_fee / liquidity in
    let () = assert(s_after.fee_growth.x.x128 = expected_fee_growth) in

    // VERIFY expected DY
    let expectedDy = Maths.calcReceivedY(s_init.sqrt_price.x80, expected_sqrt_price, liquidity, effectiveProtoFeeBps) in

    // VERIFY TOKEN TRANSFER
    let () = ExtendedFA2_helper.assert_user_balance(tokenX.taddr, cfmm.addr, expected_x_initial_balance_for_10000000 + x_amount_swap) in
    let () = ExtendedFA2_helper.assert_user_balance(tokenX.taddr, swapper, abs(swapperBalanceX - x_amount_swap)) in
    // y_received = (sqrt_price - sqrt_price_new) * liquidity / 2^80 
    //            = ((1208914576709065780394418 - 1208925819614629174706176) * 10000000) / 2^80  
    //            = -92.999135108
    let expected_y_received_for_100 = expectedDy in // 92n in
    let () = FA12_helper.assert_user_balance(tokenY.taddr, swapReceiver, expected_y_received_for_100) in
    let () = FA12_helper.assert_user_balance(tokenY.taddr, cfmm.addr, abs(expected_y_initial_balance_for_10000000 - expected_y_received_for_100)) in

    // // VERIFY FEES: `feeReceiver` receives the expected fees.
    //collectFees cfmm feeReceiver 0 liquidityProvider => update_position
    let () = ExtendedFA2_helper.assert_user_balance(tokenX.taddr, feeReceiver, 0n) in
    // COLLECT FEES
    let () = Cfmm_helper.collectFees(liquidityProvider, 0n, feeReceiver, cfmm.contr) in
    
    let expected_fee = Maths.calcSwapFee(feeBps, x_amount_swap) in 
    let () = ExtendedFA2_helper.assert_user_balance_in_range(tokenX.taddr, feeReceiver, expected_fee, 1n) in
    FA12_helper.assert_user_balance(tokenY.taddr, feeReceiver, 0n)


let test_swapping_10000_within_a_single_tick_range =
    let feeBps = 700n in
    let protoFeeBps = 700n in
    let effectiveProtoFeeBps = if config.y = CTEZ then protoFeeBps else 0n in
    let liquidity = 10_000_000n in 
    let lowerTickIndex = -1000 in
    let upperTickIndex = 1000 in
    
    let swapperBalanceX : nat = 1000000n in
    let swapperBalanceY : nat = swapperBalanceX in
    let accounts = Bootstrap.boot_accounts() in
    let (liquidityProvider, swapper, swapReceiver, feeReceiver, _) = accounts in
    // Deploy contracts
    let tokenX = Bootstrap.boot_token(0n, 0tez, liquidityProvider, liquidity + swapperBalanceX) in
    let tokenY = Bootstrap.boot_token_fa12(0n, 0tez, liquidityProvider, liquidity + swapperBalanceY) in
    let cfmm = Bootstrap.boot_cfmm_by_config(config, tokenX.addr, tokenY.addr, 1n, feeBps, effectiveProtoFeeBps, 0tez) in
    // Transfer 1_000_000 (token X and Y) to swapper
    let () = Test.set_source liquidityProvider in
    let () = ExtendedFA2_helper.transfer_success([{from_=liquidityProvider; txs=[{to_=swapper; token_id=0n; amount=swapperBalanceX}]}], tokenX.contr) in
    let () = FA12_helper.transfer_success((liquidityProvider, (swapper, swapperBalanceY)), tokenY.contr) in
    let () = ExtendedFA2_helper.assert_user_balance(tokenX.taddr, swapper, swapperBalanceX) in
    let () = FA12_helper.assert_user_balance(tokenY.taddr, swapper, swapperBalanceY) in
    // update_operators
    let () = Test.set_source liquidityProvider in
    let () = ExtendedFA2_helper.update_operators_success([Add_operator({owner=liquidityProvider; operator=cfmm.addr; token_id=0n})], tokenX.contr) in
    let () = FA12_helper.approve_success((cfmm.addr, liquidity), tokenY.contr) in
    let () = Test.set_source swapper in
    let () = ExtendedFA2_helper.update_operators_success([Add_operator({owner=swapper; operator=cfmm.addr; token_id=0n})], tokenX.contr) in
    let () = FA12_helper.approve_success((cfmm.addr, swapperBalanceY), tokenY.contr) in

    // SET POSITION
    let () = Test.set_source liquidityProvider in
    let param : Cfmm.set_position_param = Cfmm_helper.generate_set_position_param(liquidity, (lowerTickIndex, upperTickIndex)) in
    let () = Cfmm_helper.set_position_success(param, 0tez, cfmm.contr) in
    let () = Cfmm_helper.assert_liquidity(cfmm.taddr, liquidity) in
    let expected_x_initial_balance_for_10000000 = 487706n in
    let () = ExtendedFA2_helper.assert_user_balance(tokenX.taddr, cfmm.addr, expected_x_initial_balance_for_10000000) in 
    let () = ExtendedFA2_helper.assert_user_balance(tokenX.taddr, liquidityProvider, abs(10000000 - expected_x_initial_balance_for_10000000)) in 
    let expected_y_initial_balance_for_10000000 = 487706n in
    let () = FA12_helper.assert_user_balance(tokenY.taddr, cfmm.addr, expected_y_initial_balance_for_10000000) in 
    let () = FA12_helper.assert_user_balance(tokenY.taddr, liquidityProvider, abs(10000000 - expected_y_initial_balance_for_10000000)) in 


    let s_init = Test.get_storage cfmm.taddr in
    let () = ExtendedFA2_helper.assert_user_balance(tokenX.taddr, swapReceiver, 0n) in
    let () = ExtendedFA2_helper.assert_user_balance(tokenX.taddr, swapper, swapperBalanceX) in
    let () = FA12_helper.assert_user_balance(tokenY.taddr, swapper, swapperBalanceY) in
    // let (swapperBalX, swapperBalY) = Cfmm_helper.balanceOf swapper in
    // let (swapReceiverBalX, swapReceiverBalY) = Cfmm_helper.balanceOf swapReceiver in

    // SWAP 10000 X_TOKEN 
    let () = Test.set_source swapper in
    let x_amount_swap = 10000n in
    let param : Cfmm.x_to_y_param = Cfmm_helper.generate_x_to_y_param(x_amount_swap, 50n, swapReceiver) in
    let () = Cfmm_helper.x_to_y_success(param, 0tez, cfmm.contr) in
    let s_after = Test.get_storage cfmm.taddr in

    // VERIFY The contract's `sqrt_price` has moved accordingly.
    let fee_dx = Maths.calcSwapFee(feeBps, x_amount_swap) in
    let new_dx  = abs(x_amount_swap - fee_dx) in 
    let expected_sqrt_price = Maths.calcNewPriceX(s_init.sqrt_price, liquidity, new_dx) in
    let () = assert(s_after.sqrt_price.x80 = expected_sqrt_price) in 

    // VERIFY fee growth
    let expected_fee =  Bitwise.shift_left (fee_dx) 128n in 
    let expected_fee_growth = expected_fee / liquidity in
    let () = assert(s_after.fee_growth.x.x128 = expected_fee_growth) in

    // VERIFY expected DY
    let expectedDy = Maths.calcReceivedY(s_init.sqrt_price.x80, expected_sqrt_price, liquidity, effectiveProtoFeeBps) in

    // VERIFY TOKEN TRANSFER
    let () = ExtendedFA2_helper.assert_user_balance(tokenX.taddr, cfmm.addr, expected_x_initial_balance_for_10000000 + x_amount_swap) in
    let () = ExtendedFA2_helper.assert_user_balance(tokenX.taddr, swapper, abs(swapperBalanceX - x_amount_swap)) in
    // y_received = (sqrt_price - sqrt_price_new) * liquidity / 2^80 
    //            = ((1208914576709065780394418 - 1208925819614629174706176) * 10000000) / 2^80  
    //            = -92.999135108
    let expected_y_received_for_100 = expectedDy in // 92n in
    let () = FA12_helper.assert_user_balance(tokenY.taddr, swapReceiver, expected_y_received_for_100) in
    let () = FA12_helper.assert_user_balance(tokenY.taddr, cfmm.addr, abs(expected_y_initial_balance_for_10000000 - expected_y_received_for_100)) in

    // // VERIFY FEES: `feeReceiver` receives the expected fees.
    //collectFees cfmm feeReceiver 0 liquidityProvider => update_position
    let () = ExtendedFA2_helper.assert_user_balance(tokenX.taddr, feeReceiver, 0n) in
    // COLLECT FEES
    let () = Cfmm_helper.collectFees(liquidityProvider, 0n, feeReceiver, cfmm.contr) in
    
    let expected_fee = Maths.calcSwapFee(feeBps, x_amount_swap) in 
    let () = ExtendedFA2_helper.assert_user_balance_in_range(tokenX.taddr, feeReceiver, expected_fee, 1n) in
    FA12_helper.assert_user_balance(tokenY.taddr, feeReceiver, 0n)

// test_many_small_swaps
let test_many_small_swaps_equivalent_to_1_big_swap =
    let feeBps = 0n in    //fees would impact too much
    let protoFeeBps = 0n in
    let effectiveProtoFeeBps = if config.y = CTEZ then protoFeeBps else 0n in
    let liquidity = 10_000_000n in 
    let lowerTickIndex = -1000 in
    let upperTickIndex = 1000 in
    
    let swapperBalanceX : nat = 1000000n in
    let swapperBalanceY : nat = swapperBalanceX in
    let accounts = Bootstrap.boot_accounts() in
    let (liquidityProvider, swapper, swapReceiver, _feeReceiver, _) = accounts in
    // FOR CFMM1
    // Deploy contracts CFMM1 & tokenX and token Y
    let tokenX = Bootstrap.boot_token(0n, 0tez, liquidityProvider, liquidity + swapperBalanceX) in
    let tokenY = Bootstrap.boot_token_fa12(0n, 0tez, liquidityProvider, liquidity + swapperBalanceY) in
    let cfmm1 = Bootstrap.boot_cfmm_by_config(config, tokenX.addr, tokenY.addr, 1n, feeBps, effectiveProtoFeeBps, 0tez) in
    // Transfer 1_000_000 (token X and Y) to swapper
    let () = Test.set_source liquidityProvider in
    let () = ExtendedFA2_helper.transfer_success([{from_=liquidityProvider; txs=[{to_=swapper; token_id=0n; amount=swapperBalanceX}]}], tokenX.contr) in
    let () = FA12_helper.transfer_success((liquidityProvider, (swapper, swapperBalanceY)), tokenY.contr) in
    let () = ExtendedFA2_helper.assert_user_balance(tokenX.taddr, swapper, swapperBalanceX) in
    let () = FA12_helper.assert_user_balance(tokenY.taddr, swapper, swapperBalanceY) in
    // update_operators
    let () = Test.set_source liquidityProvider in
    let () = ExtendedFA2_helper.update_operators_success([Add_operator({owner=liquidityProvider; operator=cfmm1.addr; token_id=0n})], tokenX.contr) in
    let () = FA12_helper.approve_success((cfmm1.addr, liquidity), tokenY.contr) in
    let () = Test.set_source swapper in
    let () = ExtendedFA2_helper.update_operators_success([Add_operator({owner=swapper; operator=cfmm1.addr; token_id=0n})], tokenX.contr) in
    let () = FA12_helper.approve_success((cfmm1.addr, swapperBalanceY), tokenY.contr) in
    // for CFMM2
    // Deploy contracts CFMM1 & tokenX and token Y
    let tokenX2 = Bootstrap.boot_token(0n, 0tez, liquidityProvider, liquidity + swapperBalanceX) in
    let tokenY2 = Bootstrap.boot_token_fa12(0n, 0tez, liquidityProvider, liquidity + swapperBalanceY) in
    let cfmm2 = Bootstrap.boot_cfmm_by_config(config, tokenX2.addr, tokenY2.addr, 1n, feeBps, effectiveProtoFeeBps, 0tez) in
    // Transfer 1_000_000 (token X and Y) to swapper
    let () = Test.set_source liquidityProvider in
    let () = ExtendedFA2_helper.transfer_success([{from_=liquidityProvider; txs=[{to_=swapper; token_id=0n; amount=swapperBalanceX}]}], tokenX2.contr) in
    let () = FA12_helper.transfer_success((liquidityProvider, (swapper, swapperBalanceY)), tokenY2.contr) in
    let () = ExtendedFA2_helper.assert_user_balance(tokenX2.taddr, swapper, swapperBalanceX) in
    let () = FA12_helper.assert_user_balance(tokenY2.taddr, swapper, swapperBalanceY) in
    // update_operators
    let () = Test.set_source liquidityProvider in
    let () = ExtendedFA2_helper.update_operators_success([Add_operator({owner=liquidityProvider; operator=cfmm2.addr; token_id=0n})], tokenX2.contr) in
    let () = FA12_helper.approve_success((cfmm2.addr, liquidity), tokenY2.contr) in
    let () = Test.set_source swapper in
    let () = ExtendedFA2_helper.update_operators_success([Add_operator({owner=swapper; operator=cfmm2.addr; token_id=0n})], tokenX2.contr) in
    let () = FA12_helper.approve_success((cfmm2.addr, swapperBalanceY), tokenY2.contr) in

    // SET POSITION on CFMM1
    let () = Test.set_source liquidityProvider in
    let param : Cfmm.set_position_param = Cfmm_helper.generate_set_position_param(liquidity, (lowerTickIndex, upperTickIndex)) in
    let () = Cfmm_helper.set_position_success(param, 0tez, cfmm1.contr) in
    let () = Cfmm_helper.assert_liquidity(cfmm1.taddr, liquidity) in
    let expected_x_initial_balance_for_10000000 = 487706n in
    let () = ExtendedFA2_helper.assert_user_balance(tokenX.taddr, cfmm1.addr, expected_x_initial_balance_for_10000000) in 
    let () = ExtendedFA2_helper.assert_user_balance(tokenX.taddr, liquidityProvider, abs(10000000 - expected_x_initial_balance_for_10000000)) in 
    let expected_y_initial_balance_for_10000000 = 487706n in
    let () = FA12_helper.assert_user_balance(tokenY.taddr, cfmm1.addr, expected_y_initial_balance_for_10000000) in 
    let () = FA12_helper.assert_user_balance(tokenY.taddr, liquidityProvider, abs(10000000 - expected_y_initial_balance_for_10000000)) in 

    let s_init_cfmm1 = Test.get_storage cfmm1.taddr in
    let () = ExtendedFA2_helper.assert_user_balance(tokenX.taddr, swapReceiver, 0n) in
    let () = ExtendedFA2_helper.assert_user_balance(tokenX.taddr, swapper, swapperBalanceX) in
    let () = FA12_helper.assert_user_balance(tokenY.taddr, swapper, swapperBalanceY) in
    // let (swapperBalX, swapperBalY) = Cfmm_helper.balanceOf swapper in
    // let (swapReceiverBalX, swapReceiverBalY) = Cfmm_helper.balanceOf swapReceiver in


    // SET POSITION on CFMM2
    let () = Test.set_source liquidityProvider in
    let param : Cfmm.set_position_param = Cfmm_helper.generate_set_position_param(liquidity, (lowerTickIndex, upperTickIndex)) in
    let () = Cfmm_helper.set_position_success(param, 0tez, cfmm2.contr) in
    let () = Cfmm_helper.assert_liquidity(cfmm2.taddr, liquidity) in
    let expected_x_initial_balance_for_10000000 = 487706n in
    let () = ExtendedFA2_helper.assert_user_balance(tokenX2.taddr, cfmm2.addr, expected_x_initial_balance_for_10000000) in 
    let () = ExtendedFA2_helper.assert_user_balance(tokenX2.taddr, liquidityProvider, abs(10000000 - expected_x_initial_balance_for_10000000)) in 
    let expected_y_initial_balance_for_10000000 = 487706n in
    let () = FA12_helper.assert_user_balance(tokenY2.taddr, cfmm2.addr, expected_y_initial_balance_for_10000000) in 
    let () = FA12_helper.assert_user_balance(tokenY2.taddr, liquidityProvider, abs(10000000 - expected_y_initial_balance_for_10000000)) in 
    let s_init_cfmm2 = Test.get_storage cfmm2.taddr in


    // SWAP 10 times 10 X_TOKEN on CFMM1
    let () = Test.set_source swapper in
    let swap_count = 10n in
    let x_amount_swap = 10n in
    let apply_x_to_y(amount_swap, tick_spacing, receiver: nat * nat * address) : test_exec_result = 
        let param : Cfmm.x_to_y_param = Cfmm_helper.generate_x_to_y_param(amount_swap, tick_spacing, receiver) in
        let r = Cfmm_helper.x_to_y(param, 0tez, cfmm1.contr) in
        let () = Assert.tx_success r in
        r
    in
    let gen_args(_count : nat) : (nat * nat * address) = (x_amount_swap, 1n, swapReceiver) in
    let ret : test_exec_result list = ([]: test_exec_result list) in
    let _rrrr = Utils.repeat(swap_count, apply_x_to_y, gen_args, ret) in
    let s_after_cfmm1 = Test.get_storage cfmm1.taddr in


    // SWAP 100 X_TOKEN on CFMM2
    let () = Test.set_source swapper in
    let x_amount_swap_cfmm2 = 100n in
    let param : Cfmm.x_to_y_param = Cfmm_helper.generate_x_to_y_param(x_amount_swap_cfmm2, 50n, swapReceiver) in
    let () = Cfmm_helper.x_to_y_success(param, 0tez, cfmm2.contr) in 
    let s_after_cfmm2 = Test.get_storage cfmm2.taddr in

    // VERIFY The contract's `sqrt_price` has moved accordingly.
    // The two storages should be mostly identical.
    // The price might be slightly different, due to the compounding of rounding errors,
    // so we take some precision away to account for this difference.
    let sqrt_price_after_cfmm1_70 = Bitwise.shift_right s_after_cfmm1.sqrt_price.x80 10n in
    let sqrt_price_after_cfmm2_70 = Bitwise.shift_right s_after_cfmm2.sqrt_price.x80 10n in
    let () = assert(sqrt_price_after_cfmm1_70 = sqrt_price_after_cfmm2_70) in

    // VERIFY curTickIndex
    let () = assert(s_after_cfmm1.cur_tick_index = s_after_cfmm2.cur_tick_index) in

    // VERIFY fee growth
    let () = assert(s_after_cfmm1.fee_growth.x.x128 = s_after_cfmm2.fee_growth.x.x128) in

    // VERIFY expected DY
    let expectedDy_cfmm1 = Maths.calcReceivedY(s_init_cfmm1.sqrt_price.x80, s_after_cfmm1.sqrt_price.x80, liquidity, effectiveProtoFeeBps) in
    let expectedDy_cfmm2 = Maths.calcReceivedY(s_init_cfmm2.sqrt_price.x80, s_after_cfmm2.sqrt_price.x80, liquidity, effectiveProtoFeeBps) in
    let () = assert(expectedDy_cfmm1 = expectedDy_cfmm2) in

    // VERIFY TOKEN TRANSFER TOKEN_X
    let () = ExtendedFA2_helper.assert_user_balance(tokenX.taddr, cfmm1.addr, expected_x_initial_balance_for_10000000 + x_amount_swap * swap_count) in
    let () = ExtendedFA2_helper.assert_user_balance(tokenX.taddr, swapper, abs(swapperBalanceX - (x_amount_swap * swap_count))) in
    let () = ExtendedFA2_helper.assert_user_balance(tokenX2.taddr, cfmm2.addr, expected_x_initial_balance_for_10000000 + x_amount_swap_cfmm2) in
    let () = ExtendedFA2_helper.assert_user_balance(tokenX2.taddr, swapper, abs(swapperBalanceX - x_amount_swap_cfmm2)) in

    // VERIFY TOKEN TRANSFER TOKEN_Y
    // FOR CFMM1
    let expected_y_received_for_100 = expectedDy_cfmm1 in // 90n
    // Due to `dy` being rounded down, it's possible the swapper loses *up to* 1 Y token
    // on every swap.
    let () = FA12_helper.assert_user_balance_in_range(tokenY.taddr, swapReceiver, expected_y_received_for_100, swap_count) in
    let () = FA12_helper.assert_user_balance_in_range(tokenY.taddr, cfmm1.addr, abs(expected_y_initial_balance_for_10000000 - (expected_y_received_for_100)), swap_count) in
    // FOR CFMM2
    let expected_y_received_for_100 = expectedDy_cfmm2 in // 99n
    let () = FA12_helper.assert_user_balance(tokenY2.taddr, swapReceiver, expected_y_received_for_100) in
    FA12_helper.assert_user_balance(tokenY2.taddr, cfmm2.addr, abs(expected_y_initial_balance_for_10000000 - (expected_y_received_for_100)))

let test_crossing_tick =
    let feeBps = 200n in
    let protoFeeBps = 700n in
    let effectiveProtoFeeBps = if config.y = CTEZ then protoFeeBps else 0n in
    let liquidity = 1_000_000n in 
    let lowerTickIndex = -1000 in
    let upperTickIndex = 1000 in

    let nb_set_position = 10n in
    let total_liquidity : nat = liquidity * nb_set_position in

    let swapperBalanceX : nat = 1000000n in
    let swapperBalanceY : nat = swapperBalanceX in
    let accounts = Bootstrap.boot_accounts() in
    let (liquidityProvider, swapper, swapReceiver, feeReceiver1, feeReceiver2) = accounts in
    // Deploy contracts
    let tokenX = Bootstrap.boot_token(0n, 0tez, liquidityProvider, total_liquidity + swapperBalanceX) in
    let tokenY = Bootstrap.boot_token_fa12(0n, 0tez, liquidityProvider, total_liquidity + swapperBalanceY) in
    let cfmm1 = Bootstrap.boot_cfmm_by_config(config, tokenX.addr, tokenY.addr, 1n, feeBps, effectiveProtoFeeBps, 0tez) in
    let cfmm2 = Bootstrap.boot_cfmm_by_config(config, tokenX.addr, tokenY.addr, 1n, feeBps, effectiveProtoFeeBps, 0tez) in
    // Transfer 1_000_000 (token X and Y) to swapper
    let () = Test.set_source liquidityProvider in
    let () = ExtendedFA2_helper.transfer_success([{from_=liquidityProvider; txs=[{to_=swapper; token_id=0n; amount=swapperBalanceX}]}], tokenX.contr) in
    let () = FA12_helper.transfer_success((liquidityProvider, (swapper, swapperBalanceY)), tokenY.contr) in
    let () = ExtendedFA2_helper.assert_user_balance(tokenX.taddr, swapper, swapperBalanceX) in
    let () = FA12_helper.assert_user_balance(tokenY.taddr, swapper, swapperBalanceY) in
    // update_operators
    let () = Test.set_source liquidityProvider in
    let () = ExtendedFA2_helper.update_operators_success([Add_operator({owner=liquidityProvider; operator=cfmm1.addr; token_id=0n})], tokenX.contr) in
    let () = FA12_helper.approve_success((cfmm1.addr, total_liquidity), tokenY.contr) in
    let () = ExtendedFA2_helper.update_operators_success([Add_operator({owner=liquidityProvider; operator=cfmm2.addr; token_id=0n})], tokenX.contr) in
    let () = FA12_helper.approve_success((cfmm2.addr, total_liquidity), tokenY.contr) in

    let () = Test.set_source swapper in
    let () = ExtendedFA2_helper.update_operators_success([Add_operator({owner=swapper; operator=cfmm1.addr; token_id=0n})], tokenX.contr) in
    let () = FA12_helper.approve_success((cfmm1.addr, swapperBalanceY), tokenY.contr) in
    let () = ExtendedFA2_helper.update_operators_success([Add_operator({owner=swapper; operator=cfmm2.addr; token_id=0n})], tokenX.contr) in
    let () = FA12_helper.approve_success((cfmm2.addr, swapperBalanceY), tokenY.contr) in

    // INCREASE_OBSERVATION_COUNT : Add some slots to the buffers to make the tests more meaningful.
    let param : Cfmm.increase_observation_count_param = Cfmm_helper.generate_increase_observation_count_param(10n) in
    let () = Cfmm_helper.increase_observation_count_success(param, 0tez, cfmm1.contr) in
    let () = Cfmm_helper.increase_observation_count_success(param, 0tez, cfmm2.contr) in

    // -- SET POSITION : Place 1 big position on CFMM1
    let param : Cfmm.set_position_param = Cfmm_helper.generate_set_position_param(liquidity, (lowerTickIndex, upperTickIndex)) in
    let () = Cfmm_helper.set_position_success(param, 0tez, cfmm1.contr) in

    // -- SET POSITION 10 times 1000000 X_TOKEN on CFMM2 on intervals [-1000; -600] [-600; -200] ... [600; 1000]
    let () = Test.set_source liquidityProvider in
    let apply_set_position(liq, low_tick, high_tick: nat * int * int) : (int * int) = 
        let t_set_position_temp_before = Tezos.get_now() in         
        // SET POSITION
        let param : Cfmm.set_position_param = Cfmm_helper.generate_set_position_param(liq, (low_tick, high_tick)) in
        let () = Cfmm_helper.set_position_success(param, 0tez, cfmm2.contr) in

        // return lower_tick and timestamp of set position
        let s_temp_cfmm_2 = Test.get_storage cfmm2.taddr in
        if (s_temp_cfmm_2.cur_tick_index.i >= low_tick) then
            (low_tick, (t_set_position_temp_before - (0: timestamp)))
        else
            (low_tick, 0)
    in
    let interval = abs(upperTickIndex - lowerTickIndex) / nb_set_position in
    let gen_args(count : nat) : (nat * int * int) =
        (liquidity, lowerTickIndex + interval * abs(count - 1n), lowerTickIndex + interval * count)
    in
    let expected_ticks_so = Utils.generic_repeat(nb_set_position, apply_set_position, gen_args, ([]: (int * int) list)) in
    // transfrom (int * int) list to (int, int) big_map
    let insert_one(acc, (k, v) : (Cfmm.tick_index, int) big_map * (int * int)) : (Cfmm.tick_index, int) big_map = 
        Big_map.add {i=k} v acc
    in
    let expected_ticks_so_map = List.fold insert_one expected_ticks_so (Big_map.empty : (Cfmm.tick_index, int) big_map) in
    // let () = Test.log("expected_ticks_so_map", expected_ticks_so_map) in

    // debug : display ticks (seconds_outside)
    // let s_after_set_position_cfmm_2 = Test.get_storage cfmm2.taddr in
    // let startTickIndex : Cfmm.tick_index = { i=-1048575 } in
    // let stop_condition(p: Cfmm.tick_index) : bool = (p.i = 1048576) in
    // let get_so(p: (Cfmm.tick_index * Cfmm.tick_state)) : (Cfmm.tick_index * nat) = (p.0, p.1.seconds_outside) in
    // let tick_list = Cfmm_helper.forAllTicks(([]: (Cfmm.tick_index * nat) list), startTickIndex, stop_condition, s_after_set_position_cfmm_2.ticks, get_so) in
    // let () = Test.log("real_ticks_so:", tick_list) in 

    // since s.liquidity is increased only when current tick index is between lower tick and upper tick, 
    // then only the set_position on [0, 200] will increase the global liquidity
    let () = Cfmm_helper.assert_liquidity(cfmm1.taddr, liquidity) in
    let () = Cfmm_helper.assert_liquidity(cfmm2.taddr, liquidity) in

    // CHECK INVARIANTS
    // let () = Test.log("check_all_invariants") in
    let _ : unit = Cfmm_helper.check_all_invariants(cfmm1.taddr, cfmm1.addr) in
    let _ : unit = Cfmm_helper.check_all_invariants(cfmm2.taddr, cfmm2.addr) in

    let cfmm1InitialBalanceX = ExtendedFA2_helper.get_user_balance(tokenX.taddr, cfmm1.addr) in
    let cfmm1InitialBalanceY = FA12_helper.get_user_balance(tokenY.taddr, cfmm1.addr) in
    let cfmm2InitialBalanceX = ExtendedFA2_helper.get_user_balance(tokenX.taddr, cfmm2.addr) in
    let cfmm2InitialBalanceY = FA12_helper.get_user_balance(tokenY.taddr, cfmm2.addr) in


    // SWAP 200 X_TOKEN 
    // Place a small swap to move the tick past 0 and advance the time to fill the
    // buffer with _something_ other than zeros.
    let () = Test.log("swap 200 X on CFMM1") in
    let () = Test.set_source swapper in
    let x_amount_swap = 200n in
    let param : Cfmm.x_to_y_param = Cfmm_helper.generate_x_to_y_param(x_amount_swap, 1n, swapReceiver) in
    let () = Cfmm_helper.x_to_y_success(param, 0tez, cfmm1.contr) in

    let () = Test.log("swap 200 X on CFMM2") in
    let () = Test.set_source swapper in
    let x_amount_swap = 200n in
    let param : Cfmm.x_to_y_param = Cfmm_helper.generate_x_to_y_param(x_amount_swap, 1n, swapReceiver) in
    let () = Cfmm_helper.x_to_y_success(param, 0tez, cfmm2.contr) in

    // TODO (NOT NEEDED TO ADVANCE TIME BECAUSE EACH CONTRACT INVOCATION DOES IT)
    // -- Advance the time a few secs to make sure accumulators
    // -- like `seconds_per_liquidity_cumulative` change to non-zero values.
    // let _waitTime_1 = Cfmm_helper.advanceTime(cfmm1.contr) in
    // let _waitTime_2 = Cfmm_helper.advanceTime(cfmm2.contr) in

    let _s_initial_cfmm_1 = Test.get_storage cfmm1.taddr in
    let _s_initial_cfmm_2 = Test.get_storage cfmm2.taddr in

    // DEBUG
    // let () = Test.log("CUR TICK INDEX (cfmm1):", _s_initial_cfmm_1.cur_tick_index) in 
    // let () = Test.log("CUR TICK INDEX (cfmm2):", _s_initial_cfmm_2.cur_tick_index) in 

    // SWAP 50000 X_TOKEN 
    // Place 1 big swap to push the tick all the way down to `lowerTickIndex`
    let () = Test.log("swap 50000 X on CFMM1") in
    let () = Test.set_source swapper in
    let x_amount_swap = 50000n in
    let param : Cfmm.x_to_y_param = Cfmm_helper.generate_x_to_y_param(x_amount_swap, 1n, swapReceiver) in
    let () = Cfmm_helper.x_to_y_success(param, 0tez, cfmm1.contr) in

    // display ticks (seconds_outside)
    // let startTickIndex : Cfmm.tick_index = { i=-1048575 } in
    // let stop_condition(p: Cfmm.tick_index) : bool = (p.i = 1048576) in
    // let get_so(p: (Cfmm.tick_index * Cfmm.tick_state)) : (Cfmm.tick_index * nat) = (p.0, p.1.seconds_outside) in
    // let tick_list = Cfmm_helper.forAllTicks(([]: (Cfmm.tick_index * nat) list), startTickIndex, stop_condition, _s_initial_cfmm_2.ticks, get_so) in
    // let () = Test.log("ticks before SWAP 5000 (cfmm2):", tick_list) in 

    let t_swap_5000_before = Tezos.get_now() in 
    // let () = Test.log("t_swap_5000_before", t_swap_5000_before) in
    let () = Test.log("swap 50000 X on CFMM2") in
    let () = Cfmm_helper.x_to_y_success(param, 0tez, cfmm2.contr) in

    let s_after_cfmm1 = Test.get_storage cfmm1.taddr in
    let s_after_cfmm2 = Test.get_storage cfmm2.taddr in

    // DEBUG
    // let () = Test.log("CUR TICK INDEX (cfmm1):", s_after_cfmm1.cur_tick_index) in 
    // let () = Test.log("CUR TICK INDEX (cfmm2):", s_after_cfmm2.cur_tick_index) in 

    // display ticks (seconds_outside)
    // let startTickIndex : Cfmm.tick_index = { i=-1048575 } in
    // let stop_condition(p: Cfmm.tick_index) : bool = (p.i = 1048576) in
    // let get_so(p: (Cfmm.tick_index * Cfmm.tick_state)) : (Cfmm.tick_index * nat) = (p.0, p.1.seconds_outside) in
    // let tick_list = Cfmm_helper.forAllTicks(([]: (Cfmm.tick_index * nat) list), startTickIndex, stop_condition, s_after_cfmm2.ticks, get_so) in
    // let () = Test.log("ticks after SWAP 5000 (cfmm2):", tick_list) in 

    // let () = Test.log("check_all_invariants") in
    let _ : unit = Cfmm_helper.check_all_invariants(cfmm1.taddr, cfmm1.addr) in
    let _ : unit = Cfmm_helper.check_all_invariants(cfmm2.taddr, cfmm2.addr) in

    //  sCurTickIndex s_after `isInRange` lowerTickIndex $ (0, 50)
    let () = assert(((lowerTickIndex - 0) <= s_after_cfmm1.cur_tick_index.i) && (s_after_cfmm1.cur_tick_index.i < lowerTickIndex + 50)) in

    // -- Current tick should be the same.
    let () = assert(s_after_cfmm1.cur_tick_index = s_after_cfmm2.cur_tick_index) in

    // -- "Fee growth" should be fairly similar.
    // -- It can be slightly higher for the 2nd contract,
    // -- because each time we cross an initialized tick, the fee can be rounded up once.
    // -- Because in the 2nd scenario we're crossing 10 ticks, we allow for a difference of up to 10 extra X tokens in fees.
    let feeGrowthX_cfmm1 = s_after_cfmm1.fee_growth.x in
    let feeGrowthY_cfmm1 = s_after_cfmm1.fee_growth.y in
    let feeGrowthX_cfmm2 = s_after_cfmm2.fee_growth.x in
    let feeGrowthY_cfmm2 = s_after_cfmm2.fee_growth.y in
    let () = assert(feeGrowthY_cfmm1 = {x128=0n}) in
    let () = assert(feeGrowthY_cfmm2 = {x128=0n}) in

    // TODO !!  margin error very big !!
    let marginOfError = (Bitwise.shift_left 10n 128n) / liquidity in
    // let () = Test.log("marginOfError", marginOfError) in
    // let () = Test.log(feeGrowthX_cfmm2.x128) in
    let () = assert((feeGrowthX_cfmm1.x128 + 0n <= feeGrowthX_cfmm2.x128) && (feeGrowthX_cfmm2.x128 < feeGrowthX_cfmm1.x128 + marginOfError)) in

    // cfmm balances
    let cfmm1FinalBalanceX = ExtendedFA2_helper.get_user_balance(tokenX.taddr, cfmm1.addr) in
    let cfmm1FinalBalanceY = FA12_helper.get_user_balance(tokenY.taddr, cfmm1.addr) in
    let cfmm2FinalBalanceX = ExtendedFA2_helper.get_user_balance(tokenX.taddr, cfmm2.addr) in
    let cfmm2FinalBalanceY = FA12_helper.get_user_balance(tokenY.taddr, cfmm2.addr) in

    // -- The two contract should have received the exact same amount of X tokens
    let cfmm1_diff_x = cfmm1FinalBalanceX - cfmm1InitialBalanceX in 
    let cfmm2_diff_x = cfmm2FinalBalanceX - cfmm2InitialBalanceX in 
    let () = assert(cfmm1_diff_x = cfmm2_diff_x) in
    // -- The 2nd contract may have given out fewer Y tokens (due to the potential increase in fees)
    let cfmm1_diff_y = cfmm1FinalBalanceY - cfmm1InitialBalanceY in 
    let cfmm2_diff_y = cfmm2FinalBalanceY - cfmm2InitialBalanceY in 
    let () = assert((cfmm1_diff_y + 0n <= cfmm2_diff_y) && (cfmm2_diff_y + 0n < cfmm1_diff_y + 10n))  in
 
    // -- Collected fees should be fairly similar.
    // -- As explained above, the contract may charge up to 10 extra tokens.
    // -- However, when an LP collects fees for a position, the distribution of fees can be rounded down,
    // -- so we allow for a margin of error of +/-10 X tokens.
    // COLLECT ALL FEES
    let () = Cfmm_helper.collectAllFees(cfmm1.taddr, cfmm1.contr, feeReceiver1) in
    let () = Cfmm_helper.collectAllFees(cfmm2.taddr, cfmm2.contr, feeReceiver2) in

    let feeReceiver1BalanceX = ExtendedFA2_helper.get_user_balance(tokenX.taddr, feeReceiver1) in
    let feeReceiver1BalanceY = FA12_helper.get_user_balance(tokenY.taddr, feeReceiver1) in
    let feeReceiver2BalanceX = ExtendedFA2_helper.get_user_balance(tokenX.taddr, feeReceiver2) in
    let feeReceiver2BalanceY = FA12_helper.get_user_balance(tokenY.taddr, feeReceiver2) in
    let () = assert(feeReceiver1BalanceY = 0n) in 
    let () = assert(feeReceiver2BalanceY = 0n) in 
    let () = assert((feeReceiver1BalanceX - 10n <= int(feeReceiver2BalanceX)) && (feeReceiver2BalanceX < feeReceiver1BalanceX + 10n)) in

    // TODO
    // -- The global accumulators of both contracts should be the same.
    // let () = Test.log("check cumulatives_buffer") in 
    // let () = Test.log(s_after_cfmm1.cumulatives_buffer) in
    // let () = Test.log(s_after_cfmm2.cumulatives_buffer) in
    // let () = assert(s_after_cfmm1.cumulatives_buffer = s_after_cfmm2.cumulatives_buffer) in

    // Check that the ticks' states were updated correctly after being crossed.
    let rec assert_tick_indexes(acc, current, interval : (Cfmm.tick_index * Cfmm.tick_state) list * Cfmm.tick_index * nat) : (Cfmm.tick_index * Cfmm.tick_state) list =
        if (current.i >= 0) then
            acc
        else
            let () = Test.log("current", current) in 
            match Big_map.find_opt current s_after_cfmm2.ticks with
            | Some ts -> 
                // retrieve timestamp of the set position (for current tick)
                let expected_so = match Big_map.find_opt current expected_ticks_so_map with 
                | Some v -> v
                | None -> Test.failwith("expected tick so not found")
                in 
                let wait_between_pos_swap = t_swap_5000_before - expected_so - (0: timestamp) in                
                let wait_between_pos_swap_x128 = Bitwise.shift_left (abs(wait_between_pos_swap)) 128n in
                let expected_splo = abs(wait_between_pos_swap_x128 / int(liquidity)) in
                // There may be a +1/-1 error due to the rounding of the seconds_per_liquidity_outside
                let _ : unit = assert(int(ts.seconds_outside) = wait_between_pos_swap) in 
                // TODO : diff should be 2n
                let _ : unit = assert(abs(ts.seconds_per_liquidity_outside.x128 - expected_splo) <= 3n) in 
                
                let () = Test.log(ts.tick_cumulative_outside) in 
                let () = Test.log(wait_between_pos_swap * _s_initial_cfmm_2.cur_tick_index.i) in 
                // TODO : 
                // let _ : unit = assert(ts.tick_cumulative_outside = waiTime_x128.x128 * s_after_cfmm1.cur_tick_index.i) in                
                let _ : unit = assert((ts.fee_growth_outside.x.x128 <> 0n) || (ts.fee_growth_outside.y.x128 <> 0n)) in 
                assert_tick_indexes((current,ts)::acc, {i=current.i + interval}, interval)
            | None -> Test.failwith "[gen_tick_indexes]: unknown tick"
    in
    let _ticks_lower_0 = assert_tick_indexes(([]: (Cfmm.tick_index * Cfmm.tick_state) list), {i=lowerTickIndex+interval}, interval) in
    let () = Test.log("3 TODO remaining") in
    ()


// test_fee_split
let test_fee_split = 
    let feeBps = 5000n in // 50%
    let protoFeeBps = 700n in
    let effectiveProtoFeeBps = if config.y = CTEZ then protoFeeBps else 0n in
    let liquidity = 1_000_000n in // per interval
    let nb_interval = 2n in
    let total_liquidity = liquidity * nb_interval in

    let swapperBalanceX : nat = 20000n in
    let swapperBalanceY : nat = 1000n in
    let accounts = Bootstrap.boot_accounts() in
    let (liquidityProvider, swapper, _swapReceiver, feeReceiver1, feeReceiver2) = accounts in
    // Deploy contracts
    let tokenX = Bootstrap.boot_token(0n, 0tez, liquidityProvider, total_liquidity + swapperBalanceX) in
    let tokenY = Bootstrap.boot_token_fa12(0n, 0tez, liquidityProvider, total_liquidity + swapperBalanceY) in
    let cfmm = Bootstrap.boot_cfmm_by_config(config, tokenX.addr, tokenY.addr, 1n, feeBps, effectiveProtoFeeBps, 0tez) in
    // Transfer 20000 token X and 1000 token Y to swapper
    let () = Test.set_source liquidityProvider in
    let () = ExtendedFA2_helper.transfer_success([{from_=liquidityProvider; txs=[{to_=swapper; token_id=0n; amount=swapperBalanceX}]}], tokenX.contr) in
    let () = FA12_helper.transfer_success((liquidityProvider, (swapper, swapperBalanceY)), tokenY.contr) in
    let () = ExtendedFA2_helper.assert_user_balance(tokenX.taddr, swapper, swapperBalanceX) in
    let () = FA12_helper.assert_user_balance(tokenY.taddr, swapper, swapperBalanceY) in
    // update_operators
    let () = Test.set_source liquidityProvider in
    let () = ExtendedFA2_helper.update_operators_success([Add_operator({owner=liquidityProvider; operator=cfmm.addr; token_id=0n})], tokenX.contr) in
    let () = FA12_helper.approve_success((cfmm.addr, liquidity), tokenY.contr) in
    let () = Test.set_source swapper in
    let () = ExtendedFA2_helper.update_operators_success([Add_operator({owner=swapper; operator=cfmm.addr; token_id=0n})], tokenX.contr) in
    let () = FA12_helper.approve_success((cfmm.addr, swapperBalanceY), tokenY.contr) in

    // SET POSITION (1000000) on [-100;100]
    let () = Test.set_source liquidityProvider in
    let param : Cfmm.set_position_param = Cfmm_helper.generate_set_position_param(liquidity, (-100, 100)) in
    let () = Cfmm_helper.set_position_success(param, 0tez, cfmm.contr) in
    let () = Cfmm_helper.assert_liquidity(cfmm.taddr, liquidity) in

    // SET POSITION (1000000) on [-200;-100]
    let () = Test.set_source liquidityProvider in
    let param : Cfmm.set_position_param = Cfmm_helper.generate_set_position_param(liquidity, (-200, -100)) in
    let () = Cfmm_helper.set_position_success(param, 0tez, cfmm.contr) in
    let () = Cfmm_helper.assert_liquidity(cfmm.taddr, liquidity) in // liquidity of the active interval

    // SWAP 1000 Y 
    // Place a small y-to-x swap.
    // It's small enough to be executed within the [-100, 100] range,
    // so the Y fee is paid to position1 only.
    let () = Test.set_source swapper in
    let y_amount_swap = swapperBalanceY in
    let param : Cfmm.y_to_x_param = Cfmm_helper.generate_y_to_x_param(y_amount_swap, 0n, swapper) in
    let () = Cfmm_helper.y_to_x_success(param, 0tez, cfmm.contr) in

    // SWAP 20000 X 
    // Place a big x-to-y swap.
    // It's big enough to cross from the [-100, 100] range into the [-200, -100] range,
    // so the X fee is paid to both position1 and position2.
    let x_amount_swap = swapperBalanceX in
    let param : Cfmm.x_to_y_param = Cfmm_helper.generate_x_to_y_param(x_amount_swap, 0n, swapper) in
    let () = Cfmm_helper.x_to_y_success(param, 0tez, cfmm.contr) in

    // check invariants
    let _ : unit = Cfmm_helper.check_all_invariants(cfmm.taddr, cfmm.addr) in

    // position1 should have earned both X and Y fees.
    // COLLECT FEES
    let () = Cfmm_helper.collectFees(liquidityProvider, 0n, feeReceiver1, cfmm.contr) in
    let feeReceiver1_x = ExtendedFA2_helper.get_user_balance(tokenX.taddr, feeReceiver1) in
    let feeReceiver1_y = FA12_helper.get_user_balance(tokenY.taddr, feeReceiver1) in
    let () = Test.log(feeReceiver1_x) in
    let () = Test.log(feeReceiver1_y) in
    let () = assert(feeReceiver1_x <> 0n) in
    let () = assert(feeReceiver1_y <> 0n) in

    // position2 should have earned X fees only.
    let () = Test.set_source liquidityProvider in
    let param_update : Cfmm.update_position_param = Cfmm_helper.generate_update_position_param(1n, 0, feeReceiver2, 500n, 750n) in
    let () = Cfmm_helper.update_position_success(param_update, 0tez, cfmm.contr) in
    let feeReceiver2_x = ExtendedFA2_helper.get_user_balance(tokenX.taddr, feeReceiver2) in
    let () = assert(feeReceiver2_x <> 0n) in
    let () = FA12_helper.assert_user_balance(tokenY.taddr, feeReceiver2, 0n) in
    ()


// test_must_exceed_min_dy
let test_must_exceed_min_dy =
    let feeBps = 700n in
    let protoFeeBps = 700n in
    let effectiveProtoFeeBps = if config.y = CTEZ then protoFeeBps else 0n in
    let liquidity = 10_000_000n in 
    let lowerTickIndex = -1000 in
    let upperTickIndex = 1000 in

    let swapperBalanceX : nat = 1n in
    let swapperBalanceY : nat = swapperBalanceX in
    let accounts = Bootstrap.boot_accounts() in
    let (liquidityProvider, swapper, _swapReceiver, _feeReceiver, _) = accounts in
    // Deploy contracts
    let tokenX = Bootstrap.boot_token(0n, 0tez, liquidityProvider, liquidity + swapperBalanceX) in
    let tokenY = Bootstrap.boot_token_fa12(0n, 0tez, liquidityProvider, liquidity + swapperBalanceY) in
    let cfmm = Bootstrap.boot_cfmm_by_config(config, tokenX.addr, tokenY.addr, 1n, feeBps, effectiveProtoFeeBps, 0tez) in
    // Transfer 200 (token X and Y) to swapper
    let () = Test.set_source liquidityProvider in
    let () = ExtendedFA2_helper.transfer_success([{from_=liquidityProvider; txs=[{to_=swapper; token_id=0n; amount=swapperBalanceX}]}], tokenX.contr) in
    let () = FA12_helper.transfer_success((liquidityProvider, (swapper, swapperBalanceY)), tokenY.contr) in
    let () = ExtendedFA2_helper.assert_user_balance(tokenX.taddr, swapper, swapperBalanceX) in
    let () = FA12_helper.assert_user_balance(tokenY.taddr, swapper, swapperBalanceY) in
    // update_operators
    let () = Test.set_source liquidityProvider in
    let () = ExtendedFA2_helper.update_operators_success([Add_operator({owner=liquidityProvider; operator=cfmm.addr; token_id=0n})], tokenX.contr) in
    let () = FA12_helper.approve_success((cfmm.addr, liquidity), tokenY.contr) in
    let () = Test.set_source swapper in
    let () = ExtendedFA2_helper.update_operators_success([Add_operator({owner=swapper; operator=cfmm.addr; token_id=0n})], tokenX.contr) in
    let () = FA12_helper.approve_success((cfmm.addr, swapperBalanceY), tokenY.contr) in

    // SET POSITION (10000000) on [-1000;1000]
    let () = Test.set_source liquidityProvider in
    let param : Cfmm.set_position_param = Cfmm_helper.generate_set_position_param(liquidity, (lowerTickIndex, upperTickIndex)) in
    let () = Cfmm_helper.set_position_success(param, 0tez, cfmm.contr) in
    let () = Cfmm_helper.assert_liquidity(cfmm.taddr, liquidity) in

    // SWAP 1 will fail due to exceeding min_dy
    let () = Test.set_source swapper in
    let x_amount_swap = 1n in
    let param : Cfmm.x_to_y_param = Cfmm_helper.generate_x_to_y_param(x_amount_swap, 1000n, swapper) in
    let r = Cfmm_helper.x_to_y(param, 0tez, cfmm.contr) in
    Assert.min_dy_failure r Cfmm.smaller_than_min_asset_err


// test_fails_if_its_past_the_deadline
let test_fails_if_its_past_the_deadline =
    let feeBps = 700n in
    let protoFeeBps = 700n in
    let effectiveProtoFeeBps = if config.y = CTEZ then protoFeeBps else 0n in
    let liquidity = 10_000_000n in 
    let lowerTickIndex = -1000 in
    let upperTickIndex = 1000 in

    let swapperBalanceX : nat = 10n in
    let swapperBalanceY : nat = swapperBalanceX in
    let accounts = Bootstrap.boot_accounts() in
    let (liquidityProvider, swapper, _swapReceiver, _feeReceiver, _) = accounts in
    // Deploy contracts
    let tokenX = Bootstrap.boot_token(0n, 0tez, liquidityProvider, liquidity + swapperBalanceX) in
    let tokenY = Bootstrap.boot_token_fa12(0n, 0tez, liquidityProvider, liquidity + swapperBalanceY) in
    let cfmm = Bootstrap.boot_cfmm_by_config(config, tokenX.addr, tokenY.addr, 1n, feeBps, effectiveProtoFeeBps, 0tez) in
    // Transfer 200 (token X and Y) to swapper
    let () = Test.set_source liquidityProvider in
    let () = ExtendedFA2_helper.transfer_success([{from_=liquidityProvider; txs=[{to_=swapper; token_id=0n; amount=swapperBalanceX}]}], tokenX.contr) in
    let () = FA12_helper.transfer_success((liquidityProvider, (swapper, swapperBalanceY)), tokenY.contr) in
    let () = ExtendedFA2_helper.assert_user_balance(tokenX.taddr, swapper, swapperBalanceX) in
    let () = FA12_helper.assert_user_balance(tokenY.taddr, swapper, swapperBalanceY) in
    // update_operators
    let () = Test.set_source liquidityProvider in
    let () = ExtendedFA2_helper.update_operators_success([Add_operator({owner=liquidityProvider; operator=cfmm.addr; token_id=0n})], tokenX.contr) in
    let () = FA12_helper.approve_success((cfmm.addr, liquidity), tokenY.contr) in
    let () = Test.set_source swapper in
    let () = ExtendedFA2_helper.update_operators_success([Add_operator({owner=swapper; operator=cfmm.addr; token_id=0n})], tokenX.contr) in
    let () = FA12_helper.approve_success((cfmm.addr, swapperBalanceY), tokenY.contr) in

    // SET POSITION (10000000) on [-1000;1000]
    let () = Test.set_source liquidityProvider in
    let param : Cfmm.set_position_param = Cfmm_helper.generate_set_position_param(liquidity, (lowerTickIndex, upperTickIndex)) in
    let () = Cfmm_helper.set_position_success(param, 0tez, cfmm.contr) in
    let () = Cfmm_helper.assert_liquidity(cfmm.taddr, liquidity) in

    // SWAP 1 will fail due to invalid deadline
    let () = Test.set_source swapper in
    let x_amount_swap = 1n in
    let param : Cfmm.x_to_y_param = Cfmm_helper.generate_x_to_y_param(x_amount_swap, 0n, swapper) in
    let param = { param with deadline=Tezos.get_now() - 1 } in
    let r = Cfmm_helper.x_to_y(param, 0tez, cfmm.contr) in
    Assert.deadline_failure r Cfmm.past_deadline_err


// test_swaps_are_noops_when_liquidity_is_zero
// -- After crossing into a 0-liquidity range, swaps are no-ops
let test_swaps_are_noops_when_liquidity_is_zero  =
    let feeBps = 700n in
    let protoFeeBps = 700n in
    let effectiveProtoFeeBps = if config.y = CTEZ then protoFeeBps else 0n in
    let liquidity = 10_000n in 
    let lowerTickIndex = -100 in
    let upperTickIndex = 100 in
    
    let swapperBalanceX : nat = 200n in
    let swapperBalanceY : nat = swapperBalanceX in
    let accounts = Bootstrap.boot_accounts() in
    let (liquidityProvider, swapper, swapReceiver, _feeReceiver, _) = accounts in
    // Deploy contracts
    let tokenX = Bootstrap.boot_token(0n, 0tez, liquidityProvider, 300n) in // 100 for set_position + 200 for swap
    let tokenY = Bootstrap.boot_token_fa12(0n, 0tez, liquidityProvider, 300n) in
    let cfmm = Bootstrap.boot_cfmm_by_config(config, tokenX.addr, tokenY.addr, 1n, feeBps, effectiveProtoFeeBps, 0tez) in
    // Transfer 200 (token X and Y) to swapper
    let () = Test.set_source liquidityProvider in
    let () = ExtendedFA2_helper.transfer_success([{from_=liquidityProvider; txs=[{to_=swapper; token_id=0n; amount=swapperBalanceX}]}], tokenX.contr) in
    let () = FA12_helper.transfer_success((liquidityProvider, (swapper, swapperBalanceY)), tokenY.contr) in
    let () = ExtendedFA2_helper.assert_user_balance(tokenX.taddr, swapper, swapperBalanceX) in
    let () = FA12_helper.assert_user_balance(tokenY.taddr, swapper, swapperBalanceY) in
    // update_operators
    let () = Test.set_source liquidityProvider in
    let () = ExtendedFA2_helper.update_operators_success([Add_operator({owner=liquidityProvider; operator=cfmm.addr; token_id=0n})], tokenX.contr) in
    let () = FA12_helper.approve_success((cfmm.addr, liquidity), tokenY.contr) in
    let () = Test.set_source swapper in
    let () = ExtendedFA2_helper.update_operators_success([Add_operator({owner=swapper; operator=cfmm.addr; token_id=0n})], tokenX.contr) in
    let () = FA12_helper.approve_success((cfmm.addr, swapperBalanceY), tokenY.contr) in

    // SET POSITION (10000)
    let () = Test.set_source liquidityProvider in
    let param : Cfmm.set_position_param = Cfmm_helper.generate_set_position_param(liquidity, (lowerTickIndex, upperTickIndex)) in
    let () = Cfmm_helper.set_position_success(param, 0tez, cfmm.contr) in
    let () = Cfmm_helper.assert_liquidity(cfmm.taddr, liquidity) in

    // Verify before SWAP
    let () = ExtendedFA2_helper.assert_user_balance(tokenX.taddr, swapReceiver, 0n) in
    let () = ExtendedFA2_helper.assert_user_balance(tokenX.taddr, swapper, swapperBalanceX) in
    let () = FA12_helper.assert_user_balance(tokenY.taddr, swapper, swapperBalanceY) in

    // Place a swap big enough to exhaust the position's liquidity
    // SWAP 200 X_TOKEN 
    // let () = Test.log("SWAP 200") in
    let () = Test.set_source swapper in
    let x_amount_swap = 200n in
    let param : Cfmm.x_to_y_param = Cfmm_helper.generate_x_to_y_param(x_amount_swap, 1n, swapReceiver) in
    let () = Cfmm_helper.x_to_y_success(param, 0tez, cfmm.contr) in
    // let () = Test.log(r) in

    // SWAP 100 X_TOKEN 
    let s_before = Test.get_storage cfmm.taddr in
    let initial_cfmm_balance_x = ExtendedFA2_helper.get_user_balance(tokenX.taddr, cfmm.addr) in
    let initial_cfmm_balance_y = FA12_helper.get_user_balance(tokenY.taddr, cfmm.addr) in
    let () = Test.log("Attempt SWAP 100") in
    let () = Test.set_source swapper in
    let x_amount_swap = 100n in
    let param : Cfmm.x_to_y_param = Cfmm_helper.generate_x_to_y_param(x_amount_swap, 0n, swapReceiver) in
    let () = Cfmm_helper.x_to_y_success(param, 0tez, cfmm.contr) in
    // let () = Test.log(r) in
    // Verify cfmm balances (token X and token Y) and cfmm storage
    let cfmm_balance_x = ExtendedFA2_helper.get_user_balance(tokenX.taddr, cfmm.addr) in
    let cfmm_balance_y = FA12_helper.get_user_balance(tokenY.taddr, cfmm.addr) in
    let () = assert(initial_cfmm_balance_x = cfmm_balance_x) in
    let () = assert(initial_cfmm_balance_y = cfmm_balance_y) in
    let s_after = Test.get_storage cfmm.taddr in
    let () = Cfmm_helper.assert_storage_equal(s_before, s_after) in // TODO (cumulative buffer not verified)
    let () = Test.log("1 TODO remaining") in
    ()


// test_push_cur_tick_index_just_below_witness
// -- Explanation:
// -- We have 2 positions: one currently in-range with boundaries at [-100, 100],
// -- and another currently out-of-range with boundaries at [-200, -100].
// --
// -- If we deposit 52 X tokens, the cur_tick_index would move to -100 but NOT cross it.
// --
// -- If we deposit 53 X tokens, we'll exhaust the first position's liquidity,
// -- and therefore cross the tick -100.
// -- After having crossed the tick, we'll have 1 X token left to swap.
// -- But since a 1 token fee will be charged, 0 X tokens will be
// -- deposited and 0 Y tokens will be withdrawn.
// --
// -- We want to make sure invariants are not broken when this edge case occurs.
let test_push_cur_tick_index_just_below_witness  =
    let feeBps = 200n in
    let protoFeeBps = 700n in
    let effectiveProtoFeeBps = if config.y = CTEZ then protoFeeBps else 0n in
    let postion_1_liquidity = 10_000n in 
    let postion_1_lowerTickIndex = -100 in
    let postion_1_upperTickIndex = 100 in
    let postion_2_liquidity = 30_000n in 
    let postion_2_lowerTickIndex = -200 in
    let postion_2_upperTickIndex = -100 in

    let swapperBalanceX : nat = 60n in
    let swapperBalanceY : nat = swapperBalanceX in
    let accounts = Bootstrap.boot_accounts() in
    let (liquidityProvider, swapper, swapReceiver, _feeReceiver, _) = accounts in
    // Deploy contracts
    let tokenX = Bootstrap.boot_token(0n, 0tez, liquidityProvider, postion_1_liquidity + postion_2_liquidity + swapperBalanceX) in
    let tokenY = Bootstrap.boot_token_fa12(0n, 0tez, liquidityProvider, postion_1_liquidity + postion_2_liquidity + swapperBalanceY) in
    let cfmm = Bootstrap.boot_cfmm_by_config(config, tokenX.addr, tokenY.addr, 1n, feeBps, effectiveProtoFeeBps, 0tez) in
    // Transfer 200 (token X and Y) to swapper
    let () = Test.set_source liquidityProvider in
    let () = ExtendedFA2_helper.transfer_success([{from_=liquidityProvider; txs=[{to_=swapper; token_id=0n; amount=swapperBalanceX}]}], tokenX.contr) in
    let () = FA12_helper.transfer_success((liquidityProvider, (swapper, swapperBalanceY)), tokenY.contr) in
    let () = ExtendedFA2_helper.assert_user_balance(tokenX.taddr, swapper, swapperBalanceX) in
    let () = FA12_helper.assert_user_balance(tokenY.taddr, swapper, swapperBalanceY) in
    // update_operators
    let () = Test.set_source liquidityProvider in
    let () = ExtendedFA2_helper.update_operators_success([Add_operator({owner=liquidityProvider; operator=cfmm.addr; token_id=0n})], tokenX.contr) in
    let () = FA12_helper.approve_success((cfmm.addr, postion_1_liquidity + postion_2_liquidity), tokenY.contr) in
    let () = Test.set_source swapper in
    let () = ExtendedFA2_helper.update_operators_success([Add_operator({owner=swapper; operator=cfmm.addr; token_id=0n})], tokenX.contr) in
    let () = FA12_helper.approve_success((cfmm.addr, swapperBalanceY), tokenY.contr) in

    // SET POSITION (10000) on [-100;100]
    let () = Test.set_source liquidityProvider in
    let param : Cfmm.set_position_param = Cfmm_helper.generate_set_position_param(postion_1_liquidity, (postion_1_lowerTickIndex, postion_1_upperTickIndex)) in
    let () = Cfmm_helper.set_position_success(param, 0tez, cfmm.contr) in
    let () = Cfmm_helper.assert_liquidity(cfmm.taddr, postion_1_liquidity) in
    // SET POSITION (30000) on [-200;-100]
    let param : Cfmm.set_position_param = Cfmm_helper.generate_set_position_param(postion_2_liquidity, (postion_2_lowerTickIndex, postion_2_upperTickIndex)) in
    let () = Cfmm_helper.set_position_success(param, 0tez, cfmm.contr) in
    let () = Cfmm_helper.assert_liquidity(cfmm.taddr, postion_1_liquidity) in

    // SWAP 53
    let () = Test.set_source swapper in
    let x_amount_swap = 53n in
    let param : Cfmm.x_to_y_param = Cfmm_helper.generate_x_to_y_param(x_amount_swap, 1n, swapReceiver) in
    let () = Cfmm_helper.x_to_y_success(param, 0tez, cfmm.contr) in

    // sanity check
    let s_final = Test.get_storage cfmm.taddr in
    let () = assert(s_final.cur_tick_index.i = -101) in
    let _ : unit = Cfmm_helper.check_all_invariants(cfmm.taddr, cfmm.addr) in
    ()

// test_protocol_fees_are_burned
let test_protocol_fees_are_burned = 
    let () = Test.log("test_protocol_fees_are_burned: 1 TODO remaining") in
    let feeBps = 0n in
    let protoFeeBps = 5000n in
    let effectiveProtoFeeBps = if config.y = CTEZ then protoFeeBps else 0n in
    let liquidity = 10_000n in 
    let lowerTickIndex = -100 in
    let upperTickIndex = 100 in

    let swapperBalanceX : nat = 100n in
    let swapperBalanceY : nat = swapperBalanceX in
    let accounts = Bootstrap.boot_accounts() in
    let (liquidityProvider, swapper, swapReceiver, _feeReceiver, _) = accounts in
    // Deploy contracts
    let tokenX = Bootstrap.boot_token(0n, 0tez, liquidityProvider, liquidity + swapperBalanceX) in
    let tokenY = Bootstrap.boot_token_fa12(0n, 0tez, liquidityProvider, liquidity + swapperBalanceY) in
    let cfmm = Bootstrap.boot_cfmm_by_config(config, tokenX.addr, tokenY.addr, 1n, feeBps, effectiveProtoFeeBps, 0tez) in

    // Transfer some tokens (X and Y) to swapper
    let () = Test.set_source liquidityProvider in
    let () = ExtendedFA2_helper.transfer_success([{from_=liquidityProvider; txs=[{to_=swapper; token_id=0n; amount=swapperBalanceX}]}], tokenX.contr) in
    let () = FA12_helper.transfer_success((liquidityProvider, (swapper, swapperBalanceY)), tokenY.contr) in
    let () = ExtendedFA2_helper.assert_user_balance(tokenX.taddr, swapper, swapperBalanceX) in
    let () = FA12_helper.assert_user_balance(tokenY.taddr, swapper, swapperBalanceY) in
    // update_operators
    let () = Test.set_source liquidityProvider in
    let () = ExtendedFA2_helper.update_operators_success([Add_operator({owner=liquidityProvider; operator=cfmm.addr; token_id=0n})], tokenX.contr) in
    let () = FA12_helper.approve_success((cfmm.addr, liquidity), tokenY.contr) in
    let () = Test.set_source swapper in
    let () = ExtendedFA2_helper.update_operators_success([Add_operator({owner=swapper; operator=cfmm.addr; token_id=0n})], tokenX.contr) in
    let () = FA12_helper.approve_success((cfmm.addr, swapperBalanceY), tokenY.contr) in

    // SET POSITION (10000) on [-100;100]
    let () = Test.set_source liquidityProvider in
    let param : Cfmm.set_position_param = Cfmm_helper.generate_set_position_param(liquidity, (lowerTickIndex, upperTickIndex)) in
    let () = Cfmm_helper.set_position_success(param, 0tez, cfmm.contr) in
    let () = Cfmm_helper.assert_liquidity(cfmm.taddr, liquidity) in

    // -- The cfmm contract has a non-zero initial CTEZ balance
    let cfmmInitialBalanceY = FA12_helper.get_user_balance(tokenY.taddr, cfmm.addr) in
    let () = assert(cfmmInitialBalanceY >= 1n) in

    // SWAP 10 (Perform a swap that does not exhaust the position's liquidity)
    let () = Test.set_source swapper in
    let x_amount_swap = 10n in
    let param : Cfmm.x_to_y_param = Cfmm_helper.generate_x_to_y_param(x_amount_swap, 1n, swapReceiver) in
    let () = Cfmm_helper.x_to_y_success(param, 0tez, cfmm.contr) in

    // -- The cfmm contract still has a CTEZ balance that we can use to make a swap
    // -- Note: the reason why at least 4 tokens are required here is simply because
    // -- otherwise the test might be invalidated by the rounding involved
    let cfmmBalanceY = FA12_helper.get_user_balance(tokenY.taddr, cfmm.addr) in
    let () = assert(cfmmBalanceY >= 4n) in

    // -- This swap would exhaust the position liquidity and take ~50% of the
    // -- remaining CTEZ balance, but it will fail because some of that balance are
    // -- protocol fees put aside from the previous swaps.
    // -- Note: this would have succeeded if the previous swap didn't happen.
    let () = Test.set_source swapper in
    let x_amount_swap = 100n in
    let param : Cfmm.x_to_y_param = Cfmm_helper.generate_x_to_y_param(x_amount_swap, abs((cfmmBalanceY - 1) / 2n), swapper) in
    let r = Cfmm_helper.x_to_y(param, 0tez, cfmm.contr) in
    let () = Assert.min_dy_failure r Cfmm.smaller_than_min_asset_err in
    
    // -- Trying to only exhaust the position liquidity however is still possible
    let () = Test.set_source swapper in
    let x_amount_swap = 100n in
    let param : Cfmm.x_to_y_param = Cfmm_helper.generate_x_to_y_param(x_amount_swap, 1n, swapper) in
    let () = Cfmm_helper.x_to_y_success(param, 0tez, cfmm.contr) in

    // -- The cfmm contract now has ~50% of the initial balance...
    let cfmmFinalBalanceY = FA12_helper.get_user_balance(tokenY.taddr, cfmm.addr) in
    let () = assert(cfmmFinalBalanceY >= cfmmInitialBalanceY / 2n) in

    // -- ... none of which can be swapped, as it's all protocol fees
    let () = Test.set_source swapper in
    let x_amount_swap = 100n in
    let param : Cfmm.x_to_y_param = Cfmm_helper.generate_x_to_y_param(x_amount_swap, 1n, swapper) in
    let r = Cfmm_helper.x_to_y(param, 0tez, cfmm.contr) in
    let () = Assert.min_dy_failure r Cfmm.smaller_than_min_asset_err in
    ()