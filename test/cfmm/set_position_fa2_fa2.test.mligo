#import "../helpers/assert.mligo" "Assert"
#import "../bootstrap/bootstrap.mligo" "Bootstrap"
#import "../helpers/log.mligo" "Log"
#import "../helpers/cfmm.mligo" "Cfmm_helper"
#import "../../lib/cfmm/main.mligo" "Cfmm"
#import "../helpers/extended_fa2.mligo" "ExtendedFA2_helper"
#import "../helpers/config.mligo" "Config_helper"

let () = Log.describe("[FA2-FA2] [Cfmm.set_position] test suite")

let config = ({x=FA2; y=FA2} : Config_helper.config)

(* Successful Set_position *)
let test_success =
    let accounts = Bootstrap.boot_accounts() in
    let (user1, _user2, _, _, _) = accounts in
    let tokenX = Bootstrap.boot_token(0n, 0tez, user1, 1000000n) in
    let tokenY = Bootstrap.boot_token(0n, 0tez, user1, 1000000n) in

    let cfmm = Bootstrap.boot_cfmm_by_config(config, tokenX.addr, tokenY.addr, 1n, 10n, 10n, 0tez) in
    let () = Test.set_source user1 in
    let () = ExtendedFA2_helper.update_operators_success([Add_operator({owner=user1; operator=cfmm.addr; token_id=0n})], tokenX.contr) in
    let () = ExtendedFA2_helper.update_operators_success([Add_operator({owner=user1; operator=cfmm.addr; token_id=0n})], tokenY.contr) in

    let param : Cfmm.set_position_param = Cfmm_helper.generate_set_position_param(1000000n, (-15, 10)) in
    let () = Cfmm_helper.set_position_success(param, 0tez, cfmm.contr) in
    let () = Cfmm_helper.assert_liquidity(cfmm.taddr, 1000000n) in
    let () = ExtendedFA2_helper.assert_user_balance(tokenX.taddr, user1, 999500n) in
    let () = ExtendedFA2_helper.assert_user_balance(tokenX.taddr, cfmm.addr, 500n) in
    let () = ExtendedFA2_helper.assert_user_balance(tokenY.taddr, user1, 999250n) in
    ExtendedFA2_helper.assert_user_balance(tokenY.taddr, cfmm.addr, 750n)

(* Failing Set_position because amount not null *)
let test_failure_set_position_with_amount =
    let accounts = Bootstrap.boot_accounts() in
    let (user1, _user2, _, _, _) = accounts in
    let tokenX = Bootstrap.boot_token(0n, 0tez, user1, 1000000n) in
    let tokenY = Bootstrap.boot_token(0n, 0tez, user1, 1000000n) in

    let cfmm = Bootstrap.boot_cfmm_by_config(config, tokenX.addr, tokenY.addr, 1n, 10n, 10n, 0tez) in
    let () = Test.set_source user1 in
    // let () = ExtendedFA2_helper.update_operators_success([Add_operator({owner=user1; operator=cfmm.addr; token_id=0n})], tokenX.contr) in
    // let () = ExtendedFA2_helper.update_operators_success([Add_operator({owner=user1; operator=cfmm.addr; token_id=0n})], tokenY.contr) in

    let param : Cfmm.set_position_param = Cfmm_helper.generate_set_position_param(1000000n, (10, 10)) in
    let r = Cfmm_helper.set_position(param, 10tez, cfmm.contr) in
    Assert.nat_failure r Cfmm.non_zero_transfer_err

(* Failure Set_position *)
// test_equal_ticks
let test_failure_set_position_with_lower_tick_eq_upper_tick =
    let accounts = Bootstrap.boot_accounts() in
    let (user1, _user2, _, _, _) = accounts in
    let tokenX = Bootstrap.boot_token(0n, 0tez, user1, 1000000n) in
    let tokenY = Bootstrap.boot_token(0n, 0tez, user1, 1000000n) in
    let cfmm = Bootstrap.boot_cfmm_by_config(config, tokenX.addr, tokenY.addr, 1n, 10n, 10n, 0tez) in

    let () = Test.set_source user1 in
    let param : Cfmm.set_position_param = Cfmm_helper.generate_set_position_param(1000000n, (10, 10)) in
    let r = Cfmm_helper.set_position(param, 0tez, cfmm.contr) in
    Assert.nat_failure r Cfmm.tick_order_err

// test_wrong_tick_order
let test_failure_set_position_with_lower_tick_gt_upper_tick =
    let accounts = Bootstrap.boot_accounts() in
    let (user1, _user2, _, _, _) = accounts in
    let tokenX = Bootstrap.boot_token(0n, 0tez, user1, 1000000n) in
    let tokenY = Bootstrap.boot_token(0n, 0tez, user1, 1000000n) in
    let cfmm = Bootstrap.boot_cfmm_by_config(config, tokenX.addr, tokenY.addr, 1n, 10n, 10n, 0tez) in
    let () = Test.set_source user1 in
    
    let param : Cfmm.set_position_param = Cfmm_helper.generate_set_position_param(1000000n, (10, 9)) in
    let r = Cfmm_helper.set_position(param, 0tez, cfmm.contr) in
    Assert.nat_failure r Cfmm.tick_order_err

// test_setting_a_position_with_zero_liquidity_is_a_noop
let test_success_set_position_with_no_liquidity_is_noop =
    let accounts = Bootstrap.boot_accounts() in
    let (user1, _user2, _, _, _) = accounts in
    let tokenX = Bootstrap.boot_token(0n, 0tez, user1, 1000000n) in
    let tokenY = Bootstrap.boot_token(0n, 0tez, user1, 1000000n) in
    let cfmm = Bootstrap.boot_cfmm_by_config(config, tokenX.addr, tokenY.addr, 1n, 10n, 10n, 0tez) in
    let () = Test.set_source user1 in
    let () = ExtendedFA2_helper.update_operators_success([Add_operator({owner=user1; operator=cfmm.addr; token_id=0n})], tokenX.contr) in
    let () = ExtendedFA2_helper.update_operators_success([Add_operator({owner=user1; operator=cfmm.addr; token_id=0n})], tokenY.contr) in

    let s_old = Test.get_storage cfmm.taddr in
    // SET POSITION 0
    let param : Cfmm.set_position_param = Cfmm_helper.generate_set_position_param(0n, (-10, 10)) in
    let () = Cfmm_helper.set_position_success(param, 0tez, cfmm.contr) in

    let s_new = Test.get_storage cfmm.taddr in
    // SHOULD NOT CHANGE USER BALANCES
    let () = ExtendedFA2_helper.assert_user_balance(tokenX.taddr, user1, 1000000n) in
    let () = ExtendedFA2_helper.assert_user_balance(tokenX.taddr, cfmm.addr, 0n) in
    let () = ExtendedFA2_helper.assert_user_balance(tokenY.taddr, user1, 1000000n) in
    let () = ExtendedFA2_helper.assert_user_balance(tokenY.taddr, cfmm.addr, 0n) in
    // SHOULD NOT CHANGE THE STORAGE
    let () = Cfmm_helper.assert_storage_equal(s_old, s_new) in
    // let () = assert(s_old.cumulatives_buffer = s_new.cumulatives_buffer) in
    ()


// test_witnesses_must_be_valid
let test_failure_set_position_with_invalid_witness =
    let accounts = Bootstrap.boot_accounts() in
    let (user1, _user2, _, _, _) = accounts in
    let tokenX = Bootstrap.boot_token(0n, 0tez, user1, 1000000n) in
    let tokenY = Bootstrap.boot_token(0n, 0tez, user1, 1000000n) in
    let cfmm = Bootstrap.boot_cfmm_by_config(config, tokenX.addr, tokenY.addr, 1n, 10n, 10n, 0tez) in
    let () = Test.set_source user1 in

    let param : Cfmm.set_position_param = Cfmm_helper.generate_set_position_param(1000000n, (-15, 10)) in
    let param = { param with lower_tick_witness={ i=Cfmm_helper.minTickIndex + 1} } in
    let r = Cfmm_helper.set_position(param, 0tez, cfmm.contr) in
    let () = Assert.nat_failure r Cfmm.tick_not_exist_err in

    let param : Cfmm.set_position_param = Cfmm_helper.generate_set_position_param(1000000n, (-15, 10)) in
    let param = { param with upper_tick_witness={ i=Cfmm_helper.minTickIndex + 1} } in
    let r = Cfmm_helper.set_position(param, 0tez, cfmm.contr) in
    let () = Assert.nat_failure r Cfmm.tick_not_exist_err in

    let param : Cfmm.set_position_param = Cfmm_helper.generate_set_position_param(1000000n, (-15, 10)) in
    let param = { param with lower_tick_witness={ i=Cfmm_helper.maxTickIndex} } in
    let r = Cfmm_helper.set_position(param, 0tez, cfmm.contr) in
    let () = Assert.nat_failure r Cfmm.invalid_witness_err in

    let param : Cfmm.set_position_param = Cfmm_helper.generate_set_position_param(1000000n, (-15, 10)) in
    let param = { param with upper_tick_witness={ i=Cfmm_helper.maxTickIndex} } in
    let r = Cfmm_helper.set_position(param, 0tez, cfmm.contr) in
    Assert.nat_failure r Cfmm.invalid_witness_err

// test_fails_if_its_past_the_deadline
let test_failure_set_position_with_invalid_deadline =
    let accounts = Bootstrap.boot_accounts() in
    let (user1, _user2, _, _, _) = accounts in
    let tokenX = Bootstrap.boot_token(0n, 0tez, user1, 1000000n) in
    let tokenY = Bootstrap.boot_token(0n, 0tez, user1, 1000000n) in
    let cfmm = Bootstrap.boot_cfmm_by_config(config, tokenX.addr, tokenY.addr, 1n, 10n, 10n, 0tez) in
    
    let () = Test.set_source user1 in
    let param : Cfmm.set_position_param = Cfmm_helper.generate_set_position_param(1000000n, (-10, 15)) in
    let param = { param with deadline=Tezos.get_now() - 1 } in
    let r = Cfmm_helper.set_position(param, 0tez, cfmm.contr) in
    Assert.deadline_failure r Cfmm.past_deadline_err

let test_success_set_position_with_deadline_now =
    let accounts = Bootstrap.boot_accounts() in
    let (user1, _user2, _, _, _) = accounts in
    let tokenX = Bootstrap.boot_token(0n, 0tez, user1, 1000000n) in
    let tokenY = Bootstrap.boot_token(0n, 0tez, user1, 1000000n) in
    let cfmm = Bootstrap.boot_cfmm_by_config(config, tokenX.addr, tokenY.addr, 1n, 10n, 10n, 0tez) in
    let () = Test.set_source user1 in
    let () = ExtendedFA2_helper.update_operators_success([Add_operator({owner=user1; operator=cfmm.addr; token_id=0n})], tokenX.contr) in
    let () = ExtendedFA2_helper.update_operators_success([Add_operator({owner=user1; operator=cfmm.addr; token_id=0n})], tokenY.contr) in

    let param : Cfmm.set_position_param = Cfmm_helper.generate_set_position_param(1000000n, (-10, 15)) in
    let param = { param with deadline=Tezos.get_now() + 30 } in
    let () = Cfmm_helper.set_position_success(param, 0tez, cfmm.contr) in
    Cfmm_helper.assert_liquidity(cfmm.taddr, 1000000n)

// test_fails_if_its_not_multiple_tick_spacing
let test_failure_set_position_with_invalid_spacing_ticks =
    let accounts = Bootstrap.boot_accounts() in
    let (user1, _user2, _, _, _) = accounts in
    let tokenX = Bootstrap.boot_token(0n, 0tez, user1, 1000000n) in
    let tokenY = Bootstrap.boot_token(0n, 0tez, user1, 1000000n) in
    let tick_spacing = 10n in
    let cfmm = Bootstrap.boot_cfmm_by_config(config, tokenX.addr, tokenY.addr, tick_spacing, 10n, 10n, 0tez) in
    let () = Test.set_source user1 in

    let param : Cfmm.set_position_param = Cfmm_helper.generate_set_position_param(1000000n, (-10, 20)) in
    // ticks not mulitple of tick_spacing
    let param = { param with 
        lower_tick_index={ i=-9 }; 
        upper_tick_index={i=20} } 
    in
    let r = Cfmm_helper.set_position(param, 0tez, cfmm.contr) in
    let () = Assert.nat_failure r Cfmm.incorrect_tick_spacing_err in

    let param : Cfmm.set_position_param = Cfmm_helper.generate_set_position_param(1000000n, (-10, 20)) in
    // ticks not mulitple of tick_spacing
    let param = { param with 
        lower_tick_index={ i=-10 }; 
        upper_tick_index={i=11} } 
    in
    let r = Cfmm_helper.set_position(param, 0tez, cfmm.contr) in
    Assert.nat_failure r Cfmm.incorrect_tick_spacing_err

// test_cannot_set_position_over_max_tick
let test_failure_set_position_over_max_tick =
    let accounts = Bootstrap.boot_accounts() in
    let (user1, _user2, _, _, _) = accounts in
    let tokenX = Bootstrap.boot_token(0n, 0tez, user1, 1000000n) in
    let tokenY = Bootstrap.boot_token(0n, 0tez, user1, 1000000n) in
    let tick_spacing = 1n in
    let cfmm = Bootstrap.boot_cfmm_by_config(config, tokenX.addr, tokenY.addr, tick_spacing, 10n, 10n, 0tez) in
    let () = Test.set_source user1 in

    let param : Cfmm.set_position_param = Cfmm_helper.generate_set_position_param(1000000n, (-10, 20)) in
    let param = { param with upper_tick_index={ i=Cfmm_helper.maxTickIndex + 1} } in
    let r = Cfmm_helper.set_position(param, 0tez, cfmm.contr) in
    Assert.nat_failure r Cfmm.tick_not_exist_err

// test_maximum_tokens_contributed
let test_failure_set_position_lower_max_token_contributed =
    let accounts = Bootstrap.boot_accounts() in
    let (user1, _user2, _, _, _) = accounts in
    let tokenX = Bootstrap.boot_token(0n, 0tez, user1, 1000000n) in
    let tokenY = Bootstrap.boot_token(0n, 0tez, user1, 1000000n) in
    let cfmm = Bootstrap.boot_cfmm_by_config(config, tokenX.addr, tokenY.addr, 1n, 10n, 10n, 0tez) in
    let () = Test.set_source user1 in

    let param : Cfmm.set_position_param = Cfmm_helper.generate_set_position_param(1000000n, (-10, 10)) in
    let param = { param with maximum_tokens_contributed = {x = 1n; y = 1n} } in
    let r = Cfmm_helper.set_position(param, 0tez, cfmm.contr) in
    Assert.maximum_tokens_contributed_failure r Cfmm.high_tokens_err

(* Successful Set_position  + Update_position *)
let test_success_deposit_withdrawal_position =
    let accounts = Bootstrap.boot_accounts() in
    let (user1, _user2, _, _, _) = accounts in
    let tokenX = Bootstrap.boot_token(0n, 0tez, user1, 1000000n) in
    let tokenY = Bootstrap.boot_token(0n, 0tez, user1, 1000000n) in

    let cfmm = Bootstrap.boot_cfmm_by_config(config, tokenX.addr, tokenY.addr, 1n, 10n, 10n, 0tez) in
    let () = Test.set_source user1 in
    let () = ExtendedFA2_helper.update_operators_success([Add_operator({owner=user1; operator=cfmm.addr; token_id=0n})], tokenX.contr) in
    let () = ExtendedFA2_helper.update_operators_success([Add_operator({owner=user1; operator=cfmm.addr; token_id=0n})], tokenY.contr) in

    let param : Cfmm.set_position_param = Cfmm_helper.generate_set_position_param(1000000n, (-10, 10)) in
    let () = Cfmm_helper.set_position_success(param, 0tez, cfmm.contr) in
    let param_update : Cfmm.update_position_param = Cfmm_helper.generate_update_position_param(0n, -1000000, user1, 500n, 750n) in
    let () = Cfmm_helper.update_position_success(param_update, 0tez, cfmm.contr) in
    
    let () = Cfmm_helper.assert_liquidity(cfmm.taddr, 0n) in
    // Cfmm_helper.assert_storage_equal(s_after, s_test)
    let () = ExtendedFA2_helper.assert_user_balance_in_range(tokenX.taddr, user1, 1000000n, 1n) in
    let () = ExtendedFA2_helper.assert_user_balance_in_range(tokenX.taddr, cfmm.addr, 0n, 1n) in
    let () = ExtendedFA2_helper.assert_user_balance_in_range(tokenY.taddr, user1, 1000000n, 1n) in
    ExtendedFA2_helper.assert_user_balance_in_range(tokenY.taddr, cfmm.addr, 0n, 1n)

