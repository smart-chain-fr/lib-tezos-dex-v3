#import "../helpers/cfmm.mligo" "Cfmm_helper"
#import "../helpers/extended_fa2.mligo" "ExtendedFA2_helper"
#import "../helpers/sft.mligo" "SFT_helper"
#import "../helpers/fa12.mligo" "FA12_helper"
#import "../helpers/config.mligo" "Config_helper"
#import "../helpers/observe_consumer.mligo" "ObserveConsumer_helper"


let boot_accounts () =
    let () = Test.reset_state 6n ([10000000tez; 4000000tez; 4000000tez; 4000000tez; 4000000tez; 4000000tez ] : tez list) in
    let accounts =
        Test.nth_bootstrap_account 1,
        Test.nth_bootstrap_account 2,
        Test.nth_bootstrap_account 3,
        Test.nth_bootstrap_account 4,
        Test.nth_bootstrap_account 5
    in
    accounts

(* Boostrapping of the test environment for CFMM *)
let boot_cfmm (x_token, y_token, tick_spacing, fee_bps, proto_fee_bps, contract_balance : address * address * nat * nat * nat * tez) =
    let cfmm = Cfmm_helper.originate_from_file(
        Cfmm_helper.base_storage(x_token, y_token, tick_spacing, fee_bps, proto_fee_bps), contract_balance
    ) in
    cfmm

let boot_cfmm_by_config (config, x_token, y_token, tick_spacing, fee_bps, proto_fee_bps, contract_balance : Config_helper.config * address * address * nat * nat * nat * tez) =
    let cfmm = Cfmm_helper.originate_from_file_by_config(
        config, Cfmm_helper.base_storage(x_token, y_token, tick_spacing, fee_bps, proto_fee_bps), contract_balance
    ) in
    cfmm

(* Boostrapping of the test environment for observe_consumer *)
let boot_observe_consumer (contract_balance : tez) =
    let observer = ObserveConsumer_helper.originate(
        ObserveConsumer_helper.base_storage(), 
        contract_balance
    ) in
    observer


(* Boostrapping of the test environment for Token *)
let boot_token (token_id, contract_balance, beneficiary, beneficiary_amount : nat * tez * address * nat) =
    let token = ExtendedFA2_helper.originate_from_file(
        ExtendedFA2_helper.base_storage(token_id, beneficiary, beneficiary_amount), contract_balance
    ) in
    token

(* Boostrapping of the test environment for Token *)
let boot_token_sft (token_id, contract_balance, beneficiary, beneficiary_amount : nat * tez * address * nat) =
    let token = SFT_helper.originate_from_file(
        SFT_helper.base_storage(token_id, beneficiary, beneficiary_amount), contract_balance
    ) in
    token

(* Boostrapping of the test environment for Token *)
let boot_token_fa12 (token_id, contract_balance, beneficiary, beneficiary_amount : nat * tez * address * nat) =
    let beneficiary_allowances = (Map.empty : (address, nat) map) in
    let ledger = Big_map.literal ([
      (beneficiary, (beneficiary_amount, beneficiary_allowances));
    ])
    in
    let total_supply = beneficiary_amount in
    let token_info = (Map.empty: (string, bytes) map) in
    let token_metadata = 
        { token_id = token_id; token_info = token_info; }
    in
    let metadata = Big_map.literal([
        ("", Bytes.pack("tezos-storage:contents"));
        ("contents", ("54657374546F6B656E": bytes))
      ]) 
    in

    let token = FA12_helper.originate(
        FA12_helper.base_storage(ledger, token_metadata, total_supply, metadata), contract_balance
    ) in
    token