type token_type = FA12 | FA2 | CTEZ
type config = {
    x : token_type;
    y : token_type;
}

let get_main_path (config : config) : string = 
    let {x=x_type; y=y_type} = config in
    match (x_type, y_type) with
    | FA12, FA2 -> "../../lib/cfmm/main_fa12_fa2.mligo"
    | FA2, FA2 -> "../../lib/cfmm/main_fa2_fa2.mligo"
    | FA2, FA12 -> "../../lib/cfmm/main_fa2_fa12.mligo"
    | FA12, FA12 -> "../../lib/cfmm/main_fa12_fa12.mligo"
    | FA2, CTEZ -> "../../lib/cfmm/main_fa2_ctez.mligo"
    | FA12, CTEZ -> "../../lib/cfmm/main_fa12_ctez.mligo"
    | CTEZ, _ -> Test.failwith("CTEZ is not supported as a token type for x")