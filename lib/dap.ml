(* open Data_encoding *)
open Json_schema
module Q = Json_query


module StrHashtbl = Hashtbl.Make(struct type t = string let equal = String.equal let hash = Hashtbl.hash end)

(* TODO thread these through as module state *)
let tbl : string list StrHashtbl.t = StrHashtbl.create 100
let _spaces = ref 0

let space n =
  List.init n (fun _ -> "") |> String.concat " "

(* extract all $ref *)
let rec process_name ~schema name =
  Printf.printf "%sprocess name start: '%s'\n" (space !_spaces) name;
  _spaces := !_spaces + 4;

  (* first check is valid name *)
  let _ = Q.path_of_json_pointer name in
  let element = find_definition name schema in
  if not @@ StrHashtbl.mem tbl name then StrHashtbl.add tbl name [];
  process_element ~schema ~name element;

  _spaces := !_spaces - 4;
  Printf.printf "%sprocess name end: '%s'\n" (space !_spaces) name;

and process_element ~schema ~name el =
  Printf.printf "%sprocess element under '%s'\n" (space !_spaces) name;
  process_kind ~schema ~name el.kind

and process_kind ~schema ~name = function
  | Object {properties; pattern_properties; additional_properties; min_properties; max_properties; schema_dependencies; property_dependencies} -> (
      assert (0 = List.length pattern_properties);
      assert (0 = List.length schema_dependencies);
      assert (0 = List.length property_dependencies);
      assert (0 = min_properties);
      assert (Option.is_none max_properties);
      assert (Option.is_some additional_properties);
      Printf.printf "%sprocess object with %d properties under '%s'\n" (space !_spaces) (List.length properties) name;
      properties |> List.iter (fun (pname, ty, required, extra) -> process_property ~schema ~name pname ty required extra)
    )
  | Array (_, _) -> () (* failwith "TODO array" *)
  | Monomorphic_array (element, {min_items; max_items; unique_items; additional_items}) -> (
      assert (0 = min_items);
      assert (Option.is_none max_items);
      assert (not unique_items);
      assert (Option.is_none additional_items);
      Printf.printf "%sprocess mono-morphic array under '%s'\n" (space !_spaces) name;
      process_element ~schema ~name element
    )
  | Combine (c, elements) -> (
      match c with
      | All_of -> (
        Printf.printf "%sprocess combination with %d elements under '%s'\n" (space !_spaces) (List.length elements) name;
        elements |> List.iter (fun el -> process_element ~schema ~name el)
      )
      | Any_of | One_of | Not -> () (* failwith "TODO other combinators" *)
    )
  | Def_ref path ->
    let path_str = Q.json_pointer_of_path path in
    (* add path_str to the entries under name - but only if not already there *)
    let ps = StrHashtbl.find_opt tbl name |> Option.value ~default:[] in
    if not @@ List.mem path_str ps then
      StrHashtbl.replace tbl name (path_str :: ps);
    (* if path_str is also a new ref then recurse into it *)
    if not @@ StrHashtbl.mem tbl path_str then (
      Printf.printf "%sfound new $ref '%s' under '%s', recursing\n" (space !_spaces) path_str name;
      process_name ~schema path_str
    )
    else (
      Printf.printf "%sfound old $ref '%s' under '%s', not recursing\n" (space !_spaces) path_str name;
    )
  | Id_ref _ -> () (* failwith "TODO Id_ref" *)
  | Ext_ref _ -> () (* failwith "TODO Ext_ref" *)
  | String _ -> () (* failwith "TODO String" *)
  | Integer _ -> () (* failwith "TODO Integer" *)
  | Number _ -> () (* failwith "TODO Number" *)
  | Boolean -> () (* failwith "TODO Boolean" *)
  | Null -> () (* failwith "TODO Null" *)
  | Any -> () (* failwith "TODO Any" *)
  | Dummy -> () (* failwith "TODO Dummy" *)

and process_property ~schema ~name pname element _required _extra =
  Printf.printf "%sprocess property '%s' under '%s'\n" (space !_spaces) pname name;
  process_element ~schema ~name element


let names = function
| `O fields -> fields |> List.map (fun (nm, _) -> nm)
| _ -> []


let process schema =
  StrHashtbl.reset tbl;
  let ns = Q.query [`Field "definitions"] (to_json schema) |> names in
  Printf.printf "\n\nprocessing '%d' names\n" @@ List.length ns;
  ns |> List.iter (fun nm -> let name = Printf.sprintf "/definitions/%s" nm in process_name ~schema name);
  tbl


let pp_tbl tbl =
  StrHashtbl.to_seq tbl
  |> List.of_seq
  |> List.map (fun (name, deps) -> Printf.sprintf "%s:\n  [ %s ]" name @@ String.concat "; " deps)
  |> List.sort String.compare
  |> List.iter (fun ln -> Printf.printf "%s\n\n" ln)
