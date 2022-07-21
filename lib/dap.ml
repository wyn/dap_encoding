(* open Data_encoding *)
open Json_schema
module Q = Json_query


module StrHashtbl = Hashtbl.Make(struct type t = string let equal = String.equal let hash = Hashtbl.hash end)
(* string list StrHashtbl.t = StrHashtbl.create 100 in *)
(* extract all $ref *)
let _process ~name full_schema element =

  Printf.printf "\nprocess start: '%s'\n" name;

  let pths = ref [] in

  let rec process_element el =
    process_kind el.kind

  and process_kind = function
    | Object {properties; pattern_properties; additional_properties; min_properties; max_properties; schema_dependencies; property_dependencies} -> (
        assert (0 = List.length pattern_properties);
        assert (0 = List.length schema_dependencies);
        assert (0 = List.length property_dependencies);
        assert (0 = min_properties);
        assert (Option.is_none max_properties);
        assert (Option.is_some additional_properties);
        properties |> List.iter (fun (name, ty, required, extra) -> process_property name ty required extra)
      )
    | Array (_, _) -> () (* failwith "TODO array" *)
    | Monomorphic_array (element, {min_items; max_items; unique_items; additional_items}) -> (
        assert (0 = min_items);
        assert (Option.is_none max_items);
        assert (not unique_items);
        assert (Option.is_none additional_items);
        process_element element
      )
    | Combine (c, elements) -> (
        match c with
        | All_of -> elements |> List.iter (fun el -> process_element el)
        | Any_of | One_of | Not -> () (* failwith "TODO other combinators" *)
      )
    | Def_ref path ->
      let path_str = Q.json_pointer_of_path path in
      if List.mem path_str !pths then
        Printf.printf "found old $ref '%s', ignoring\n" path_str
      else
        let element = find_definition path_str full_schema in
        pths := path_str :: !pths;
        Printf.printf "found new $ref '%s'\n" path_str;
        process_element element

    | Id_ref _ -> () (* failwith "TODO Id_ref" *)
    | Ext_ref _ -> () (* failwith "TODO Ext_ref" *)
    | String _ -> () (* failwith "TODO String" *)
    | Integer _ -> () (* failwith "TODO Integer" *)
    | Number _ -> () (* failwith "TODO Number" *)
    | Boolean -> () (* failwith "TODO Boolean" *)
    | Null -> () (* failwith "TODO Null" *)
    | Any -> () (* failwith "TODO Any" *)
    | Dummy -> () (* failwith "TODO Dummy" *)

  and process_property _name element _required _extra =
    Printf.printf "process property '%s'\n" _name;
    process_element element

  in
  process_element element;
  (name, !pths)

let process ~name full_schema =
  (* first check is valid name *)
  let _ = Q.path_of_json_pointer name in
  let el = find_definition name full_schema in
  _process ~name full_schema el



(* module ProtocolMessage = struct
 *
 *   type msg =
 *     | Request
 *     | Response
 *     | Event
 *
 *   type t = {
 *     seq: int64;
 *     type_: msg;
 *   }
 *
 *   let msg_e =
 *     conv
 *       ( function | Request -> "request" | Response -> "response" | Event -> "event" )
 *       ( fun msg -> match msg with | "request" -> Request | "response" -> Response | "event" -> Event | _ -> failwith "ERROR: unkown msg type" )
 *       string
 *
 *   let e =
 *     let description = "Sequence number (also known as message ID). For protocol messages of type 'request' this ID can be used to cancel the request." in
 *     conv
 *       (fun {seq; type_} -> (seq, type_))
 *       (fun (seq, type_) -> {seq; type_})
 *       (obj2
 *          (req ~description "seq" int64)
 *          (req ~description:"Message type." "type" msg_e))
 * end
 *
 * module Request = struct
 *
 *   type 'args t = {
 *     seq: int64;
 *     type_: ProtocolMessage.msg;
 *     command: string;
 *     arguments: 'args option;
 *   }
 *
 *   let msg_e =
 *     let open ProtocolMessage in
 *     conv
 *       ( function | Request -> "request" | _ -> failwith "ERROR: expected Request")
 *       ( fun msg -> match msg with | "request" -> Request | _ -> failwith "ERROR: expected 'request'" )
 *       string
 *
 *   let e args =
 *     conv
 *       (fun {seq; type_; command; arguments} -> (seq, type_, command, arguments))
 *       (fun (seq, type_, command, arguments) -> {seq; type_; command; arguments})
 *       (obj4
 *          (req "seq" int64)
 *          (req "type" msg_e)
 *          (req "command" string)
 *          (opt "arguments" args))
 *
 * end
 *
 * module Event = struct
 *
 *   type 'body t = {
 *     seq: int64;
 *     type_: ProtocolMessage.msg;
 *     event: string;
 *     body: 'body option;
 *   }
 *
 *   let msg_e =
 *     let open ProtocolMessage in
 *     conv
 *       ( function | Event -> "event" | _ -> failwith "ERROR: expected Event")
 *       ( fun msg -> match msg with | "event" -> Event | _ -> failwith "ERROR: expected 'event'" )
 *       string
 *
 *   let e body =
 *     conv
 *       (fun {seq; type_; event; body} -> (seq, type_, event, body))
 *       (fun (seq, type_, event, body) -> {seq; type_; event; body})
 *       (obj4
 *          (req "seq" int64)
 *          (req "type" msg_e)
 *          (req "event" string)
 *          (opt "body" body))
 *
 * end
 *
 * module Response = struct
 *
 *   type 'body t = {
 *     seq: int64;
 *     type_: ProtocolMessage.msg;
 *     request_seq: int64;
 *     success: bool;
 *     command: string;
 *     message: string option;
 *     body: 'body option;
 *   }
 *
 *   let msg_e =
 *     let open ProtocolMessage in
 *     conv
 *       ( function | Response -> "response" | _ -> failwith "ERROR: expected Response")
 *       ( fun msg -> match msg with | "response" -> Response | _ -> failwith "ERROR: expected 'response'" )
 *       string
 *
 *   let response_e body =
 *     obj7
 *       (req "seq" int64)
 *       (req "type" msg_e)
 *       (req "request_seq" int64)
 *       (req "success" bool)
 *       (req "command" string)
 *       (opt "message" string)
 *       (opt "body" body)
 *
 *   let e body =
 *     conv
 *       (fun {seq; type_; request_seq; success; command; message; body} -> (seq, type_, request_seq, success, command, message, body))
 *       (fun (seq, type_, request_seq, success, command, message, body) -> {seq; type_; request_seq; success; command; message; body})
 *       (response_e body)
 * end
 *
 * module Message = struct
 *   type t = {
 *     id: int64;
 *     format: string;
 *     variables: (string * string) list option;
 *     sendTelemetry: bool option;
 *     showUser: bool option;
 *     url: string option;
 *     urlLabel: string option;
 *   }
 *
 *   let msg_e =
 *     obj7
 *       (req "id" int64)
 *       (req "format" string)
 *       (opt "variables" (list @@ (tup2 string string)))
 *       (opt "sendTelemetry" bool)
 *       (opt "showUser" bool)
 *       (opt "url" string)
 *       (opt "urlLabel" string)
 *
 *   let e =
 *     conv
 *       (fun {id; format; variables; sendTelemetry; showUser; url; urlLabel} -> (id, format, variables, sendTelemetry, showUser, url, urlLabel))
 *       (fun (id, format, variables, sendTelemetry, showUser, url, urlLabel) -> {id; format; variables; sendTelemetry; showUser; url; urlLabel})
 *       msg_e
 * end
 *
 * module Error = struct
 *     type t = {
 *       error: Message.t option;
 *     }
 *
 *     let e =
 *       conv
 *         (fun {error} -> error)
 *         (fun error -> {error})
 *         (obj1 (opt "error" Message.e))
 *
 *   end
 *
 *
 * module ErrorResponse = struct
 *
 *   type t = {
 *     seq: int64;
 *     type_: ProtocolMessage.msg;
 *     request_seq: int64;
 *     success: bool;
 *     command: string;
 *     message: string option;
 *     body: Error.t;
 *   }
 *
 *   let msg_e =
 *     let open ProtocolMessage in
 *     conv
 *       ( function | Response -> "response" | _ -> failwith "ERROR: expected Response")
 *       ( fun msg -> match msg with | "response" -> Response | _ -> failwith "ERROR: expected 'response'" )
 *       string
 *
 *   let response_e =
 *     obj7
 *       (req "seq" int64)
 *       (req "type" msg_e)
 *       (req "request_seq" int64)
 *       (req "success" bool)
 *       (req "command" string)
 *       (opt "message" string)
 *       (req "body" Error.e)
 *
 *   let e =
 *     conv
 *       (fun {seq; type_; request_seq; success; command; message; body} -> (seq, type_, request_seq, success, command, message, body))
 *       (fun (seq, type_, request_seq, success, command, message, body) -> {seq; type_; request_seq; success; command; message; body})
 *       response_e
 * end
 *
 * module CancelArguments = struct
 *   type t = {
 *     requestId: int64 option;
 *     progressId: string option;
 *   }
 *
 *   let e =
 *     conv
 *       (fun {requestId; progressId} -> (requestId, progressId))
 *       (fun (requestId, progressId) -> {requestId; progressId})
 *       (obj2
 *          (opt "requestId" int64)
 *          (opt "progressId" string))
 *
 *   let e_ =
 *     obj2
 *       (opt "requestId" int64)
 *       (opt "progressId" string)
 * end
 *
 * module CancelRequest = struct
 *
 *   let cmd_e =
 *     obj2
 *       (req "command" (constant "cancel"))
 *       (opt "arguments" CancelArguments.e)
 *
 *   let e =
 *     merge_objs (Request.e CancelArguments.e) cmd_e
 * end
 *
 * module CancelResponse = struct
 *   let e =
 *     Response.e unit
 * end *)
