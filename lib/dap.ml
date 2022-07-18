open Data_encoding

module ProtocolMessage = struct

  type msg =
    | Request
    | Response
    | Event

  let msg_e =
    conv
      ( function | Request -> "request" | Response -> "response" | Event -> "event" )
      ( fun msg -> match msg with | "request" -> Request | "response" -> Response | "event" -> Event | _ -> failwith "ERROR: unkown msg type" )
      string

  let e =
    obj2
      (req "seq" int64)
      (req "type" msg_e)
end

module Request = struct

  let msg_e =
    let open ProtocolMessage in
    conv
      ( function | Request -> "request" | _ -> failwith "ERROR: expected Request")
      ( fun msg -> match msg with | "request" -> Request | _ -> failwith "ERROR: expected 'request'" )
      string

  let request_e args =
    obj3
      (req "type" msg_e)
      (req "command" string)
      (opt "arguments" args)

  let e args =
    merge_objs ProtocolMessage.e (request_e args)
end

module Event = struct

  let msg_e =
    let open ProtocolMessage in
    conv
      ( function | Event -> "event" | _ -> failwith "ERROR: expected Event")
      ( fun msg -> match msg with | "event" -> Event | _ -> failwith "ERROR: expected 'event'" )
      string

  let event_e body =
    obj3
      (req "type" msg_e)
      (req "event" string)
      (opt "body" body)

  let e body =
    merge_objs ProtocolMessage.e (event_e body)
end

module Response = struct

  let msg_e =
    let open ProtocolMessage in
    conv
      ( function | Response -> "response" | _ -> failwith "ERROR: expected Response")
      ( fun msg -> match msg with | "response" -> Response | _ -> failwith "ERROR: expected 'response'" )
      string

  let response_e body =
    obj6
      (req "type" msg_e)
      (req "request_seq" int64)
      (req "success" bool)
      (req "command" string)
      (opt "message" string)
      (opt "body" body)

  let e body =
    merge_objs ProtocolMessage.e (response_e body)
end

module Message = struct
  let e =
    obj7
      (req "id" int64)
      (req "format" string)
      (opt "variables" (list @@ (tup2 string string)))
      (opt "sendTelemetry" bool)
      (opt "showUser" bool)
      (opt "url" string)
      (opt "urlLabel" string)
end


module ErrorResponse = struct

  let response_e =
    let err =
      obj1
        (opt "error" Message.e)
    in
    obj1
      (req "body" err)

  let e body =
    merge_objs (Response.e body) response_e
end

module CancelArguments = struct
  let e =
    obj2
      (opt "requestId" int64)
      (opt "progressId" string)
end

module CancelRequest = struct

  let cmd_e =
    obj2
      (req "command" (constant "cancel"))
      (opt "arguments" CancelArguments.e)

  let e =
    merge_objs (Request.e CancelArguments.e) cmd_e
end

module CancelResponse = struct
  let e =
    Response.e unit
end
