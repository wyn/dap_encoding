open Dap_base


module Response = struct

  type t =
    | Cancelled

  let enc_t =
    let open Data_encoding in
    conv
      (function | Cancelled -> "cancelled")
      (function | "cancelled" -> Cancelled | _ -> failwith "Unknown request")
      string

  type 'json cls_t = <
    ProtocolMessage.cls_t;
    request_seq:int64;
    success:bool;
    command:string;
    message:t option;
    body:'json option
  >

  class ['json] cls
      (seq:int64)
      (request_seq:int64)
      (success:bool)
      (command:string)
      (message:t option)
      (body:'json option)
      = object(_self)
    inherit ProtocolMessage.cls seq Response as _super

    method request_seq = request_seq
    method success = success
    method command = command
    method message = Some (message |> Option.value ~default:Cancelled)
    method body = body

  end

  let enc js =
    let open Data_encoding in
    conv
      (fun (r:'json cls_t) ->
         (r#seq, r#type_, r#request_seq, r#success, r#command, r#message, r#body) )

      (fun (seq, _, request_seq, success, command, message, body) ->
         new cls seq request_seq success command message body)

      (obj7
         (req "seq" int64)
         (req "type" ProtocolMessage.enc_t)
         (req "request_seq" int64)
         (req "success" bool)
         (req "command" string)
         (opt "message" enc_t)
         (opt "body" js)
      )

end

module ErrorResponse = struct

  type body = {
    error: Message.t option
  }

  type cls_t = body Response.cls_t

  class cls (seq:int64) (request_seq:int64) (success:bool) (command:string) (body:body option) = object
    inherit [body] Response.cls seq request_seq success command None body
  end


end


module CancelResponse = struct

  type cls_t = unit Response.cls_t

  class cls (seq:int64) (request_seq:int64) (success:bool) (command:string) = object
    inherit [unit] Response.cls seq request_seq success command None None
  end

end
