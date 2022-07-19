open Dap_base

module Request = struct

  type t =
    | Cancel

  let enc_t =
    let open Data_encoding in
    conv
      (function | Cancel -> "cancel")
      (function | "cancel" -> Cancel | _ -> failwith "Unknown request")
      string

  type 'json cls_t = <
    ProtocolMessage.cls_t;
    command:t;
    arguments:'json option
  >

  class ['json] cls
      (seq:int64)
      (command:t)
      (arguments:'json option)
      = object
    inherit ProtocolMessage.cls seq Request

    method command = command
    method arguments = arguments

  end

  let enc js =
    let open Data_encoding in
    conv
      (fun (r : < 'json cls_t >) ->
         (r#seq, r#type_, r#command, r#arguments) )

      (fun (seq, _, command, arguments) ->
         new cls seq command arguments)

      (obj4
         (req "seq" int64)
         (req "type" ProtocolMessage.enc_t)
         (req "command" enc_t)
         (opt "arguments" js)
      )

end


module CancelRequest = struct

  type t = CancelArguments.t

  type cls_t = t Request.cls_t

  class cls (seq:int64) (arguments:t option) = object
    inherit [t] Request.cls seq Cancel arguments
  end

  let enc = Request.enc CancelArguments.enc

end


(* class ['json] requester_cls (req:'json Request.cls_t) = object
 *   method req = req
 * end *)
