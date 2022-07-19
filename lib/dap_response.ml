open Dap_base

type response_t =
  | Cancelled


module Response = struct

  type 'json cls_t = <
    ProtocolMessage.cls_t;
    request_seq:int64;
    success:bool;
    command:string;
    message:response_t option;
    body:'json
  >

  class ['json] cls
      (seq:int64)
      (request_seq:int64)
      (success:bool)
      (command:string)
      (message:response_t option)
      (body:'json)
      = object
    inherit ProtocolMessage.cls seq Response

    method request_seq = request_seq
    method success = success
    method command = command
    method message = message |> Option.value ~default:Cancelled
    method body = body

  end

end

module ErrorResponse = struct

  type body = {
    error: Message.t option
  }

  type cls_t = body Response.cls_t

  class cls (seq:int64) (request_seq:int64) (success:bool) (command:string) (body:body) = object
    inherit [body] Response.cls seq request_seq success command None body
  end


end


module CancelResponse = struct

  type cls_t = unit option Response.cls_t

  class cls (seq:int64) (request_seq:int64) (success:bool) (command:string) = object
    inherit [unit option] Response.cls seq request_seq success command None None
  end

end
