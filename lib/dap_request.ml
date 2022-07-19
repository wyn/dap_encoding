open Dap_base

type request_t =
  | Cancel


module Request = struct

  type 'json cls_t = <
    ProtocolMessage.cls_t;
    command:request_t;
    arguments:'json option
  >

  class ['json] cls
      (seq:int64)
      (command:request_t)
      (arguments:'json option)
      = object
    inherit ProtocolMessage.cls seq Request

    method command = command
    method arguments = arguments

  end

end


module CancelRequest = struct

  type cls_t = CancelArguments.t Request.cls_t

  class cls (seq:int64) (arguments:CancelArguments.t  option) = object
    inherit [CancelArguments.t] Request.cls seq Cancel arguments

  end

end




class ['json] requester_cls (req:'json Request.cls_t) = object
  method req = req
end
