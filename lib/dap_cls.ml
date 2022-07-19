(* open Data_encoding *)

type msg_t =
  | Request
  | Response
  | Event

type request_t =
  | Cancel

type response_t =
  | Cancelled

type event_t =
  | Initialized
  | Stopped
  | Continued
  | Exited
  | Terminated
  | Thread
  | Output
  | Breakpoint
  | Module
  | LoadedSource
  | Process
  | Capabilities
  | ProgressStart
  | ProgressUpdate
  | ProgressEnd
  | Invalidated
  | Memory
  | RunInTerminal

type stopping_reason =
  | Step
  | Breakpoint
  | Exception
  | Pause
  | Entry
  | Goto
  | Function_breakpoint
  | Data_breakpoint
  | Instruction_breakpoint


module ProtocolMessage = struct

  type cls_t = < seq:int64; type_:msg_t >

  class cls (seq:int64) (type_:msg_t) = object
    method seq = seq
    method type_ = type_
  end

end

module Request = struct

  type 'args cls_t = < ProtocolMessage.cls_t; command:request_t; arguments:'args option >

  class ['args] cls (seq:int64) (command:request_t) (arguments:'args option) = object
    inherit ProtocolMessage.cls seq Request

    method command = command
    method arguments = arguments

  end

end

module Event = struct

  type 'body cls_t = < ProtocolMessage.cls_t; event:event_t; body:'body >

  class ['body] cls (seq:int64) (event:event_t) (body:'body) = object
    inherit ProtocolMessage.cls seq Event

    method event = event
    method body = body

  end

end

module Response = struct

  type 'body cls_t = < ProtocolMessage.cls_t; request_seq:int64; success:bool; command:string; message:response_t option; body:'body >

  class ['body] cls (seq:int64) (request_seq:int64) (success:bool) (command:string) (message:response_t option) (body:'body) = object
    inherit ProtocolMessage.cls seq Response

    method request_seq = request_seq
    method success = success
    method command = command
    method message = message |> Option.value ~default:Cancelled
    method body = body

  end

end

module Message = struct

  type t = {
    id: int64;
    format: string;
    variables: (string * string) list option;
    sendTelemetry: bool option;
    showUser: bool option;
    url: string option;
    urlLabel: string option;
  }

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

module CancelArguments = struct

  type t = {
    requestId:int64 option;
    progressId:string option
  }

end

module CancelRequest = struct

  type cls_t = CancelArguments.t Request.cls_t

  class cls (seq:int64) (arguments:CancelArguments.t  option) = object
    inherit [CancelArguments.t] Request.cls seq Cancel arguments

  end

end

module CancelResponse = struct

  type cls_t = unit option Response.cls_t

  class cls (seq:int64) (request_seq:int64) (success:bool) (command:string) = object
    inherit [unit option] Response.cls seq request_seq success command None None
  end

end


class ['args] requester_cls (req:'args Request.cls_t) = object
  method req = req
end


module InitializedEvent = struct

  type cls_t = unit option Event.cls_t

  class cls (seq:int64) = object
    inherit [unit option] Event.cls seq Initialized None
  end

end


module StoppedEvent = struct


  type body = {
    reason: stopping_reason;
    description: string option;
    threadId: int64 option;
    preserveFocusHint: bool option;
    text: string option;
    allThreadsStopped: bool option;
    hitBreakpointIds: int64 list option;
  }

  type cls_t = body Event.cls_t

  class cls (seq:int64) (body:body) = object
    inherit [body] Event.cls seq Stopped body
  end

end


module ContinuedEvent = struct

  type body = {
    threadId: int64;
    allThreadsContinued: bool option;
  }

  type cls_t = body Event.cls_t

  class cls (seq:int64) (body:body) = object
    inherit [body] Event.cls seq Continued body
  end

end


module ExitedEvent = struct

  type body = {
    exitCode: int64;
  }

  type cls_t = body Event.cls_t

  class cls (seq:int64) (body:body) = object
    inherit [body] Event.cls seq Exited body
  end

end


module TerminatedEvent = struct

  type 'args body = {
    restart: 'args
  }

  type 'args cls_t = 'args body option Event.cls_t

  class ['args] cls (seq:int64) (body:'args body option) = object
    inherit ['args body option] Event.cls seq Terminated body
  end

end