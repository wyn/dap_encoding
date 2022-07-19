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


module ThreadEvent = struct

  type thread_reason =
    | Started
    | Exited

  type body = {
    reason: thread_reason;
    threadId: int64;
  }

  type cls_t = body Event.cls_t

  class cls (seq:int64) (body:body) = object
    inherit [body] Event.cls seq Thread body
  end

end

module Checksum = struct

  type checksum_algorithm =
    | MD5
    | SHA1
    | SHA256
    | Timestamp

  type t = {
    algorithm: checksum_algorithm;
    checksum: string;
  }

end


module Source = struct

  type hint =
    | Normal
    | Emphasize
    | Deemphasize

  type 'data t = {
    name: string option;
    path: string option;
    sourceReference: int64 option;
    presentationHint: hint option;
    origin: string option;
    sources: 'data t list option;
    adapterData: 'data option;
    checksums: Checksum.t list option;
  }

end


module OutputEvent = struct

  type output_category =
    | Console
    | Important
    | Stdout
    | Stderr
    | Telemetry

  type group_t =
    | Start
    | StartCollapsed
    | End


  type 'data body = {
    output: string;
    category: output_category option;
    group: group_t option;
    variablesReference: int64 option;
    source: Source.t option;
    line: int64 option;
    column: int64 option;
    data: 'data option;
  }

  type 'data cls_t = 'data body Event.cls_t

  class ['data] cls (seq:int64) (body:'data body) = object
    inherit ['data body] Event.cls seq Output body
  end

end

module Breakpoint = struct type t = (* TODO *) int end

module BreakpointEvent = struct

  type reason =
    | Changed
    | New
    | Removed

  type body = {
    reason: reason;
    breakpoint: Breakpoint.t;
  }

  type cls_t = body Event.cls_t

  class cls (seq:int64) (body:body) = object
    inherit [body] Event.cls seq Breakpoint body
  end

end

module Module_ = struct type t = (* TODO *) int end
module ModuleEvent = struct

  type reason =
    | New
    | Changed
    | Removed

  type body = {
    reason: reason;
    module_: Module_.t
  }

  type cls_t = body Event.cls_t

  class cls (seq:int64) (body:body) = object
    inherit [body] Event.cls seq Module body
  end

end


module LoadedSourceEvent = struct

  type reason =
    | New
    | Changed
    | Removed

  type body = {
    reason: reason;
    source: Source.t;
  }

  type cls_t = body Event.cls_t

  class cls (seq:int64) (body:body) = object
    inherit [body] Event.cls seq LoadedSource body
  end

end


module ProcessEvent = struct

  type start_method =
    | Launch
    | Attach
    | AttachForSuspendedLaunch

  type body = {
    name: string;
    systemProcessId: int64 option;
    isLocalProcess: bool option;
    startMethod: start_method option;
    pointerSize: int64 option;
  }

  type cls_t = body Event.cls_t

  class cls (seq:int64) (body:body) = object
    inherit [body] Event.cls seq Process body
  end

end

module Capabilities = struct type t = (* TODO *) int end
module CapabilitiesEvent = struct

  type body = {
    capabilities: Capabilities.t
  }

  type cls_t = body Event.cls_t

  class cls (seq:int64) (body:body) = object
    inherit [body] Event.cls seq Capabilities body
  end

end


module ProgressStartEvent = struct

  type body = {
    progressId: string;
    title: string;
    requestId: int64 option;
    cancellable: bool option;
    message: string option;
    percentage: int64 option;
  }

  type cls_t = body Event.cls_t

  class cls (seq:int64) (body:body) = object
    inherit [body] Event.cls seq ProgressStart body
  end

end


module ProgressUpdateEvent = struct

  type body = {
    progressId: string;
    message: string option;
    percentage: int64 option;
  }

  type cls_t = body Event.cls_t

  class cls (seq:int64) (body:body) = object
    inherit [body] Event.cls seq ProgressUpdate body
  end

end


module ProgressEndEvent = struct

  type body = {
    progressId: string;
    message: string option;
  }

  type cls_t = body Event.cls_t

  class cls (seq:int64) (body:body) = object
    inherit [body] Event.cls seq ProgressEnd body
  end

end

module InvalidatedAreas = struct
  type t =
    | All
    | Stacks
    | Threads
    | Variables
end

module InvalidatedEvent = struct

  type body = {
    areas: InvalidatedAreas.t list option;
    threadId: int64 option;
    stackFrameId: int64 option;

  }

  type cls_t = body Event.cls_t

  class cls (seq:int64) (body:body) = object
    inherit [body] Event.cls seq Invalidated body
  end

end


module MemoryEvent = struct

  type body = {
    memoryReference: string;
    offset: int64;
    count: int64;
  }

  type cls_t = body Event.cls_t

  class cls (seq:int64) (body:body) = object
    inherit [body] Event.cls seq Memory body
  end

end
