open Dap_base

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


module Event = struct

  type 'body cls_t = <
    ProtocolMessage.cls_t;
    event:event_t;
    body:'body
  >

  class ['body] cls
      (seq:int64)
      (event:event_t)
      (body:'body)
      = object
    inherit ProtocolMessage.cls seq Event

    method event = event
    method body = body

  end

end


module InitializedEvent = struct

  type cls_t = unit option Event.cls_t

  class cls (seq:int64) = object
    inherit [unit option] Event.cls seq Initialized None
  end

end


module StoppedEvent = struct

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

  type ('data, 'source_data) body = {
    output: string;
    category: output_category option;
    group: group_t option;
    variablesReference: int64 option;
    source: 'source_data Source.t option;
    line: int64 option;
    column: int64 option;
    data: 'data option;
  }

  type ('data, 'source_data) cls_t = ('data, 'source_data) body Event.cls_t

  class ['data, 'source_data] cls
      (seq:int64)
      (body:('data, 'source_data) body) = object
    inherit [('data, 'source_data) body] Event.cls seq Output body
  end

end


module BreakpointEvent = struct

  type reason =
    | Changed
    | New
    | Removed

  type 'data body = {
    reason: reason;
    breakpoint: 'data Breakpoint.t;
  }

  type 'data cls_t = 'data body Event.cls_t

  class ['data] cls (seq:int64) (body:'data body) = object
    inherit ['data body] Event.cls seq Breakpoint body
  end

end


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

  type 'data body = {
    reason: reason;
    source: 'data Source.t;
  }

  type 'data cls_t = 'data body Event.cls_t

  class ['data] cls (seq:int64) (body:'data body) = object
    inherit ['data body] Event.cls seq LoadedSource body
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
