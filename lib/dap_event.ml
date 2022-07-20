open Dap_base


module Event = struct

  type t =
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

  let enc_t =
    let open Data_encoding in
    conv
      (function
        | Initialized -> "initialized"
        | Stopped -> "stopped"
        | Continued -> "continued"
        | Exited -> "exited"
        | Terminated -> "terminated"
        | Thread -> "thread"
        | Output -> "output"
        | Breakpoint -> "breakpoint"
        | Module -> "module"
        | LoadedSource -> "loadedSource"
        | Process -> "process"
        | Capabilities -> "capabilities"
        | ProgressStart -> "progressStart"
        | ProgressUpdate -> "progressUpdate"
        | ProgressEnd -> "progressEnd"
        | Invalidated -> "invalidated"
        | Memory -> "memory"
        | RunInTerminal -> "runInTerminal"
      )
      (function
        | "initialized" -> Initialized
        | "stopped" -> Stopped
        | "continued" -> Continued
        | "exited" -> Exited
        | "terminated" -> Terminated
        | "thread" -> Thread
        | "output" -> Output
        | "breakpoint" -> Breakpoint
        | "module" -> Module
        | "loadedSource" -> LoadedSource
        | "process" -> Process
        | "capabilities" -> Capabilities
        | "progressStart" -> ProgressStart
        | "progressUpdate" -> ProgressUpdate
        | "progressEnd" -> ProgressEnd
        | "invalidated" -> Invalidated
        | "memory" -> Memory
        | "runInTerminal" -> RunInTerminal
        | _ -> failwith "Unknown event"
      )
      string


  type 'json cls_t = <
    ProtocolMessage.cls_t;
    event:t;
    body:'json
  >

  class ['json] cls
      (seq:int64)
      (event:t)
      (body:'json)
      = object
    inherit ProtocolMessage.cls seq Event

    method event = event
    method body = body

  end

  let enc js =
    let open Data_encoding in
    conv
      (fun (r : < 'json cls_t >) ->
         (r#seq, r#type_, r#event, r#body) )

      (fun (seq, _, event, body) ->
         new cls seq event body)

      (obj4
         (req "seq" int64)
         (req "type" ProtocolMessage.enc_t)
         (req "event" enc_t)
         (req "body" js)
      )

end


module InitializedEvent = struct

  type cls_t = unit option Event.cls_t

  class cls (seq:int64) = object
    inherit [unit option] Event.cls seq Initialized None
  end

  let enc = Event.enc (Data_encoding.(option unit))

end


module StoppedEvent = struct

  type reason =
    | Step
    | Breakpoint
    | Exception
    | Pause
    | Entry
    | Goto
    | Function_breakpoint
    | Data_breakpoint
    | Instruction_breakpoint

  let enc_reason =
    let open Data_encoding in
    conv
      (function
        | Step -> "step"
        | Breakpoint -> "breakpoint"
        | Exception -> "exception"
        | Pause -> "pause"
        | Entry -> "entry"
        | Goto -> "goto"
        | Function_breakpoint -> "function breakpoint"
        | Data_breakpoint -> "data breakpoint"
        | Instruction_breakpoint -> "instruction breakpoint"
      )
      (function
        | "step" -> Step
        | "breakpoint" -> Breakpoint
        | "exception" -> Exception
        | "pause" -> Pause
        | "entry" -> Entry
        | "goto" -> Goto
        | "function breakpoint" -> Function_breakpoint
        | "data breakpoint" -> Data_breakpoint
        | "instruction breakpoint" -> Instruction_breakpoint
        | _ -> failwith "Unknown stopping reason"
      )
      string

  type body = {
    reason: reason;
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

  let enc =
    let open Data_encoding in
    Event.enc @@
    conv
      (fun {
         reason;
         description;
         threadId;
         preserveFocusHint;
         text;
         allThreadsStopped;
         hitBreakpointIds
       } -> (
           reason,
           description,
           threadId,
           preserveFocusHint,
           text,
           allThreadsStopped,
           hitBreakpointIds
         ))
      (fun (
           reason,
           description,
           threadId,
           preserveFocusHint,
           text,
           allThreadsStopped,
           hitBreakpointIds
         ) -> {
         reason;
         description;
         threadId;
         preserveFocusHint;
         text;
         allThreadsStopped;
         hitBreakpointIds
       })
      (obj7
         (req "reason" enc_reason)
         (opt "description" string)
         (opt "threadId" int64)
         (opt "preserveFocusHint" bool)
         (opt "text" string)
         (opt "allThreadsStopped" bool)
         (opt "hitBreakpointIds" (list int64))
         )

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

  let enc =
    let open Data_encoding in
    Event.enc @@
    conv
      (fun {threadId; allThreadsContinued} -> (threadId, allThreadsContinued))
      (fun (threadId, allThreadsContinued) -> {threadId; allThreadsContinued})
      (obj2
         (req "threadId" int64)
         (opt "allThreadsContinued" bool))

end


module ExitedEvent = struct

  type body = {
    exitCode: int64;
  }

  type cls_t = body Event.cls_t

  class cls (seq:int64) (body:body) = object
    inherit [body] Event.cls seq Exited body
  end

  let enc =
    let open Data_encoding in
    Event.enc @@
    conv
      (fun {exitCode} -> exitCode)
      (fun exitCode -> {exitCode})
      (obj1
         (req "exitCode" int64))

end


module TerminatedEvent = struct

  type 'json body = {
    restart: 'json
  }

  type 'json cls_t = 'json body option Event.cls_t

  class ['json] cls (seq:int64) (body:'json body option) = object
    inherit ['json body option] Event.cls seq Terminated body
  end

  let enc =
    let open Data_encoding in
    Event.enc @@
    conv
      (fun {restart} -> restart)
      (fun restart -> {restart})
      (obj1
         (req "restart" json))

end


module ThreadEvent = struct

  type reason =
    | Started
    | Exited

  let enc_reason =
    let open Data_encoding in
    conv
      (function | Started -> "started" | Exited -> "exited")
      (function | "started" -> Started | "exited" -> Exited | _ -> failwith "Unknown thread reason")
      string

  type body = {
    reason: reason;
    threadId: int64;
  }

  type cls_t = body Event.cls_t

  class cls (seq:int64) (body:body) = object
    inherit [body] Event.cls seq Thread body
  end

  let enc =
    let open Data_encoding in
    Event.enc @@
    conv
      (fun {reason; threadId} -> (reason, threadId))
      (fun (reason, threadId) -> {reason; threadId})
      (obj2
         (req "reason" enc_reason)
         (req "threadId" int64))

end


module OutputEvent = struct

  type category =
    | Console
    | Important
    | Stdout
    | Stderr
    | Telemetry

  let enc_category =
    let open Data_encoding in
    conv
      (function
        | Console -> "console"
        | Important -> "important"
        | Stdout -> "stdout"
        | Stderr -> "stderr"
        | Telemetry -> "telemetry"
      )
      (function
        | "console" -> Console
        | "important" -> Important
        | "stdout" -> Stdout
        | "stderr" -> Stderr
        | "telemetry" -> Telemetry
        | _ -> failwith "Unknown output category"
      )
      string

  type group =
    | Start
    | StartCollapsed
    | End

  let enc_group =
    let open Data_encoding in
    conv
      (function
        | Start -> "start"
        | StartCollapsed -> "startCollapsed"
        | End -> "end"
      )
      (function
        | "start" -> Start
        | "startCollapsed" -> StartCollapsed
        | "end" -> End
        | _ -> failwith "Unknown output group"
      )
      string

  type 'json body = {
    output: string;
    category: category option;
    group: group option;
    variablesReference: int64 option;
    source: 'json Source.t option;
    line: int64 option;
    column: int64 option;
    data: 'json option;
  }

  type 'json cls_t = 'json body Event.cls_t

  class ['json] cls
      (seq:int64)
      (body:'json body) = object
    inherit ['json body] Event.cls seq Output body
  end

  let enc =
    let open Data_encoding in
    Event.enc @@
    conv
      (fun {
         output;
         category;
         group;
         variablesReference;
         source;
         line;
         column;
         data
       } -> (
           output,
           category,
           group,
           variablesReference,
           source,
           line,
           column,
           data
         )
      )
      (fun (
         output,
         category,
         group,
         variablesReference,
         source,
         line,
         column,
         data
       )
         -> {
             output;
             category;
             group;
             variablesReference;
             source;
             line;
             column;
             data
           }
      )
      (obj8
         (req "output" string)
         (opt "category" enc_category)
         (opt "group" enc_group)
         (opt "variablesReference" int64)
         (opt "source" (Source.enc json))
         (opt "line" int64)
         (opt "column" int64)
         (opt "data" json)
      )

end


module BreakpointEvent = struct

  type reason =
    | Changed
    | New
    | Removed

  type 'json body = {
    reason: reason;
    breakpoint: 'json Breakpoint.t;
  }

  type 'json cls_t = 'json body Event.cls_t

  class ['json] cls (seq:int64) (body:'json body) = object
    inherit ['json body] Event.cls seq Breakpoint body
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

  type 'json body = {
    reason: reason;
    source: 'json Source.t;
  }

  type 'json cls_t = 'json body Event.cls_t

  class ['json] cls (seq:int64) (body:'json body) = object
    inherit ['json body] Event.cls seq LoadedSource body
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
