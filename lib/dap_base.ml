type msg_t =
  | Request
  | Response
  | Event

module ProtocolMessage = struct

  type cls_t = < seq:int64; type_:msg_t >

  class cls (seq:int64) (type_:msg_t) = object
    method seq = seq
    method type_ = type_
  end

end


module CancelArguments = struct

  type t = {
    requestId:int64 option;
    progressId:string option
  }

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


module ChecksumAlgorithm = struct

  type t =
    | MD5
    | SHA1
    | SHA256
    | Timestamp

end

module Checksum = struct

  type t = {
    algorithm: ChecksumAlgorithm.t;
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


module Breakpoint = struct

  type 'data t = {
    id: int64 option;
    verified: bool;
    message: string option;
    source: 'data Source.t option;
    line: int64 option;
    column: int64 option;
    endLine: int64 option;
    endColumn: int64 option;
    instructionReference: string option;
    offset: int64 option;
  }

end



module Module_ = struct

  type id =
    | I of int
    | S of string

  type t = {
    id: id;
    name: string;
    path: string option;
    isOptimized: bool option;
    isUserCode: bool option;
    version: string option;
    symbolStatus: string option;
    symbolFilePath: string option;
    dateTimeStamp: string option;
    addressRange: string option;
  }

end


module ExceptionBreakpointsFilter = struct

  type t = {
    filter: string;
    label: string;
    description: string option;
    default: bool option;
    supportsCondition: bool option;
    conditionDescription: string option;
  }

end

module ColumnDescriptor = struct

  type column_type =
    | String
    | Number
    | Boolean
    | UnixTimestampUTC

  type t = {
    attributeName: string;
    label: string;
    format: string option;
    type_: column_type option;
    width: int64 option
  }

end


module Capabilities = struct

  type t = {
        supportsConfigurationDoneRequest: bool option;
        supportsFunctionBreakpoints: bool option;
        supportsConditionalBreakpoints: bool option;
        supportsHitConditionalBreakpoints: bool option;
        supportsEvaluateForHovers: bool option;
        exceptionBreakpointFilters: ExceptionBreakpointsFilter.t list option;
        supportsStepBack: bool option;
        supportsSetVariable: bool option;
        supportsRestartFrame: bool option;
        supportsGotoTargetsRequest: bool option;
        supportsStepInTargetsRequest: bool option;
        supportsCompletionsRequest: bool option;
        completionTriggerCharacters: string list option;
        supportsModulesRequest: bool option;
        additionalModuleColumns: ColumnDescriptor.t option;
        supportedChecksumAlgorithms: ChecksumAlgorithm.t list option;
        supportsRestartRequest: bool option;
        supportsExceptionOptions: bool option;
        supportsValueFormattingOptions: bool option;
        supportsExceptionInfoRequest: bool option;
        supportTerminateDebuggee: bool option;
        supportSuspendDebuggee: bool option;
        supportsDelayedStackTraceLoading: bool option;
        supportsLoadedSourcesRequest: bool option;
        supportsLogPoints: bool option;
        supportsTerminateThreadsRequest: bool option;
        supportsSetExpression: bool option;
        supportsTerminateRequest: bool option;
        supportsDataBreakpoints: bool option;
        supportsReadMemoryRequest: bool option;
        supportsWriteMemoryRequest: bool option;
        supportsDisassembleRequest: bool option;
        supportsCancelRequest: bool option;
        supportsBreakpointLocationsRequest: bool option;
        supportsClipboardContext: bool option;
        supportsSteppingGranularity: bool option;
        supportsInstructionBreakpoints: bool option;
        supportsExceptionFilterOptions: bool option;
        supportsSingleThreadExecutionRequests: bool option;
}

end


module InvalidatedAreas = struct
  type t =
    | All
    | Stacks
    | Threads
    | Variables
end
