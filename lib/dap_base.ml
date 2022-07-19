module ProtocolMessage = struct

  type t =
    | Request
    | Response
    | Event

  let enc_t =
    let open Data_encoding in
    conv
      (function | Request -> "request" | Response -> "response" | Event -> "event")
      (function | "request" -> Request | "response" -> Response | "event" -> Event | _ -> failwith "Unknown message type")
      string

  type cls_t = < seq:int64; type_:t >

  class cls (seq:int64) (type_:t) = object
    method seq = seq
    method type_ = type_
  end

end

module type ENC_0 = sig
  type t
  val enc : t Data_encoding.t
end

module type ENC_1 = sig
  type 'a t
  val enc : 'a Data_encoding.t -> 'a t Data_encoding.t
end

module CancelArguments = struct

  type t = {
    requestId:int64 option;
    progressId:string option
  }

  let enc =
    let open Data_encoding in
    conv
      (fun {requestId; progressId} -> (requestId, progressId))
      (fun (requestId, progressId) -> {requestId; progressId})
      (obj2
         (opt "requestId" int64)
         (opt "progressId" string))

end


module Message = struct

    (* TODO not sure whats going on with the variables field
       "Message": {
     *   "type": "object",
     *   "description": "A structured message object. Used to return errors from requests.",
     *   "properties": {
     *     "id": {
     *       "type": "integer",
     *       "description": "Unique identifier for the message."
     *     },
     *     "format": {
     *       "type": "string",
     *       "description": "A format string for the message. Embedded variables have the form '{name}'.\nIf variable name starts with an underscore character, the variable does not contain user data (PII) and can be safely used for telemetry purposes."
     *     },
     *     "variables": {
     *       "type": "object",
     *       "description": "An object used as a dictionary for looking up the variables in the format string.",
     *       "additionalProperties": {
     *         "type": "string",
     *         "description": "Values must be strings."
     *       }
     *     },
     *     "sendTelemetry": {
     *       "type": "boolean",
     *       "description": "If true send to telemetry."
     *     },
     *     "showUser": {
     *       "type": "boolean",
     *       "description": "If true show user."
     *     },
     *     "url": {
     *       "type": "string",
     *       "description": "An optional url where additional information about this message can be found."
     *     },
     *     "urlLabel": {
     *       "type": "string",
     *       "description": "An optional label that is presented to the user as the UI for opening the url."
     *     }
     *   },
     *   "required": ["id", "format"]
     * }, *)


  type t = {
    id: int64;
    format: string;
    variables: (string * string) list option;
    sendTelemetry: bool option;
    showUser: bool option;
    url: string option;
    urlLabel: string option;
  }

  let enc =
    let open Data_encoding in
    conv
      (fun {id; format; variables; sendTelemetry; showUser; url; urlLabel} -> (id, format, variables, sendTelemetry, showUser, url, urlLabel))
      (fun (id, format, variables, sendTelemetry, showUser, url, urlLabel) -> {id; format; variables; sendTelemetry; showUser; url; urlLabel})
      (obj7
         (req "id" int64)
         (req "format" string)
         (opt "variables" (list @@ tup2 string string))
         (opt "sendTelemetry" bool)
         (opt "showUser" bool)
         (opt "url" string)
         (opt "urlLabel" string))

end


module ChecksumAlgorithm = struct

  type t =
    | MD5
    | SHA1
    | SHA256
    | Timestamp

  let enc =
    let open Data_encoding in
    conv
      (function | MD5 -> "MD5" | SHA1 -> "SHA1" | SHA256 -> "SHA256" | Timestamp -> "timestamp")
      (function | "MD5" -> MD5 | "SHA1" -> SHA1 | "SHA256" -> SHA256 | "timestamp" -> Timestamp | _ -> failwith "Unknown Checksum")
      string

end

module Checksum = struct

  type t = {
    algorithm: ChecksumAlgorithm.t;
    checksum: string;
  }

  let enc =
    let open Data_encoding in
    conv
      (fun {algorithm; checksum} -> (algorithm, checksum))
      (fun (algorithm, checksum) -> {algorithm; checksum})
      (obj2
         (req "algorithm" ChecksumAlgorithm.enc)
         (req "checksum" string))

end


module Source = struct

  type hint =
    | Normal
    | Emphasize
    | Deemphasize

  let hint_enc =
    let open Data_encoding in
    conv
      (function | Normal -> "normal" | Emphasize -> "emphasize" | Deemphasize -> "deemphasize")
      (function | "normal" -> Normal | "emphasize" -> Emphasize | "deemphasize" -> Deemphasize | _ -> failwith "Unknown hint")
      string

  type 'json t = {
    name: string option;
    path: string option;
    sourceReference: int64 option;
    presentationHint: hint option;
    origin: string option;
    sources: 'json t list option;
    adapterData: 'json option;
    checksums: Checksum.t list option;
  }

  let enc json_enc =
    let open Data_encoding in
    mu "t" (fun e ->
        conv
          (fun {
             name;
             path;
             sourceReference;
             presentationHint;
             origin;
             sources;
             adapterData;
             checksums;
           } -> (
               name,
               path,
               sourceReference,
               presentationHint,
               origin,
               sources,
               adapterData,
               checksums
             ))
          (fun (
             name,
             path,
             sourceReference,
             presentationHint,
             origin,
             sources,
             adapterData,
             checksums
           ) -> {
               name;
               path;
               sourceReference;
               presentationHint;
               origin;
               sources;
               adapterData;
               checksums;
             })
          (obj8
             (opt "name" string)
             (opt "path" string)
             (opt "sourceReference" int64)
             (opt "presentationHint" hint_enc)
             (opt "origin" string)
             (opt "sources" (list e))
             (opt "adapterData" json_enc)
             (opt "checksums" (list Checksum.enc))
          )
      )
end


module Breakpoint = struct

  type 'json t = {
    id: int64 option;
    verified: bool;
    message: string option;
    source: 'json Source.t option;
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
