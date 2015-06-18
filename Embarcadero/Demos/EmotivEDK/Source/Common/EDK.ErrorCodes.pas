{
  ORIGINAL FILE: edkErrorCode.h
  ------------------------------------------
  |Emotiv Development Kit API return values|
  |Copyright (c) 2009 Emotiv Systems, Inc. |
  ------------------------------------------

  Translated by LaKraven Studios Ltd (14th October 2011)
  Copyright (C) 2011, LaKraven Studios Ltd, All Rights Reserved
  Last Updated: 14th October 2011
}
unit EDK.ErrorCodes;

interface

const
  EDK_OK	                          = $0000;  // Default success value
  EDK_UNKNOWN_ERROR                 = $0001;  // An internal error occurred
  EDK_INVALID_DEV_ID_ERROR          = $0002;  // Invalid Developer ID
  EDK_INVALID_PROFILE_ARCHIVE       = $0101;  // The contents of the buffer supplied to EE_SetUserProfile aren't a valid, serialized EmoEngine profile.
  EDK_NO_USER_FOR_BASEPROFILE       = $0102;  // Returned from EE_EmoEngineEventGetUserId if the event supplied contains a base profile (which isn't associated with specific user).
  EDK_CANNOT_ACQUIRE_DATA           = $0200;  // The EmoEngine is unable to acquire EEG data for processing.
  EDK_BUFFER_TOO_SMALL              = $0300;  // The buffer supplied to the function isn't large enough
  EDK_OUT_OF_RANGE                  = $0301;  // A parameter supplied to the function is out of range
  EDK_INVALID_PARAMETER             = $0302;  // One of the parameters supplied to the function is invalid
  EDK_PARAMETER_LOCKED              = $0303;  // The parameter value is currently locked by a running detection and cannot be modified at this time.
  EDK_COG_INVALID_TRAINING_ACTION   = $0304;  // The current training action is not in the list of expected training actions
  EDK_COG_INVALID_TRAINING_CONTROL  = $0305;  // The current training control is not in the list of expected training controls
  EDK_COG_INVALID_ACTIVE_ACTION     = $0306;  // One of the field in the action bits vector is invalid
  EDK_COG_EXCESS_MAX_ACTIONS        = $0307;  // The current action bits vector contains more action types than it is allowed
  EDK_EXP_NO_SIG_AVAILABLE          = $0308;  // A trained signature is not currently available for use - addition actions (including neutral) may be required
  EDK_FILESYSTEM_ERROR              = $0309;  // A filesystem error occurred that prevented the function from succeeding
  EDK_INVALID_USER_ID               = $0400;  // The user ID supplied to the function is invalid
  EDK_EMOENGINE_UNINITIALIZED       = $0500;  // The EDK needs to be initialized via EE_EngineConnect or EE_EngineRemoteConnect
  EDK_EMOENGINE_DISCONNECTED        = $0501;  // The connection with a remote instance of the EmoEngine (made via EE_EngineRemoteConnect) has been lost
  EDK_EMOENGINE_PROXY_ERROR         = $0502;  // The API was unable to establish a connection with a remote instance of the EmoEngine.
  EDK_NO_EVENT                      = $0600;  // There are no new EmoEngine events at this time
  EDK_GYRO_NOT_CALIBRATED           = $0700;  // The gyro is not calibrated. Ask the user to stay still for at least 0.5s
  EDK_OPTIMIZATION_IS_ON            = $0800;  // Operation failure due to optimization
  EDK_RESERVED1                     = $0900;  // Reserved return value

function EDK_ErrorToString(error: integer): string;
procedure RaiseEdkError(error: integer);

implementation

uses SysUtils;

procedure RaiseEdkError(error: integer);
begin
  if error <> EDK_OK then
  begin
    raise Exception.Create(EDK_ErrorToString(Error)) at ReturnAddress;
  end;
end;

function EDK_ErrorToString(error: integer): string;
begin
  case error of
    EDK_OK	                          : Result := '0x0000 Default success value.';
    EDK_UNKNOWN_ERROR                 : Result := '0x0001 An internal error occurred.';
    EDK_INVALID_DEV_ID_ERROR          : Result := '0x0002 Invalid Developer ID.';
    EDK_INVALID_PROFILE_ARCHIVE       : Result := '0x0101 The contents of the buffer supplied to EE_SetUserProfile aren''t a valid, serialized EmoEngine profile.';
    EDK_NO_USER_FOR_BASEPROFILE       : Result := '0x0102 Returned from EE_EmoEngineEventGetUserId if the event supplied contains a base profile (which isn''t associated with specific user).';
    EDK_CANNOT_ACQUIRE_DATA           : Result := '0x0200 The EmoEngine is unable to acquire EEG data for processing.';
    EDK_BUFFER_TOO_SMALL              : Result := '0x0300 The buffer supplied to the function isn''t large enough.';
    EDK_OUT_OF_RANGE                  : Result := '0x0301 A parameter supplied to the function is out of range.';
    EDK_INVALID_PARAMETER             : Result := '0x0302 One of the parameters supplied to the function is invalid.';
    EDK_PARAMETER_LOCKED              : Result := '0x0303 The parameter value is currently locked by a running detection and cannot be modified at this time.';
    EDK_COG_INVALID_TRAINING_ACTION   : Result := '0x0304 The current training action is not in the list of expected training actions.';
    EDK_COG_INVALID_TRAINING_CONTROL  : Result := '0x0305 The current training control is not in the list of expected training controls.';
    EDK_COG_INVALID_ACTIVE_ACTION     : Result := '0x0306 One of the field in the action bits vector is invalid.';
    EDK_COG_EXCESS_MAX_ACTIONS        : Result := '0x0307 The current action bits vector contains more action types than it is allowed.';
    EDK_EXP_NO_SIG_AVAILABLE          : Result := '0x0308 A trained signature is not currently available for use - addition actions (including neutral) may be required.';
    EDK_FILESYSTEM_ERROR              : Result := '0x0309 A filesystem error occurred that prevented the function from succeeding.';
    EDK_INVALID_USER_ID               : Result := '0x0400 The user ID supplied to the function is invalid.';
    EDK_EMOENGINE_UNINITIALIZED       : Result := '0x0500 The EDK needs to be initialized via EE_EngineConnect or EE_EngineRemoteConnect.';
    EDK_EMOENGINE_DISCONNECTED        : Result := '0x0501 The connection with a remote instance of the EmoEngine (made via EE_EngineRemoteConnect) has been lost.';
    EDK_EMOENGINE_PROXY_ERROR         : Result := '0x0502 The API was unable to establish a connection with a remote instance of the EmoEngine.';
    EDK_NO_EVENT                      : Result := '0x0600 There are no new EmoEngine events at this time.';
    EDK_GYRO_NOT_CALIBRATED           : Result := '0x0700 The gyro is not calibrated. Ask the user to stay still for at least 0.5s.';
    EDK_OPTIMIZATION_IS_ON            : Result := '0x0800 Operation failure due to optimization.';
    EDK_RESERVED1                     : Result := '0x0900 Reserved return value.';
  end;
end;

end.
