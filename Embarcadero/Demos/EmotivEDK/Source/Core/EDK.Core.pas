{
  ORIGINAL FILE: edk.h
  ---------------------------------------------------------------------------------------------------------
  |Emotiv Development Kit (EDK) API                                                                       |
  |Copyright (c) 2009 Emotiv Systems, Inc.                                                                |
  |                                                                                                       |
  |The main interface that allows interactions between external programs and the Emotiv detection engine. |
  |                                                                                                       |
  |None of the API functions are thread-safe.                                                             |
  |                                                                                                       |
  |This header file is designed to be includable under C and C++ environment.                             |
  ---------------------------------------------------------------------------------------------------------

  Translated by LaKraven Studios Ltd (14th October 2011)
  Copyright (C) 2011, LaKraven Studios Ltd, All Rights Reserved
  Last Updated: 14th October 2011
}
unit EDK.Core;

interface

uses
  EDK.ErrorCodes, EDK.EmoState;

type
  { Enums }
  EE_ExpressivThreshold_t = (EXP_SENSITIVITY);
  TExpressivThreshold = EE_ExpressivThreshold_t;

  EE_ExpressivTrainingControl_t = (EXP_NONE, EXP_START, EXP_ACCEPT, EXP_REJECT, EXP_ERASE, EXP_RESET);
  TExpressivTrainingControl = EE_ExpressivTrainingControl_t;

  PEE_ExpressivSignature_t = ^EE_ExpressivSignature_t;
  EE_ExpressivSignature_t = (EXP_SIG_UNIVERSAL, EXP_SIG_TRAINED);
  PExpressivSignature = ^TExpressivSignature;
  TExpressivSignature = EE_ExpressivSignature_t;

  EE_CognitivTrainingControl_t = (COG_NONE, COG_START, COG_ACCEPT, COG_REJECT, COG_ERASE, COG_RESET);
  TCognitivTrainingControl = EE_CognitivTrainingControl_t;

  EE_Event_t = (EE_UnknownEvent = $0000, EE_EmulatorError	= $0001, EE_ReservedEvent	= $0002, EE_UserAdded = $0010,
                EE_UserRemoved = $0020, EE_EmoStateUpdated = $0040, EE_ProfileEvent = $0080, EE_CognitivEvent = $0100,
		            EE_ExpressivEvent	= $0200, EE_InternalStateChanged = $0400,
                EE_AllEvent = EE_UserAdded + EE_UserRemoved + EE_EmoStateUpdated + EE_ProfileEvent + EE_CognitivEvent +
                EE_ExpressivEvent + EE_InternalStateChanged);
  TEDKEvent = EE_Event_t;

  EE_ExpressivEvent_t = (EE_ExpressivNoEvent = 0, EE_ExpressivTrainingStarted, EE_ExpressivTrainingSucceeded,
		                     EE_ExpressivTrainingFailed, EE_ExpressivTrainingCompleted, EE_ExpressivTrainingDataErased,
		                     EE_ExpressivTrainingRejected, EE_ExpressivTrainingReset);
  TExpressivEvent = EE_ExpressivEvent_t;

  EE_CognitivEvent_t = (EE_CognitivNoEvent = 0, EE_CognitivTrainingStarted, EE_CognitivTrainingSucceeded,
		                    EE_CognitivTrainingFailed, EE_CognitivTrainingCompleted, EE_CognitivTrainingDataErased,
		                    EE_CognitivTrainingRejected, EE_CognitivTrainingReset,
		                    EE_CognitivAutoSamplingNeutralCompleted, EE_CognitivSignatureUpdated);
  TCognitivEvent = EE_CognitivEvent_t;

  { Pointers }

	EmoEngineEventHandle = Pointer;
  PEmoEngineEventHandle = EmoEngineEventHandle;

	OptimizationParamHandle = Pointer;
  POptimizationParamHandle = OptimizationParamHandle;

	DataHandle = Pointer;
  PDataHandle = DataHandle;

  { Structures }
  PInputSensorDescriptor_t = ^InputSensorDescriptor_t;
  InputSensorDescriptor_t = record
    channelId: EE_InputChannels_t;
    fExists: Integer;
    pszLabel: PAnsiChar;
    xLoc,
    yLoc,
    zLoc: Double;
  end;
  TInputSensorDescriptor = InputSensorDescriptor_t;

function EE_EngineConnect(const strDevID: PAnsiChar): Integer; cdecl; external EDK_DLL;
function EE_EngineRemoteConnect(const szHost: PAnsiChar; port: Word; const strDevID: PAnsiChar): Integer; cdecl; external EDK_DLL;
function EE_EngineDisconnect: Integer; cdecl; external EDK_DLL;
function EE_EnableDiagnostics(const szFileName: PAnsiChar; fEnable: Integer; nReserved: Integer): Integer; cdecl; external EDK_DLL;
function EE_EmoEngineEventCreate: EmoEngineEventHandle; cdecl; external EDK_DLL;
function EE_ProfileEventCreate: EmoEngineEventHandle; cdecl; external EDK_DLL;
procedure EE_EmoEngineEventFree(hEvent: EmoEngineEventHandle); cdecl; external EDK_DLL;
function EE_EmoStateCreate: EmoStateHandle; cdecl; external EDK_DLL;
procedure EE_EmoStateFree(hState: EmoStateHandle); cdecl; external EDK_DLL;
function EE_EmoEngineEventGetType(hEvent: EmoEngineEventHandle): EE_Event_t; cdecl; external EDK_DLL;
function EE_CognitivEventGetType(hEvent: EmoEngineEventHandle): EE_CognitivEvent_t; cdecl; external EDK_DLL;
function EE_ExpressivEventGetType(hEvent: EmoEngineEventHandle): EE_ExpressivEvent_t; cdecl; external EDK_DLL;
function EE_EmoEngineEventGetUserId(hEvent: EmoEngineEventHandle; pUserIdOut: PWord): Integer; cdecl; external EDK_DLL;
function EE_EmoEngineEventGetEmoState(hEvent: EmoEngineEventHandle; hEmoState: EmoStateHandle): Integer; cdecl; external EDK_DLL;
function EE_EngineGetNextEvent(hEvent: EmoEngineEventHandle): Integer; cdecl; external EDK_DLL;
function EE_EngineClearEventQueue(eventTypes: Integer): Integer; cdecl; external EDK_DLL;
function EE_EngineGetNumUser(pNumUserOut: PWord): Integer; cdecl; external EDK_DLL;
function EE_SetHardwarePlayerDisplay(userId: Word; playerNum: Word): Integer; cdecl; external EDK_DLL;
function EE_SetUserProfile(userId: Word; const profileBuffer: Array of Byte; length: Word): Integer; cdecl; external EDK_DLL;
function EE_GetUserProfile(userId: Word; hEvent: EmoEngineEventHandle): Integer; cdecl; external EDK_DLL;
function EE_GetBaseProfile(hEvent: EmoEngineEventHandle): Integer; cdecl; external EDK_DLL;
function EE_GetUserProfileSize(hEvt: EmoEngineEventHandle; pProfileSizeOut: PWord): Integer; cdecl; external EDK_DLL;
function EE_GetUserProfileBytes(hEvt: EmoEngineEventHandle; destBuffer: Array of Byte; length: Word): Integer; cdecl; external EDK_DLL;
function EE_LoadUserProfile(userID: Word; const szInputFilename: PAnsiChar): Integer; cdecl; external EDK_DLL;
function EE_SaveUserProfile(userID: Word; const szOutputFileName: PAnsiChar): Integer; cdecl; external EDK_DLL;
function EE_ExpressivSetThreshold(userId: Word; algoName: EE_ExpressivAlgo_t; thresholdName: EE_ExpressivThreshold_t; value: Integer): Integer; cdecl; external EDK_DLL;
function EE_ExpressivGetThreshold(userId: Word; algoName: EE_ExpressivAlgo_t; thresholdName: EE_ExpressivThreshold_t; pValueOut: PInteger): Integer; cdecl; external EDK_DLL;
function EE_ExpressivSetTrainingAction(userId: Word; action: EE_ExpressivAlgo_t): Integer; cdecl; external EDK_DLL;
function EE_ExpressivSetTrainingControl(userId: Word; control: EE_ExpressivTrainingControl_t): Integer; cdecl; external EDK_DLL;
function EE_ExpressivGetTrainingAction(userId: Word; pActionOut: EE_ExpressivAlgo_t): Integer; cdecl; external EDK_DLL;
function EE_ExpressivGetTrainingTime(userId: Word; pTrainingTimeOut: PWord): Integer; cdecl; external EDK_DLL;
function EE_ExpressivGetTrainedSignatureActions(userId: Word; pTrainedActionsOut: PCardinal): Integer; cdecl; external EDK_DLL;
function EE_ExpressivGetTrainedSignatureAvailable(userId: Word; pfAvailableOut: PInteger): Integer; cdecl; external EDK_DLL;
function EE_ExpressivSetSignatureType(userId: Word; sigType: EE_ExpressivSignature_t): Integer; cdecl; external EDK_DLL;
function EE_ExpressivGetSignatureType(userId: Integer; pSigTypeOut: PEE_ExpressivSignature_t): Integer; cdecl; external EDK_DLL;
function EE_CognitivSetActiveActions(userId: Word; activeActions: Cardinal): Integer; cdecl; external EDK_DLL;
function EE_CognitivGetActiveActions(userId: Word; pActiveActionsOut: PCardinal): Integer; cdecl; external EDK_DLL;
function EE_CognitivGetTrainingTime(userId: Word; pTrainingTimeOut: PWord): Integer; cdecl; external EDK_DLL;
function EE_CognitivSetTrainingControl(userId: Word; control: EE_CognitivTrainingControl_t): Integer; cdecl; external EDK_DLL;
function EE_CognitivSetTrainingAction(userId: Word; action: EE_CognitivAction_t): Integer; cdecl; external EDK_DLL;
function EE_CognitivGetTrainingAction(userId: Word; pActionOut: PEE_CognitivAction_t): Integer; cdecl; external EDK_DLL;
function EE_CognitivGetTrainedSignatureActions(userId: Word; pTrainedActionOut: PCardinal): Integer; cdecl; external EDK_DLL;
function EE_CognitivGetOverallSkillRating(userId: Word; pOverallSkillRatingOut: PSingle): Integer; cdecl; external EDK_DLL;
function EE_CognitivGetActionSkillRating(userId: Word; action: EE_CognitivAction_t; pActionSkillRatingOut: PSingle): Integer; cdecl; external EDK_DLL;
function EE_CognitivSetActivationLevel(userId: Word; Level: Integer): Integer; cdecl; external EDK_DLL;
function EE_CognitivSetActionSensitivity(userId: Word; action1Sensitivity, action2Sensitivity, action3Sensitivity, action4Sensitivity: Integer): Integer; cdecl; external EDK_DLL;
function EE_CognitivGetActivationLevel(userId: Word; pLevelOut: PInteger): Integer; cdecl; external EDK_DLL;
function EE_CognitivGetActionSensitivity(userId: Word; pAction1SensitivityOut, pAction2SensitivityOut, pAction3SensitivityOut, pAction4SensitivityOut: PInteger): Integer; cdecl; external EDK_DLL;
function EE_CognitivStartSamplingNeutral(userId: Word): Integer; cdecl; external EDK_DLL;
function EE_CognitivStopSamplingNeutral(userId: Word): Integer; cdecl; external EDK_DLL;
function EE_CognitivSetSignatureCaching(userId: Word; enabled: Cardinal): Integer; cdecl; external EDK_DLL;
function EE_CognitivGetSignatureCaching(userId: Word; pEnabledOut: PCardinal): Integer; cdecl; external EDK_DLL;
function EE_CognitivSetSignatureCacheSize(userId: Word; size: Cardinal): Integer; cdecl; external EDK_DLL;
function EE_CognitivGetSignatureCacheSize(userId: Word; pSizeOut: PCardinal): Integer; cdecl; external EDK_DLL;
function EE_HeadsetGetSensorDetails(channelId: EE_InputChannels_t; pDescriptorOut: PInputSensorDescriptor_t): Integer; cdecl; external EDK_DLL;
function EE_HardwareGetVersion(userId: Word; pHwVersionOut: PCardinal): Integer; cdecl; external EDK_DLL;
function EE_SoftwareGetVersion(pszVersionOut: PAnsiChar; nVersionChars: Cardinal; pBuildNumOut: PCardinal): Integer; cdecl; external EDK_DLL;
function EE_HeadsetGetGyroDelta(userId: Word; pXOut, pYOut: PInteger): Integer; cdecl; external EDK_DLL;
function EE_HeadsetGyroRezero(userId: Word): Integer; cdecl; external EDK_DLL;
function EE_OptimizationParamCreate: OptimizationParamHandle; cdecl; external EDK_DLL;
procedure EE_OptimizationParamFree(hParam: OptimizationParamHandle); cdecl; external EDK_DLL;
function EE_OptimizationEnable(hParam: OptimizationParamHandle): Integer; cdecl; external EDK_DLL;
function EE_OptimizationIsEnabled(pEnabledOut: PBoolean): Integer; cdecl; external EDK_DLL;
function EE_OptimizationDisable: Integer; cdecl; external EDK_DLL;
function EE_OptimizationGetParam(hParam: OptimizationParamHandle): Integer; cdecl; external EDK_DLL;
function EE_OptimizationGetVitalAlgorithm(hParam: OptimizationParamHandle; suite: EE_EmotivSuite_t; pVitalAlgorithmBitVectorOut: PCardinal): Integer; cdecl; external EDK_DLL;
function EE_OptimizationSetVitalAlgorithm(hParam: OptimizationParamHandle; suite: EE_EmotivSuite_t; vitalAlgorithmBitVector: Integer): Integer; cdecl; external EDK_DLL;
function EE_ResetDetection(userId: Word; suite: EE_EmotivSuite_t; detectionBitVector: Word): Integer; cdecl; external EDK_DLL;
function EE_GetSecurityCode: Double; cdecl; external EDK_DLL;
function EE_CheckSecurityCode(x: Double): Boolean; cdecl; external EDK_DLL;

implementation

end.
