{*******************************************************}
{                                                       }
{                Delphi Runtime Library                 }
{                                                       }
{ Files: bluetoothleapis.h bthledef.h                   }
{         Copyright (C) Microsoft Corporation.          }
{         All Rights Reserved.                          }
{                                                       }
{       Translator: Embarcadero Technologies, Inc.      }
{ Copyright(c) 1995-2014 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit Winapi.BluetoothLE;

{$ALIGN ON}
{$MINENUMSIZE 4}
{$WEAKPACKAGEUNIT}

interface

uses
  Winapi.Windows;

{$HPPEMIT '#include <bthledef.h>'}
{$HPPEMIT '#include <bluetoothleapis.h>'}



///////////////////////////////
///
///  Start bthledef.h
///
///////////////////////////////

{ Bluetooth LE device interface GUID }
const
  GUID_BLUETOOTHLE_DEVICE_INTERFACE: TGUID = '{781AEE18-7733-4CE4-ADD0-91F41C67B592}';
  {$EXTERNALSYM GUID_BLUETOOTHLE_DEVICE_INTERFACE}

{ Bluetooth LE Service device interface GUID }
  GUID_BLUETOOTH_GATT_SERVICE_DEVICE_INTERFACE: TGUID = '{6E3BB679-4372-40C8-9EAA-4509DF260CD8}';
  {$EXTERNALSYM GUID_BLUETOOTH_GATT_SERVICE_DEVICE_INTERFACE}

  BTH_LE_ATT_BLUETOOTH_BASE_GUID: TGUID = '{00000000-0000-1000-8000-00805F9B34FB}';
  {$EXTERNALSYM BTH_LE_ATT_BLUETOOTH_BASE_GUID}


{ ////////////////////////////////////////////////////////////////////////////// }
{ GATT Constants }
{ ////////////////////////////////////////////////////////////////////////////// }

const
  E_BLUETOOTH_ATT_INVALID_HANDLE = $80650001;
  {$EXTERNALSYM E_BLUETOOTH_ATT_INVALID_HANDLE}
  E_BLUETOOTH_ATT_READ_NOT_PERMITTED = $80650002;
  {$EXTERNALSYM E_BLUETOOTH_ATT_READ_NOT_PERMITTED}
  E_BLUETOOTH_ATT_WRITE_NOT_PERMITTED = $80650003;
  {$EXTERNALSYM E_BLUETOOTH_ATT_WRITE_NOT_PERMITTED}
  E_BLUETOOTH_ATT_INVALID_PDU = $80650004;
  {$EXTERNALSYM E_BLUETOOTH_ATT_INVALID_PDU}
  E_BLUETOOTH_ATT_INSUFFICIENT_AUTHENTICATION = $80650005;
  {$EXTERNALSYM E_BLUETOOTH_ATT_INSUFFICIENT_AUTHENTICATION}
  E_BLUETOOTH_ATT_REQUEST_NOT_SUPPORTED = $80650006;
  {$EXTERNALSYM E_BLUETOOTH_ATT_REQUEST_NOT_SUPPORTED}
  E_BLUETOOTH_ATT_INVALID_OFFSET = $80650007;
  {$EXTERNALSYM E_BLUETOOTH_ATT_INVALID_OFFSET}
  E_BLUETOOTH_ATT_INSUFFICIENT_AUTHORIZATION = $80650008;
  {$EXTERNALSYM E_BLUETOOTH_ATT_INSUFFICIENT_AUTHORIZATION}
  E_BLUETOOTH_ATT_PREPARE_QUEUE_FULL = $80650009;
  {$EXTERNALSYM E_BLUETOOTH_ATT_PREPARE_QUEUE_FULL}
  E_BLUETOOTH_ATT_ATTRIBUTE_NOT_FOUND = $8065000A;
  {$EXTERNALSYM E_BLUETOOTH_ATT_ATTRIBUTE_NOT_FOUND}
  E_BLUETOOTH_ATT_ATTRIBUTE_NOT_LONG = $8065000B;
  {$EXTERNALSYM E_BLUETOOTH_ATT_ATTRIBUTE_NOT_LONG}
  E_BLUETOOTH_ATT_INSUFFICIENT_ENCRYPTION_KEY_SIZE = $8065000C;
  {$EXTERNALSYM E_BLUETOOTH_ATT_INSUFFICIENT_ENCRYPTION_KEY_SIZE}
  E_BLUETOOTH_ATT_INVALID_ATTRIBUTE_VALUE_LENGTH = $8065000D;
  {$EXTERNALSYM E_BLUETOOTH_ATT_INVALID_ATTRIBUTE_VALUE_LENGTH}
  E_BLUETOOTH_ATT_UNLIKELY = $8065000E;
  {$EXTERNALSYM E_BLUETOOTH_ATT_UNLIKELY}
  E_BLUETOOTH_ATT_INSUFFICIENT_ENCRYPTION = $8065000F;
  {$EXTERNALSYM E_BLUETOOTH_ATT_INSUFFICIENT_ENCRYPTION}
  E_BLUETOOTH_ATT_UNSUPPORTED_GROUP_TYPE = $80650010;
  {$EXTERNALSYM E_BLUETOOTH_ATT_UNSUPPORTED_GROUP_TYPE}
  E_BLUETOOTH_ATT_INSUFFICIENT_RESOURCES = $80650011;
  {$EXTERNALSYM E_BLUETOOTH_ATT_INSUFFICIENT_RESOURCES}
  E_BLUETOOTH_ATT_UNKNOWN_ERROR = $80651000;
  {$EXTERNALSYM E_BLUETOOTH_ATT_UNKNOWN_ERROR}

{ Services UUIDs (Assigned numbers) }

const
  BTH_LE_SERVICE_GAP = $1800; 
  {$EXTERNALSYM BTH_LE_SERVICE_GAP}
  BTH_LE_SERVICE_GATT = $1801; 
  {$EXTERNALSYM BTH_LE_SERVICE_GATT}


{ GATT attribute types (Assigned numbers) }

  BTH_LE_GATT_ATTRIBUTE_TYPE_PRIMARY_SERVICE = $2800; 
  {$EXTERNALSYM BTH_LE_GATT_ATTRIBUTE_TYPE_PRIMARY_SERVICE}
  BTH_LE_GATT_ATTRIBUTE_TYPE_SECONDARY_SERVICE = $2801; 
  {$EXTERNALSYM BTH_LE_GATT_ATTRIBUTE_TYPE_SECONDARY_SERVICE}
  BTH_LE_GATT_ATTRIBUTE_TYPE_INCLUDE = $2802; 
  {$EXTERNALSYM BTH_LE_GATT_ATTRIBUTE_TYPE_INCLUDE}
  BTH_LE_GATT_ATTRIBUTE_TYPE_CHARACTERISTIC = $2803; 
  {$EXTERNALSYM BTH_LE_GATT_ATTRIBUTE_TYPE_CHARACTERISTIC}


{ GATT Characteristic Descriptors (Assigned numbers) }

  BTH_LE_GATT_CHARACTERISTIC_DESCRIPTOR_EXTENDED_PROPERTIES = $2900;
  {$EXTERNALSYM BTH_LE_GATT_CHARACTERISTIC_DESCRIPTOR_EXTENDED_PROPERTIES}
  BTH_LE_GATT_CHARACTERISTIC_DESCRIPTOR_USER_DESCRIPTION = $2901; 
  {$EXTERNALSYM BTH_LE_GATT_CHARACTERISTIC_DESCRIPTOR_USER_DESCRIPTION}
  BTH_LE_GATT_CHARACTERISTIC_DESCRIPTOR_CLIENT_CONFIGURATION = $2902; 
  {$EXTERNALSYM BTH_LE_GATT_CHARACTERISTIC_DESCRIPTOR_CLIENT_CONFIGURATION}
  BTH_LE_GATT_CHARACTERISTIC_DESCRIPTOR_SERVER_CONFIGURATION = $2903; 
  {$EXTERNALSYM BTH_LE_GATT_CHARACTERISTIC_DESCRIPTOR_SERVER_CONFIGURATION}
  BTH_LE_GATT_CHARACTERISTIC_DESCRIPTOR_FORMAT = $2904; 
  {$EXTERNALSYM BTH_LE_GATT_CHARACTERISTIC_DESCRIPTOR_FORMAT}
  BTH_LE_GATT_CHARACTERISTIC_DESCRIPTOR_AGGREGATE_FORMAT = $2905; 
  {$EXTERNALSYM BTH_LE_GATT_CHARACTERISTIC_DESCRIPTOR_AGGREGATE_FORMAT}


{ GATT Characteristic Types (Assigned numbers) }

  BTH_LE_GATT_CHARACTERISTIC_TYPE_DEVICE_NAME = $2A00; 
  {$EXTERNALSYM BTH_LE_GATT_CHARACTERISTIC_TYPE_DEVICE_NAME}
  BTH_LE_GATT_CHARACTERISTIC_TYPE_APPEARANCE = $2A01; 
  {$EXTERNALSYM BTH_LE_GATT_CHARACTERISTIC_TYPE_APPEARANCE}
  BTH_LE_GATT_CHARACTERISTIC_TYPE_PERIPHERAL_PRIVACY_FLAG = $2A02; 
  {$EXTERNALSYM BTH_LE_GATT_CHARACTERISTIC_TYPE_PERIPHERAL_PRIVACY_FLAG}
  BTH_LE_GATT_CHARACTERISTIC_TYPE_RECONNECTION_ADDRESS = $2A03; 
  {$EXTERNALSYM BTH_LE_GATT_CHARACTERISTIC_TYPE_RECONNECTION_ADDRESS}
  BTH_LE_GATT_CHARACTERISTIC_TYPE_PERIPHERAL_PREFERED_CONNECTION_PARAMETER = $2A04; 
  {$EXTERNALSYM BTH_LE_GATT_CHARACTERISTIC_TYPE_PERIPHERAL_PREFERED_CONNECTION_PARAMETER}
  BTH_LE_GATT_CHARACTERISTIC_TYPE_SERVICE_CHANGED = $2A05; 
  {$EXTERNALSYM BTH_LE_GATT_CHARACTERISTIC_TYPE_SERVICE_CHANGED}


  BTH_LE_GAP_APPEARANCE_CATEGORY_OFFSET = $6;
  {$EXTERNALSYM BTH_LE_GAP_APPEARANCE_CATEGORY_OFFSET}
  BTH_LE_GAP_APPEARANCE_CATEGORY_MASK = $3ff;
  {$EXTERNALSYM BTH_LE_GAP_APPEARANCE_CATEGORY_MASK}


function BTH_LE_GAP_APPEARANCE_GET_CATEGORY(a: Cardinal): Cardinal; inline;
{$EXTERNALSYM BTH_LE_GAP_APPEARANCE_GET_CATEGORY}

const
  BTH_LE_GAP_APPEARANCE_SUB_CATEGORY_MASK = $3f;
  {$EXTERNALSYM BTH_LE_GAP_APPEARANCE_SUB_CATEGORY_MASK}

function BTH_LE_GAP_APPEARANCE_GET_SUB_CATEGORY(a: Cardinal): Byte; inline;
{$EXTERNALSYM BTH_LE_GAP_APPEARANCE_GET_SUB_CATEGORY}

const
  BTH_LE_GAP_APPEARANCE_CATEGORY_PHONE = $0001; 
  {$EXTERNALSYM BTH_LE_GAP_APPEARANCE_CATEGORY_PHONE}
  BTH_LE_GAP_APPEARANCE_CATEGORY_COMPUTER = $0002; 
  {$EXTERNALSYM BTH_LE_GAP_APPEARANCE_CATEGORY_COMPUTER}
  BTH_LE_GAP_APPEARANCE_CATEGORY_WATCH = $0003; 
  {$EXTERNALSYM BTH_LE_GAP_APPEARANCE_CATEGORY_WATCH}
  BTH_LE_GAP_APPEARANCE_CATEGORY_CLOCK = $0004; 
  {$EXTERNALSYM BTH_LE_GAP_APPEARANCE_CATEGORY_CLOCK}
  BTH_LE_GAP_APPEARANCE_CATEGORY_DISPLAY = $0005; 
  {$EXTERNALSYM BTH_LE_GAP_APPEARANCE_CATEGORY_DISPLAY}
  BTH_LE_GAP_APPEARANCE_CATEGORY_REMOTE_CONTROL = $0006; 
  {$EXTERNALSYM BTH_LE_GAP_APPEARANCE_CATEGORY_REMOTE_CONTROL}
  BTH_LE_GAP_APPEARANCE_CATEGORY_EYE_GLASSES = $0007; 
  {$EXTERNALSYM BTH_LE_GAP_APPEARANCE_CATEGORY_EYE_GLASSES}
  BTH_LE_GAP_APPEARANCE_CATEGORY_TAG = $0008; 
  {$EXTERNALSYM BTH_LE_GAP_APPEARANCE_CATEGORY_TAG}
  BTH_LE_GAP_APPEARANCE_CATEGORY_KEYRING = $0009; 
  {$EXTERNALSYM BTH_LE_GAP_APPEARANCE_CATEGORY_KEYRING}
  BTH_LE_GAP_APPEARANCE_CATEGORY_MEDIA_PLAYER = $000a; 
  {$EXTERNALSYM BTH_LE_GAP_APPEARANCE_CATEGORY_MEDIA_PLAYER}
  BTH_LE_GAP_APPEARANCE_CATEGORY_BARCODE_SCANNER = $000b; 
  {$EXTERNALSYM BTH_LE_GAP_APPEARANCE_CATEGORY_BARCODE_SCANNER}
  BTH_LE_GAP_APPEARANCE_CATEGORY_THERMOMETER = $000c; 
  {$EXTERNALSYM BTH_LE_GAP_APPEARANCE_CATEGORY_THERMOMETER}
  BTH_LE_GAP_APPEARANCE_CATEGORY_HEART_RATE = $000d; 
  {$EXTERNALSYM BTH_LE_GAP_APPEARANCE_CATEGORY_HEART_RATE}
  BTH_LE_GAP_APPEARANCE_CATEGORY_BLOOD_PRESSURE = $000e; 
  {$EXTERNALSYM BTH_LE_GAP_APPEARANCE_CATEGORY_BLOOD_PRESSURE}
  BTH_LE_GAP_APPEARANCE_CATEGORY_HID = $000f; 
  {$EXTERNALSYM BTH_LE_GAP_APPEARANCE_CATEGORY_HID}
  BTH_LE_GAP_APPEARANCE_CATEGORY_GLUCOSE_METER = $0010; 
  {$EXTERNALSYM BTH_LE_GAP_APPEARANCE_CATEGORY_GLUCOSE_METER}
  BTH_LE_GAP_APPEARANCE_CATEGORY_RUNNING_WALKING_SENSOR = $0011; 
  {$EXTERNALSYM BTH_LE_GAP_APPEARANCE_CATEGORY_RUNNING_WALKING_SENSOR}
  BTH_LE_GAP_APPEARANCE_CATEGORY_CYCLING = $0012; 
  {$EXTERNALSYM BTH_LE_GAP_APPEARANCE_CATEGORY_CYCLING}

  BTH_LE_GAP_APPEARANCE_HID_SUBCATEGORY_KEYBOARD = $01; 
  {$EXTERNALSYM BTH_LE_GAP_APPEARANCE_HID_SUBCATEGORY_KEYBOARD}
  BTH_LE_GAP_APPEARANCE_HID_SUBCATEGORY_MOUSE = $02; 
  {$EXTERNALSYM BTH_LE_GAP_APPEARANCE_HID_SUBCATEGORY_MOUSE}
  BTH_LE_GAP_APPEARANCE_HID_SUBCATEGORY_JOYSTICK = $03; 
  {$EXTERNALSYM BTH_LE_GAP_APPEARANCE_HID_SUBCATEGORY_JOYSTICK}
  BTH_LE_GAP_APPEARANCE_HID_SUBCATEGORY_GAMEPAD = $04; 
  {$EXTERNALSYM BTH_LE_GAP_APPEARANCE_HID_SUBCATEGORY_GAMEPAD}
  BTH_LE_GAP_APPEARANCE_HID_SUBCATEGORY_DIGITIZER_TABLET = $05; 
  {$EXTERNALSYM BTH_LE_GAP_APPEARANCE_HID_SUBCATEGORY_DIGITIZER_TABLET}
  BTH_LE_GAP_APPEARANCE_HID_SUBCATEGORY_CARD_READER = $06; 
  {$EXTERNALSYM BTH_LE_GAP_APPEARANCE_HID_SUBCATEGORY_CARD_READER}
  BTH_LE_GAP_APPEARANCE_HID_SUBCATEGORY_DIGITAL_PEN = $07; 
  {$EXTERNALSYM BTH_LE_GAP_APPEARANCE_HID_SUBCATEGORY_DIGITAL_PEN}
  BTH_LE_GAP_APPEARANCE_HID_SUBCATEGORY_BARCODE_SCANNER = $08; 
  {$EXTERNALSYM BTH_LE_GAP_APPEARANCE_HID_SUBCATEGORY_BARCODE_SCANNER}


{ GATT Included Services Default Maximum Nested Depth }

  BTH_LE_GATT_DEFAULT_MAX_INCLUDED_SERVICES_DEPTH = 3;
  {$EXTERNALSYM BTH_LE_GATT_DEFAULT_MAX_INCLUDED_SERVICES_DEPTH}

{ ////////////////////////////////////////////////////////////////////////////// }
{ ATT Constants }
{ ////////////////////////////////////////////////////////////////////////////// }


{ Transation timeout }

  BTH_LE_ATT_TRANSACTION_TIMEOUT = 30; { seconds }
  {$EXTERNALSYM BTH_LE_ATT_TRANSACTION_TIMEOUT}


{ Maximum size of any attribute value }

  BTH_LE_ATT_MAX_VALUE_SIZE = 512; 
  {$EXTERNALSYM BTH_LE_ATT_MAX_VALUE_SIZE}


{ CID }

  BTH_LE_ATT_CID = $0004; 
  {$EXTERNALSYM BTH_LE_ATT_CID}


{ MTU }

  BTHLEENUM_ATT_MTU_MIN = 23; 
  {$EXTERNALSYM BTHLEENUM_ATT_MTU_MIN}
  BTHLEENUM_ATT_MTU_MAX = $FFFF;
  {$EXTERNALSYM BTHLEENUM_ATT_MTU_MAX}
  BTHLEENUM_ATT_MTU_DEFAULT = BTHLEENUM_ATT_MTU_MIN; 
  {$EXTERNALSYM BTHLEENUM_ATT_MTU_DEFAULT}
  BTHLEENUM_ATT_MTU_INITIAL_NEGOTIATION = 525; 
  {$EXTERNALSYM BTHLEENUM_ATT_MTU_INITIAL_NEGOTIATION}

{ ////////////////////////////////////////////////////////////////////////////// }
{ ATT-specific Error Codes }
{ ////////////////////////////////////////////////////////////////////////////// }

  BTH_LE_ERROR_INVALID_HANDLE = $01; 
  {$EXTERNALSYM BTH_LE_ERROR_INVALID_HANDLE}
  BTH_LE_ERROR_READ_NOT_PERMITTED = $02; 
  {$EXTERNALSYM BTH_LE_ERROR_READ_NOT_PERMITTED}
  BTH_LE_ERROR_WRITE_NOT_PERMITTED = $03; 
  {$EXTERNALSYM BTH_LE_ERROR_WRITE_NOT_PERMITTED}
  BTH_LE_ERROR_INVALID_PDU = $04; 
  {$EXTERNALSYM BTH_LE_ERROR_INVALID_PDU}
  BTH_LE_ERROR_INSUFFICIENT_AUTHENTICATION = $05; 
  {$EXTERNALSYM BTH_LE_ERROR_INSUFFICIENT_AUTHENTICATION}
  BTH_LE_ERROR_REQUEST_NOT_SUPPORTED = $06; 
  {$EXTERNALSYM BTH_LE_ERROR_REQUEST_NOT_SUPPORTED}
  BTH_LE_ERROR_INVALID_OFFSET = $07; 
  {$EXTERNALSYM BTH_LE_ERROR_INVALID_OFFSET}
  BTH_LE_ERROR_INSUFFICIENT_AUTHORIZATION = $08; 
  {$EXTERNALSYM BTH_LE_ERROR_INSUFFICIENT_AUTHORIZATION}
  BTH_LE_ERROR_PREPARE_QUEUE_FULL = $09; 
  {$EXTERNALSYM BTH_LE_ERROR_PREPARE_QUEUE_FULL}
  BTH_LE_ERROR_ATTRIBUTE_NOT_FOUND = $0A; 
  {$EXTERNALSYM BTH_LE_ERROR_ATTRIBUTE_NOT_FOUND}
  BTH_LE_ERROR_ATTRIBUTE_NOT_LONG = $0B; 
  {$EXTERNALSYM BTH_LE_ERROR_ATTRIBUTE_NOT_LONG}
  BTH_LE_ERROR_INSUFFICIENT_ENCRYPTION_KEY_SIZE = $0C; 
  {$EXTERNALSYM BTH_LE_ERROR_INSUFFICIENT_ENCRYPTION_KEY_SIZE}
  BTH_LE_ERROR_INVALID_ATTRIBUTE_VALUE_LENGTH = $0D; 
  {$EXTERNALSYM BTH_LE_ERROR_INVALID_ATTRIBUTE_VALUE_LENGTH}
  BTH_LE_ERROR_UNLIKELY = $0E; 
  {$EXTERNALSYM BTH_LE_ERROR_UNLIKELY}
  BTH_LE_ERROR_INSUFFICIENT_ENCRYPTION = $0F; 
  {$EXTERNALSYM BTH_LE_ERROR_INSUFFICIENT_ENCRYPTION}
  BTH_LE_ERROR_UNSUPPORTED_GROUP_TYPE = $10; 
  {$EXTERNALSYM BTH_LE_ERROR_UNSUPPORTED_GROUP_TYPE}
  BTH_LE_ERROR_INSUFFICIENT_RESOURCES = $11; 
  {$EXTERNALSYM BTH_LE_ERROR_INSUFFICIENT_RESOURCES}
  BTH_LE_ERROR_UNKNOWN = $1000; 
  {$EXTERNALSYM BTH_LE_ERROR_UNKNOWN}

{ ////////////////////////////////////////////////////////////////////////////// }
{ GATT Security and Other Flag-related Facilities }
{ ////////////////////////////////////////////////////////////////////////////// }

  BLUETOOTH_GATT_FLAG_NONE = $00000000; 
  {$EXTERNALSYM BLUETOOTH_GATT_FLAG_NONE}
  BLUETOOTH_GATT_FLAG_CONNECTION_ENCRYPTED = $00000001;
  {$EXTERNALSYM BLUETOOTH_GATT_FLAG_CONNECTION_ENCRYPTED}
  BLUETOOTH_GATT_FLAG_CONNECTION_AUTHENTICATED = $00000002; 
  {$EXTERNALSYM BLUETOOTH_GATT_FLAG_CONNECTION_AUTHENTICATED}
  BLUETOOTH_GATT_FLAG_FORCE_READ_FROM_DEVICE = $00000004; 
  {$EXTERNALSYM BLUETOOTH_GATT_FLAG_FORCE_READ_FROM_DEVICE}
  BLUETOOTH_GATT_FLAG_FORCE_READ_FROM_CACHE = $00000008; 
  {$EXTERNALSYM BLUETOOTH_GATT_FLAG_FORCE_READ_FROM_CACHE}
  BLUETOOTH_GATT_FLAG_SIGNED_WRITE = $00000010; 
  {$EXTERNALSYM BLUETOOTH_GATT_FLAG_SIGNED_WRITE}
  BLUETOOTH_GATT_FLAG_WRITE_WITHOUT_RESPONSE = $00000020; 
  {$EXTERNALSYM BLUETOOTH_GATT_FLAG_WRITE_WITHOUT_RESPONSE}
  BLUETOOTH_GATT_FLAG_RETURN_ALL = $00000040;
  {$EXTERNALSYM BLUETOOTH_GATT_FLAG_RETURN_ALL}

  BLUETOOTH_GATT_FLAG_VALID_MASK = BLUETOOTH_GATT_FLAG_NONE
 or            BLUETOOTH_GATT_FLAG_CONNECTION_ENCRYPTED
 or            BLUETOOTH_GATT_FLAG_CONNECTION_AUTHENTICATED
 or            BLUETOOTH_GATT_FLAG_FORCE_READ_FROM_DEVICE
 or            BLUETOOTH_GATT_FLAG_FORCE_READ_FROM_CACHE
 or            BLUETOOTH_GATT_FLAG_SIGNED_WRITE
 or            BLUETOOTH_GATT_FLAG_WRITE_WITHOUT_RESPONSE
 or            BLUETOOTH_GATT_FLAG_RETURN_ALL;
  {$EXTERNALSYM BLUETOOTH_GATT_FLAG_VALID_MASK}

function IS_BLUETOOTH_GATT_FLAG_VALID(f: Cardinal): Boolean; inline;
{$EXTERNALSYM IS_BLUETOOTH_GATT_FLAG_VALID}

{ ////////////////////////////////////////////////////////////////////////////// }
{ GATT Structures }
{ ////////////////////////////////////////////////////////////////////////////// }

type
  BLUETOOTH_GATT_EVENT_HANDLE = THandle;
  {$EXTERNALSYM BLUETOOTH_GATT_EVENT_HANDLE}
  TBluetoothGattEventHandle = BLUETOOTH_GATT_EVENT_HANDLE;
  PBluetoothGattEventHandle = ^TBluetoothGattEventHandle;

  _BTH_LE_UUID = record
    IsShortUuid: Boolean;
    case Integer of
      0: (LongUuid: TGUID);
      1: (ShortUuid: USHORT);
      2: (ForceAlign4: Int32);
  end;
  {$EXTERNALSYM _BTH_LE_UUID}
  BTH_LE_UUID = _BTH_LE_UUID;
  {$EXTERNALSYM BTH_LE_UUID}
  PBTH_LE_UUID = ^BTH_LE_UUID;
  {$EXTERNALSYM PBTH_LE_UUID}
  TBthLeUuid = BTH_LE_UUID;
  PBthLeUuid = ^TBthLeUuid;

  _BTH_LE_GATT_SERVICE = record
    ServiceUuid: TBthLeUuid;
    AttributeHandle: USHORT;
  end;
  {$EXTERNALSYM _BTH_LE_GATT_SERVICE}
  BTH_LE_GATT_SERVICE = _BTH_LE_GATT_SERVICE;
  {$EXTERNALSYM BTH_LE_GATT_SERVICE}
  PBTH_LE_GATT_SERVICE = ^BTH_LE_GATT_SERVICE;
  {$EXTERNALSYM PBTH_LE_GATT_SERVICE}
  TBthLeGattService = BTH_LE_GATT_SERVICE;
  PBthLeGattService = ^TBthLEGattService;

type
  BTH_LE_GATT_DESCRIPTOR_TYPE = (
    CharacteristicExtendedProperties  = 0,
    CharacteristicUserDescription     = 1,
    ClientCharacteristicConfiguration = 2,
    ServerCharacteristicConfiguration = 3,
    CharacteristicFormat              = 4,
    CharacteristicAggregateFormat     = 5,
    CustomDescriptor                  = 6
  );
  {$EXTERNALSYM BTH_LE_GATT_DESCRIPTOR_TYPE}
  PBTH_LE_GATT_DESCRIPTOR_TYPE = ^BTH_LE_GATT_DESCRIPTOR_TYPE;
  {$EXTERNALSYM PBTH_LE_GATT_DESCRIPTOR_TYPE}
  TBthLeGattDescriptorType = BTH_LE_GATT_DESCRIPTOR_TYPE;
  PBthLeGattDescriptorType = ^TBthLeGattDescriptorType;

  _BTH_LE_GATT_CHARACTERISTIC = record
    ServiceHandle: USHORT;
    CharacteristicUuid: TBthLeUuid;
    AttributeHandle: USHORT;
    CharacteristicValueHandle: USHORT;
    IsBroadcastable: BOOLEAN;
    IsReadable: BOOLEAN;
    IsWritable: BOOLEAN;
    IsWritableWithoutResponse: BOOLEAN;
    IsSignedWritable: BOOLEAN;
    IsNotifiable: BOOLEAN;
    IsIndicatable: BOOLEAN;
    HasExtendedProperties: BOOLEAN;
  end;
  {$EXTERNALSYM _BTH_LE_GATT_CHARACTERISTIC}
  BTH_LE_GATT_CHARACTERISTIC = _BTH_LE_GATT_CHARACTERISTIC;
  {$EXTERNALSYM BTH_LE_GATT_CHARACTERISTIC}
  PBTH_LE_GATT_CHARACTERISTIC = ^BTH_LE_GATT_CHARACTERISTIC;
  {$EXTERNALSYM PBTH_LE_GATT_CHARACTERISTIC}
  TBthLeGattCharacteristic = BTH_LE_GATT_CHARACTERISTIC;
  PBthLeGattCharacteristic = ^TBthLeGattCharacteristic;

  _BTH_LE_GATT_CHARACTERISTIC_VALUE = record
    DataSize: ULONG;
    Data: PByte;
    //Data: array [0..0] of byte;
  end;
  {$EXTERNALSYM _BTH_LE_GATT_CHARACTERISTIC_VALUE}
  BTH_LE_GATT_CHARACTERISTIC_VALUE = _BTH_LE_GATT_CHARACTERISTIC_VALUE;
  {$EXTERNALSYM BTH_LE_GATT_CHARACTERISTIC_VALUE}
  PBTH_LE_GATT_CHARACTERISTIC_VALUE = ^BTH_LE_GATT_CHARACTERISTIC_VALUE;
  {$EXTERNALSYM PBTH_LE_GATT_CHARACTERISTIC_VALUE}
  TBthLeGattCharacteristicValue = BTH_LE_GATT_CHARACTERISTIC_VALUE;
  PBthLeGattCharacteristicValue = ^TBthLeGattCharacteristicValue;


  BTH_LE_GATT_DESCRIPTOR = record
    ServiceHandle: USHORT;
    CharacteristicHandle: USHORT;
    DescriptorType: TBthLeGattDescriptorType;
    DescriptorUuid: TBthLeUuid;
    AttributeHandle: USHORT;
  end;
  _BTH_LE_GATT_DESCRIPTOR = BTH_LE_GATT_DESCRIPTOR;
  {$EXTERNALSYM BTH_LE_GATT_DESCRIPTOR}
  PBTH_LE_GATT_DESCRIPTOR = ^BTH_LE_GATT_DESCRIPTOR;
  {$EXTERNALSYM PBTH_LE_GATT_DESCRIPTOR}
  TBthLeGattDescriptor = BTH_LE_GATT_DESCRIPTOR;
  PBthLeGattDescriptor = ^TBthLeGattDescriptor;


  AuxGattDescriptorType = record
    case BTH_LE_GATT_DESCRIPTOR_TYPE of
      CharacteristicExtendedProperties: (
        IsReliableWriteEnabled: BOOLEAN;
        IsAuxiliariesWritable: BOOLEAN;
      );

      ClientCharacteristicConfiguration: (
        IsSubscribeToNotification: BOOLEAN;
        IsSubscribeToIndication: BOOLEAN;
      );

      ServerCharacteristicConfiguration: (IsBroadcast: BOOLEAN);

      CharacteristicFormat: (
        Format: UCHAR;
        Exponent: ShortInt;
        &Unit: TBthLeUuid;
        NameSpace: UCHAR;
        Description: TBthLeUuid;
      );
  end;

  _BTH_LE_GATT_DESCRIPTOR_VALUE = record
    DescriptorType: TBthLeGattDescriptorType;
    DescriptorUuid: TBthLeUuid;
    DescriptorInfo: AuxGattDescriptorType;
    DataSize: ULONG;
    Data: PByte;
  end;
  BTH_LE_GATT_DESCRIPTOR_VALUE = _BTH_LE_GATT_DESCRIPTOR_VALUE;
  {$EXTERNALSYM BTH_LE_GATT_DESCRIPTOR_VALUE}
  PBTH_LE_GATT_DESCRIPTOR_VALUE = ^BTH_LE_GATT_DESCRIPTOR_VALUE;
  {$EXTERNALSYM PBTH_LE_GATT_DESCRIPTOR_VALUE}
  TBthLeGattDescriptorValue = BTH_LE_GATT_DESCRIPTOR_VALUE;
  PBthLeGattDescriptorValue = ^TBthLeGattDescriptorValue;

  BTH_LE_GATT_EVENT_TYPE = (
    CharacteristicValueChangedEvent = 0
  );
  {$EXTERNALSYM BTH_LE_GATT_EVENT_TYPE}
  TBthLeGattEventType = BTH_LE_GATT_EVENT_TYPE;


{ Callback function signature for Bluetooth GATT events. }

type
  PFNBLUETOOTH_GATT_EVENT_CALLBACK = procedure(
    EventType: TBthLeGattEventType; EventOutParameter: Pointer;
    Context: Pointer); cdecl;
{$EXTERNALSYM PFNBLUETOOTH_GATT_EVENT_CALLBACK}
  PFnBluetoothGattEventCallback = PFNBLUETOOTH_GATT_EVENT_CALLBACK;

{ Structure associated with a CharacteristicValueChanged Event }

  PBluetoothGattValueChangedEventRegistration = ^TBluetoothGattValueChangedEventRegistration;
  BLUETOOTH_GATT_VALUE_CHANGED_EVENT_REGISTRATION = record
    NumCharacteristics: USHORT;
    Characteristics: array [0..0] of TBthLeGattCharacteristic;
  end;
  _BLUETOOTH_GATT_VALUE_CHANGED_EVENT_REGISTRATION = BLUETOOTH_GATT_VALUE_CHANGED_EVENT_REGISTRATION;
  {$EXTERNALSYM BLUETOOTH_GATT_VALUE_CHANGED_EVENT_REGISTRATION}
  PBLUETOOTH_GATT_VALUE_CHANGED_EVENT_REGISTRATION = ^BLUETOOTH_GATT_VALUE_CHANGED_EVENT_REGISTRATION;
  {$EXTERNALSYM PBLUETOOTH_GATT_VALUE_CHANGED_EVENT_REGISTRATION}
  TBluetoothGattValueChangedEventRegistration = BLUETOOTH_GATT_VALUE_CHANGED_EVENT_REGISTRATION;

  BLUETOOTH_GATT_VALUE_CHANGED_EVENT = record
    ChangedAttributeHandle: USHORT;
    CharacteristicValueDataSize: size_t;
    CharacteristicValue: PBthLeGattCharacteristicValue;
  end;
  _BLUETOOTH_GATT_VALUE_CHANGED_EVENT = BLUETOOTH_GATT_VALUE_CHANGED_EVENT;
  {$EXTERNALSYM BLUETOOTH_GATT_VALUE_CHANGED_EVENT}
  PBLUETOOTH_GATT_VALUE_CHANGED_EVENT = ^BLUETOOTH_GATT_VALUE_CHANGED_EVENT;
  {$EXTERNALSYM PBLUETOOTH_GATT_VALUE_CHANGED_EVENT}
  TBluetoothGattValueChangedEvent = BLUETOOTH_GATT_VALUE_CHANGED_EVENT;
  PBluetoothGattValueChangedEvent = ^TBluetoothGattValueChangedEvent;

  BTH_LE_GATT_RELIABLE_WRITE_CONTEXT = ULONG64;
  {$EXTERNALSYM PBTH_LE_GATT_RELIABLE_WRITE_CONTEXT}
  PBTH_LE_GATT_RELIABLE_WRITE_CONTEXT = ^BTH_LE_GATT_RELIABLE_WRITE_CONTEXT;
  {$EXTERNALSYM BTH_LE_GATT_RELIABLE_WRITE_CONTEXT}
  TBthLeGattReliableWriteContext = BTH_LE_GATT_RELIABLE_WRITE_CONTEXT;
  PBthLeGattReliableWriteContext = ^TBthLeGattReliableWriteContext;


function IsBthLEUuidMatch(uuid1: TBthLeUuid; uuid2: TBthLeUuid): Boolean; inline;
{$EXTERNALSYM IsBthLEUuidMatch}
///////////////////////////////
///
///  End bthledef.h
///
///////////////////////////////


///////////////////////////////
///
///  Start BluetoothLEAPIs.h
///
///////////////////////////////

function BluetoothGATTGetServices(hDevice: THandle; ServicesBufferCount : Word;
  ServicesBuffer: PBthLeGattService; var ServicesBufferActual: Word; Flags: Cardinal): HRESULT; stdcall;
{$EXTERNALSYM BluetoothGATTGetServices}

function BluetoothGATTGetIncludedServices(hDevice: THandle; ParentService: PBthLeGattService;
  IncludedServicesBufferCount: Word; IncludedServicesBuffer: PBthLeGattService;
  var IncludedServicesBufferActual: Word; Flags: Cardinal): HRESULT; stdcall;
{$EXTERNALSYM BluetoothGATTGetIncludedServices}

function BluetoothGATTGetCharacteristics(hDevice: THandle; Service: PBthLeGattService;
  CharacteristicsBufferCount : Word; CharacteristicsBuffer: PBthLeGattCharacteristic;
  var CharacteristicsBufferActual : Word; Flags   : Cardinal): HRESULT; stdcall;
{$EXTERNALSYM BluetoothGATTGetCharacteristics}

function BluetoothGATTGetDescriptors(hDevice: THandle; var Characteristic: TBthLeGattCharacteristic;
  DescriptorsBufferCount: Word; DescriptorsBuffer: PBthLeGattDescriptor;
  var DescriptorsBufferActual: Word; Flags: Cardinal): HRESULT; stdcall;
{$EXTERNALSYM BluetoothGATTGetDescriptors}

function BluetoothGATTGetCharacteristicValue(hDevice: THandle; var Characteristic: TBthLeGattCharacteristic;
  CharacteristicValueDataSize: Cardinal; CharacteristicValue: PBthLeGattCharacteristicValue;
  CharacteristicValueSizeRequired: PWord; Flags: ULONG): HRESULT; stdcall;
{$EXTERNALSYM BluetoothGATTGetCharacteristicValue}

function BluetoothGATTGetDescriptorValue(hDevice: THandle; var Descriptor: TBthLeGattDescriptor;
  DescriptorValueDataSize: Cardinal; DescriptorValue: PBthLeGattDescriptorValue;
  DescriptorValueSizeRequired: PWord; Flags: Cardinal): HRESULT; stdcall;
{$EXTERNALSYM BluetoothGATTGetDescriptorValue}

function BluetoothGATTBeginReliableWrite(hDevice: THandle;
  var ReliableWriteContext: TBthLeGattReliableWriteContext; Flags: Cardinal): HRESULT; stdcall;
{$EXTERNALSYM BluetoothGATTBeginReliableWrite}

function BluetoothGATTEndReliableWrite(hDevice: THandle; ReliableWriteContext: TBthLeGattReliableWriteContext;
  Flags: Cardinal): HRESULT; stdcall;
{$EXTERNALSYM BluetoothGATTEndReliableWrite}

function BluetoothGATTAbortReliableWrite(hDevice: THandle; ReliableWriteContext: TBthLeGattReliableWriteContext;
  Flags: Cardinal): HRESULT; stdcall;
{$EXTERNALSYM BluetoothGATTAbortReliableWrite}

function BluetoothGATTSetCharacteristicValue(hDevice: THandle; var Characteristic: TBthLeGattCharacteristic;
  CharacteristicValue: PBthLeGattCharacteristicValue; ReliableWriteContext: TBthLeGattReliableWriteContext;
  Flags: Cardinal): HRESULT; stdcall;
{$EXTERNALSYM BluetoothGATTSetCharacteristicValue}

function BluetoothGATTSetDescriptorValue(hDevice: THandle; var Descriptor: TBthLeGattDescriptor;
  DescriptorValue: PBthLeGattDescriptorValue; Flags: Cardinal): HRESULT; stdcall;
{$EXTERNALSYM BluetoothGATTSetDescriptorValue}

function BluetoothGATTRegisterEvent(hService: THandle; EventType: TBthLeGattEventType; EventParameterIn: Pointer;
  Callback: PFnBluetoothGattEventCallback; CallbackContext: Pointer;
  var EventHandle: TBluetoothGattEventHandle; Flags: Cardinal): HRESULT; stdcall;
{$EXTERNALSYM BluetoothGATTRegisterEvent}

function BluetoothGATTUnregisterEvent(EventHandle: TBluetoothGattEventHandle; Flags: Cardinal): HRESULT; stdcall;
{$EXTERNALSYM BluetoothGATTUnregisterEvent}

///////////////////////////////
///
///  End BluetoothLEAPIs.h
///
///////////////////////////////

implementation

uses
  System.SysUtils;

function BTH_LE_GAP_APPEARANCE_GET_CATEGORY(a: Cardinal): Cardinal;
begin
  Result := (a shr BTH_LE_GAP_APPEARANCE_CATEGORY_OFFSET) and BTH_LE_GAP_APPEARANCE_CATEGORY_MASK;
end;

function BTH_LE_GAP_APPEARANCE_GET_SUB_CATEGORY(a: Cardinal): Byte;
begin
  Result := a and BTH_LE_GAP_APPEARANCE_SUB_CATEGORY_MASK;
end;

function IS_BLUETOOTH_GATT_FLAG_VALID(f: Cardinal): Boolean;
begin
  Result := (f and not BLUETOOTH_GATT_FLAG_VALID_MASK) = 0;
end;

function IsBthLEUuidMatch(uuid1: TBthLeUuid; uuid2: TBthLeUuid): Boolean;
var
  tempLongUuid: TBthLeUuid;
begin
  Result := False;
  tempLongUuid := Default(TBthLeUuid);
  tempLongUuid.IsShortUuid := False;
  tempLongUuid.LongUuid := BTH_LE_ATT_BLUETOOTH_BASE_GUID;

  if uuid1.IsShortUuid and uuid2.IsShortUuid then
    Result := uuid1.ShortUuid = uuid2.ShortUuid
  else  if not uuid1.IsShortUuid and not uuid2.IsShortUuid then
    Result := uuid1.LongUuid = uuid2.LongUuid
  else if uuid1.IsShortUuid then
  begin
    Inc(tempLongUuid.LongUuid.D1, uuid1.ShortUuid);
    Result := tempLongUuid.LongUuid = uuid2.LongUuid;
  end
  else if uuid2.IsShortUuid then
  begin
    Inc(tempLongUuid.LongUuid.D1, uuid2.ShortUuid);
    Result := tempLongUuid.LongUuid = uuid1.LongUuid;
  end;
end;

const
// TODO -oJulianM: remember Lee Cantey to add BluetoothApis to import32/64
  bthapile = 'BluetoothApis.dll';

{$WARN SYMBOL_PLATFORM OFF}
function BluetoothGATTAbortReliableWrite; external bthapile name 'BluetoothGATTAbortReliableWrite' delayed;
function BluetoothGATTBeginReliableWrite; external bthapile name 'BluetoothGATTBeginReliableWrite' delayed;
function BluetoothGATTEndReliableWrite; external bthapile name 'BluetoothGATTEndReliableWrite' delayed;
function BluetoothGATTGetCharacteristics; external bthapile name 'BluetoothGATTGetCharacteristics' delayed;
function BluetoothGATTGetCharacteristicValue; external bthapile name 'BluetoothGATTGetCharacteristicValue' delayed;
function BluetoothGATTGetDescriptors; external bthapile name 'BluetoothGATTGetDescriptors' delayed;
function BluetoothGATTGetDescriptorValue; external bthapile name 'BluetoothGATTGetDescriptorValue' delayed;
function BluetoothGATTGetIncludedServices; external bthapile name 'BluetoothGATTGetIncludedServices' delayed;
function BluetoothGATTGetServices; external bthapile name 'BluetoothGATTGetServices' delayed;
function BluetoothGATTRegisterEvent; external bthapile name 'BluetoothGATTRegisterEvent' delayed;
function BluetoothGATTSetCharacteristicValue; external bthapile name 'BluetoothGATTSetCharacteristicValue' delayed;
function BluetoothGATTSetDescriptorValue; external bthapile name 'BluetoothGATTSetDescriptorValue' delayed;
function BluetoothGATTUnregisterEvent; external bthapile name 'BluetoothGATTUnregisterEvent' delayed;

end.
