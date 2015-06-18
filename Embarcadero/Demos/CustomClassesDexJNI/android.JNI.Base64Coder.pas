// This software is Copyright (c) 2014 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of one of Embarcadero's developer tools products.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

// Based on http://www.source-code.biz/base64coder/java/

unit android.JNI.Base64Coder;

interface
{$IFDEF Android}
uses
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes;

type
  {Class forward declarations}
  JBase64Coder = interface;//biz.source_code.base64Coder.Base64Coder

JBase64CoderClass = interface(JObjectClass)
['{5262A792-3556-472E-A1D9-B93CF466D3C9}']
  {Methods}
  function encodeString(P1: JString): JString; cdecl;
  function encodeLines(P1: TJavaArray<Byte>): JString; cdecl; overload;
  function encodeLines(P1: TJavaArray<Byte>; P2: Integer; P3: Integer; P4: Integer; P5: JString): JString; cdecl; overload;
  function encode(P1: TJavaArray<Byte>): TJavaArray<Char>; cdecl; overload;
  function encode(P1: TJavaArray<Byte>; P2: Integer): TJavaArray<Char>; cdecl; overload;
  function encode(P1: TJavaArray<Byte>; P2: Integer; P3: Integer): TJavaArray<Char>; cdecl; overload;
  function decodeString(P1: JString): JString; cdecl;
  function decodeLines(P1: JString): TJavaArray<Byte>; cdecl;
  function decode(P1: JString): TJavaArray<Byte>; cdecl; overload;
  function decode(P1: TJavaArray<Char>): TJavaArray<Byte>; cdecl; overload;
  function decode(P1: TJavaArray<Char>; P2: Integer; P3: Integer): TJavaArray<Byte>; cdecl; overload;
end;

[JavaSignature('biz/source_code/base64Coder/Base64Coder')]
JBase64Coder = interface(JObject)
['{24B074E9-71AF-466D-8192-45E2791A62D1}']
end;
TJBase64Coder = class(TJavaGenericImport<JBase64CoderClass, JBase64Coder>) end;

{$ENDIF}

implementation

{$IFDEF Android}

procedure RegisterTypes;
begin
  TRegTypes.RegisterType('android.JNI.Base64Coder.JBase64Coder', TypeInfo(android.JNI.Base64Coder.JBase64Coder));
end;

{$ENDIF}

initialization

{$IFDEF Android}
  RegisterTypes;
{$ENDIF} 
 
end.


