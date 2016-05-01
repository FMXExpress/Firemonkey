//---------------------------------------------------------------------------
// Copyright (c) 2016 Embarcadero Technologies, Inc. All rights reserved.  
//
// This software is the copyrighted property of Embarcadero Technologies, Inc. 
// ("Embarcadero") and its licensors. You may only use this software if you 
// are an authorized licensee of Delphi, C++Builder or RAD Studio 
// (the "Embarcadero Products").  This software is subject to Embarcadero's 
// standard software license and support agreement that accompanied your 
// purchase of the Embarcadero Products and is considered a Redistributable, 
// as such term is defined thereunder. Your use of this software constitutes 
// your acknowledgement of your agreement to the foregoing software license 
// and support agreement. 
//---------------------------------------------------------------------------
unit IntentServiceUnit;

interface

uses
  AndroidApi.JNI.GraphicsContentViewText;

type
  TIntentServiceHelper = record
  private
    FIntent: JIntent;
    FCode: Integer;
    FData: string;
  public
    class function Create(const Intent: JIntent): TIntentServiceHelper; overload; static;
    class function Create(const AServiceName: string; Code: Integer; Data: string): TIntentServiceHelper; overload; static;
    property Code: Integer read FCode;
    property Data: string read FData;
    property Intent: JIntent read FIntent;
  end;

implementation

uses
  System.SysUtils,
  Androidapi.Helpers,
  Androidapi.Jni,
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes;

{ TIntentServiceHelper }

class function TIntentServiceHelper.Create(const Intent: JIntent): TIntentServiceHelper;
begin
  Result.FCode := Intent.getIntExtra(TAndroidHelper.StringToJString('Code'), -1);
  Result.FData := TAndroidHelper.JStringToString(Intent.getStringExtra(TAndroidHelper.StringToJString('Data')));
end;

class function TIntentServiceHelper.Create(const AServiceName: string; Code: Integer; Data: string): TIntentServiceHelper;
var
  LService: string;
begin
  Result.FIntent := TJIntent.Create;
  LService := AServiceName;
  if not LService.StartsWith('com.embarcadero.services.') then
    LService := 'com.embarcadero.services.' + LService;
  Result.FIntent.setClassName(TAndroidHelper.Context.getPackageName(), TAndroidHelper.StringToJString(LService));
  Result.FIntent.putExtra(TAndroidHelper.StringToJString('Code'), Code);
  Result.FIntent.putExtra(TAndroidHelper.StringToJString('Data'), TAndroidHelper.StringToJString(Data));
end;

end.
