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
unit RemoteServiceUnit;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Android.Service,
  AndroidApi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Os;

type
  TRemoteServiceDM = class(TAndroidService)
    function AndroidServiceHandleMessage(const Sender: TObject; const AMessage: JMessage): Boolean;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  RemoteServiceDM: TRemoteServiceDM;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

uses
  Androidapi.Helpers;

function TRemoteServiceDM.AndroidServiceHandleMessage(const Sender: TObject; const AMessage: JMessage): Boolean;
const
  GET_STRING = 123;
  SERVICE_STRING = 321;
var
  LMessage: JMessage;
  LBundle: JBundle;
begin
  case AMessage.what of
    GET_STRING:
    begin
      LBundle := TJBundle.Create;  // we can not send String because is not parcelable
      LMessage := TJMessage.Create;
      LMessage.what := SERVICE_STRING;
      LBundle.putString(TAndroidHelper.StringToJString('Key'), TAndroidHelper.StringToJString('This is a service text !!!'));
      LMessage.obj := LBundle;
      AMessage.replyTo.send(LMessage);
      Result := True;
    end;
  else
    Result := False;
  end;

end;

end.
