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
unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, System.Android.Service, Androidapi.JNI.Os;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Panel4: TPanel;
    Button3: TButton;
    Panel5: TPanel;
    Button4: TButton;
    Panel2: TPanel;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    FServiceConnection: TRemoteServiceConnection;
    procedure OnServiceConnected(const ServiceMessenger: JMessenger);
    procedure OnHandleMessage(const AMessage: JMessage);

  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  AndroidApi.Helpers,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Widget;

procedure TForm1.Button2Click(Sender: TObject);
var
  LMessage: JMessage;
const
  GET_STRING = 123;
begin
  LMessage := TJMessage.JavaClass.obtain(nil, GET_STRING);
  LMessage.replyTo := FServiceConnection.LocalMessenger;
  FServiceConnection.ServiceMessenger.send(LMessage);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  FServiceConnection.BindService('com.embarcadero.AppRemoteHost',
    'com.embarcadero.services.RemoteService');
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  FServiceConnection.UnbindService;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FServiceConnection := TRemoteServiceConnection.Create;
  FServiceConnection.OnConnected := OnServiceConnected;
  FServiceConnection.OnHandleMessage := OnHandleMessage;

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FServiceConnection.Free;
end;

procedure TForm1.OnHandleMessage(const AMessage: JMessage);
const
  SERVICE_STRING = 321;
var
  LStr: JString;
  LBundle: JBundle;
begin
  case AMessage.what of
    SERVICE_STRING:
    begin
      LBundle := TJBundle.Wrap(AMessage.obj);
      LStr := LBundle.getString(TAndroidHelper.StringToJString('Key'));
      TJToast.JavaClass.makeText(TAndroidHelper.Context, LStr.subSequence(0, LStr.length),
        TJToast.JavaClass.LENGTH_SHORT).show;
    end;
  else
    FServiceConnection.Handler.Super.handleMessage(AMessage);
  end;
end;

procedure TForm1.OnServiceConnected(const ServiceMessenger: JMessenger);
begin
  Button2.Enabled := True;
end;

end.
