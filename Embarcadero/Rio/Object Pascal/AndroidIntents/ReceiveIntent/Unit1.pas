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
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Messaging, FMX.Platform,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, Androidapi.JNI.GraphicsContentViewText,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.WebBrowser, FMX.Objects, FMX.ScrollBox, FMX.Memo;

type
  TForm1 = class(TForm)
    ToolBar1: TToolBar;
    Layout1: TLayout;
    Memo1: TMemo;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    function HandleAppEvent(AAppEvent: TApplicationEvent; AContext: TObject): Boolean;
    procedure HandleActivityMessage(const Sender: TObject; const M: TMessage);
    function HandleIntentAction(const Data: JIntent): Boolean;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses FMX.Platform.Android, Androidapi.JNI.JavaTypes, Androidapi.JNI.Net, Androidapi.JNI.Os, Androidapi.Helpers;

procedure TForm1.FormCreate(Sender: TObject);
var
  AppEventService: IFMXApplicationEventService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXApplicationEventService, AppEventService) then
    AppEventService.SetApplicationEventHandler(HandleAppEvent);

  // Register the type of intent action that we want to be able to receive.
  // Note: A corresponding <action> tag must also exist in the <intent-filter> section of AndroidManifest.template.xml.
  MainActivity.registerIntentAction(TJIntent.JavaClass.ACTION_VIEW);
  TMessageManager.DefaultManager.SubscribeToMessage(TMessageReceivedNotification, HandleActivityMessage);
end;

procedure TForm1.HandleActivityMessage(const Sender: TObject; const M: TMessage);
begin
  if M is TMessageReceivedNotification then
    HandleIntentAction(TMessageReceivedNotification(M).Value);
end;

function TForm1.HandleAppEvent(AAppEvent: TApplicationEvent; AContext: TObject): Boolean;
var
  StartupIntent: JIntent;
begin
  Result := False;
  if AAppEvent = TApplicationEvent.BecameActive then
  begin
    StartupIntent := MainActivity.getIntent;
    if StartupIntent <> nil then
      HandleIntentAction(StartupIntent);
  end;
end;

function TForm1.HandleIntentAction(const Data: JIntent): Boolean;
var
  Extras: JBundle;
begin
  Result := False;
  if Data <> nil then
  begin
    Memo1.ClearContent;
    Extras := Data.getExtras;
    if Extras <> nil then
      Memo1.Text := JStringToString(Extras.getString(TJIntent.JavaClass.EXTRA_TEXT));
    Invalidate;
  end;
end;

end.

