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
unit MainFormU;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, StreamingDeviceFrameU,
  System.Generics.Collections, ListenerFrameU,
  ConnectionFrameU, LoggingFrameU, FMX.TabControl,
  System.JSON, NotifyDeviceFrameU, FMX.Layouts;

type
  TThingpointForm = class(TForm)
    HeartRateFrame: TStreamingDeviceFrame;
    EMSEdgeLoggingFrame1: TEMSEdgeLoggingFrame;
    EMSServerConnectionFrame1: TEMSServerConnectionFrame;
    EMSEdgeModuleListenerFrame1: TEMSEdgeModuleListenerFrame;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    BloodPressureFrame: TNotifyDeviceFrame;
    Layout1: TLayout;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure HeartRateFrameButtonStartStopClick(Sender: TObject);
    procedure BloodPressureFrameButtonNotifyClick(Sender: TObject);
    procedure HeartRateFrameConnectButtonClick(Sender: TObject);
  private
    procedure HeartRateChanged(Sender: TObject);
    procedure BloodPressureChanged(Sender: TObject);
    procedure OnIdle(Sender: TObject; var ADone: Boolean);
    procedure HeartRateClear(Sender: TObject);
    procedure BloodPressureClear(Sender: TObject);
    { Private declarations }
  public
    { Public declarations }
  end;


var
  ThingpointForm: TThingpointForm;

implementation

{$R *.fmx}

uses CacheDataModuleU, EdgeServiceModuleU, FMX.DialogService;

procedure TThingpointForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  EdgeServiceModule.EMSEdgeService1.Active := False;  // Unregister

end;

procedure TThingpointForm.FormCreate(Sender: TObject);
begin
  HeartRateFrame.OnChanged := HeartRateChanged;
  HeartRateFrame.OnClear := HeartRateClear;

  BloodPressureFrame.OnChanged := BloodPressureChanged;
  BloodPressureFrame.OnClear := BloodPressureClear;
  BloodPressureFrame.OnNextValue :=
    procedure (const AValue: TJSONObject)
    var
      LPressure: Integer;
    begin
      LPressure := 115 + Random(5);
      AValue.AddPair('systolic', TJSONNumber.Create(LPressure));
      LPressure := 75 + Random(5);
      AValue.AddPair('diastolic', TJSONNumber.Create(LPressure));
    end;

  EdgeServiceModule.OnModuleOverwrite :=
    function(const AModuleName: string): Boolean
    var
      LResult: Boolean;
    begin
      LResult := False;
      TDialogService.MessageDialog(Format('ModuleName "%0:s" is already registered.  Overwrite "%0:s?"', [AModuleName]),
        TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo, TMsgDlgBtn.mbCancel], TMsgDlgBtn.mbCancel, 0,
          procedure(const AResult: TModalResult)
            begin
              if AResult = mrYes then
                LResult := True
              else if AResult = mrCancel then
                LResult := True;
            end
        );
      Result := LResult;
    end;

  EdgeServiceModule.OnProtocolPropsConflictDelete :=
    function(const AModuleName, AProtocolProps: string): Boolean
    var
      LResult: Boolean;
    begin
      LResult := False;
      TDialogService.MessageDialog(
        Format('ModuleName "%0:s" has conflicting connection properties (%1:s).  Delete "%0:s?"',
        [AModuleName, AProtocolProps]), TMsgDlgType.mtConfirmation,
        [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo, TMsgDlgBtn.mbCancel], TMsgDlgBtn.mbCancel, 0,
          procedure(const AResult: TModalResult)
            begin
              if AResult = mrYes then
                LResult := True
              else if AResult = mrCancel then
                LResult := True;
            end
        );
      Result := LResult;
    end;

  EMSServerConnectionFrame1.EMSProvider := EdgeServiceModule.EMSProvider1;
  EMSEdgeModuleListenerFrame1.EMSEdgeService := EdgeServiceModule.EMSEdgeService1;
  Application.OnIdle := OnIdle;
end;

procedure TThingpointForm.OnIdle(Sender: TObject; var ADone: Boolean);
begin
  if EdgeServiceModule.EMSEdgeService1.Active then
    Caption := Format('Thingpoint: %s', [EdgeServiceModule.EMSEdgeService1.ModuleName])
  else
    Caption := 'Thingpoint (inactive)';
end;

procedure TThingpointForm.HeartRateChanged(Sender: TObject);
begin
  HeartRateFrame.LogValue;
  CacheDataModule.SaveDeviceData('heartrate', HeartRateFrame.Time, HeartRateFrame.Value)
end;

procedure TThingpointForm.HeartRateClear(Sender: TObject);
begin
  HeartRateFrame.ClearLog;
  CacheDataModule.ClearDeviceData('heartrate')
end;


procedure TThingpointForm.HeartRateFrameButtonStartStopClick(Sender: TObject);
begin
  HeartRateFrame.ActionStartStopExecute(Sender);

end;

procedure TThingpointForm.HeartRateFrameConnectButtonClick(Sender: TObject);
begin
  HeartRateFrame.ButtonConnectClick(Sender);

end;

procedure TThingpointForm.BloodPressureChanged(Sender: TObject);
begin
  BloodPressureFrame.LogValue;
  CacheDataModule.SaveDeviceData('bloodpressure', BloodPressureFrame.Time, BloodPressureFrame.Value)
end;

procedure TThingpointForm.BloodPressureClear(Sender: TObject);
begin
 BloodPressureFrame.ClearLog;
 CacheDataModule.ClearDeviceData('bloodpressure');
end;


procedure TThingpointForm.BloodPressureFrameButtonNotifyClick(Sender: TObject);
begin
  BloodPressureFrame.ActionStartStopExecute(Sender);

end;

end.
