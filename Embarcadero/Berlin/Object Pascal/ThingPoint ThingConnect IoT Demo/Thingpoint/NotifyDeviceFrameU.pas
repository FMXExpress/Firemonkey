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
unit NotifyDeviceFrameU;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  System.Actions, FMX.ActnList, FMX.Controls.Presentation, FMX.ScrollBox,
  FMX.Memo, FMX.Layouts, System.JSON;

type
  TNotifyDeviceFrame = class(TFrame)
    ButtonNotify: TButton;
    Memo1: TMemo;
    Layout1: TLayout;
    Layout2: TLayout;
    Layout3: TLayout;
    Layout4: TLayout;
    ButtonClear: TButton;
    procedure ActionStartStopExecute(Sender: TObject);
    procedure ButtonClearClick(Sender: TObject);
  public type
    TNextValueCallback = reference to procedure(const Value: TJSONObject);
  private
    FValue: TJSONObject;
    FTime: TDateTime;
    FOnChanged: TNotifyEvent;
    FNextValueCallback: TNextValueCallback;
    FOnClear: TNotifyEvent;
    procedure DoClear;
    { Private declarations }
  protected
    procedure DoChanged; virtual;
    procedure DoNextValue; virtual;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LogValue;
    procedure NextValue;
    procedure ClearLog;
    property Value: TJSONObject read FValue;
    property Time: TDateTime read FTime;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property OnNextValue: TNextValueCallback write FNextValueCallback;
    property OnClear: TNotifyEvent read FOnClear write FOnClear;
  end;

implementation

{$R *.fmx}

procedure TNotifyDeviceFrame.ActionStartStopExecute(Sender: TObject);
begin
  NextValue;
  DoChanged;
end;

procedure TNotifyDeviceFrame.ButtonClearClick(Sender: TObject);
begin
  DoClear;
end;

procedure TNotifyDeviceFrame.ClearLog;
begin
  Memo1.Lines.Clear;
end;

procedure TNotifyDeviceFrame.DoClear;
begin
  if Assigned(FOnClear) then
    FOnClear(Self);
end;

constructor TNotifyDeviceFrame.Create(AOwner: TComponent);
begin
  inherited;
  FValue := TJSONObject.Create;
end;

destructor TNotifyDeviceFrame.Destroy;
begin
  FValue.Free;
  inherited;
end;

procedure TNotifyDeviceFrame.DoChanged;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TNotifyDeviceFrame.DoNextValue;
begin
  if Assigned(Self.FNextValueCallback) then
  begin
    while FValue.Count > 0 do
      FValue.RemovePair(FValue.Pairs[0].JsonString.Value);
    FNextValueCallback(FValue);
  end;
end;

procedure TNotifyDeviceFrame.LogValue;
begin
  Memo1.Lines.Insert(0, Format('[%s] %s', [FormatDateTime('mm:ss:zzz', FTime), Value.ToString]));
end;

procedure TNotifyDeviceFrame.NextValue;
begin
  DoNextValue;
  FTime := Now;
end;


end.
