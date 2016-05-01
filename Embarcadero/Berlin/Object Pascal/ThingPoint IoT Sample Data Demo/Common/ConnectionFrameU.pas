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
unit ConnectionFrameU;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, FMX.Controls.Presentation, FMX.Layouts, IPPeerClient,
  REST.Backend.EMSProvider;

type
  TEMSServerConnectionFrame = class(TFrame)
    GroupBox1: TGroupBox;
    ButtonTestConnection: TButton;
    EditURLHost: TEdit;
    LabelURLHost: TLabel;
    EditURLPort: TEdit;
    LabelURLPort: TLabel;
    Layout1: TLayout;
    CheckBoxHTTPS: TCheckBox;
    procedure ButtonTestConnectionClick(Sender: TObject);
    procedure EditURLHostChange(Sender: TObject);
    procedure EditURLPortChange(Sender: TObject);
    procedure CheckBoxHTTPSChange(Sender: TObject);
  private
    FEMSProvider: TEMSProvider;
    function GetEMSProvider: TEMSProvider;
    procedure SetEMSProvider(const Value: TEMSProvider);
    procedure EMSProviderChanged;
    { Private declarations }
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    { Public declarations }
    property EMSProvider: TEMSProvider read GetEMSProvider write SetEMSProvider;
  end;

implementation

{$R *.fmx}

uses REST.JSON, System.JSON;

procedure TEMSServerConnectionFrame.SetEMSProvider(
  const Value: TEMSProvider);
begin
  if FEMSProvider <> Value then
  begin
    if FEMSProvider <> nil then
    begin
      FEMSProvider.RemoveFreeNotification(Self);
    end;
    FEMSProvider := Value;
    if FEMSProvider <> nil then
    begin
      FEMSProvider.FreeNotification(Self);
    end;
    EMSProviderChanged;
  end;
end;

procedure TEMSServerConnectionFrame.CheckBoxHTTPSChange(Sender: TObject);
begin
  if FEMSProvider <> nil then
    if CheckBoxHTTPS.IsChecked then
       FEMSProvider.URLProtocol := 'https'
    else
       FEMSProvider.URLProtocol := 'http'
end;

procedure TEMSServerConnectionFrame.EditURLHostChange(Sender: TObject);
begin
  if FEMSProvider <> nil then
    FEMSProvider.URLHost := EditURLHost.Text;
end;

procedure TEMSServerConnectionFrame.EditURLPortChange(Sender: TObject);
begin
  if FEMSProvider <> nil then
    FEMSProvider.URLPort := StrToInt(EditURLPort.Text);
end;

procedure TEMSServerConnectionFrame.EMSProviderChanged;
begin
  if FEMSProvider <> nil then
  begin
    EditURLHost.Text := FEMSProvider.URLHost;
    EditURLPort.Text := FEMSProvider.URLPort.ToString;
  end;
end;

procedure TEMSServerConnectionFrame.ButtonTestConnectionClick(Sender: TObject);
begin
  EMSProvider.AppHandshake(
    procedure(const AObj: TJSONObject)
    begin
      ShowMessage(TJson.Format(AObj));
    end);

end;

function TEMSServerConnectionFrame.GetEMSProvider: TEMSProvider;
begin
  if FEMSProvider = nil then
    raise Exception.Create('Missing EMSProvider');
  Result := FEMSProvider;
end;

procedure TEMSServerConnectionFrame.Notification(
  AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  /// clean up component-references
  if (Operation = opRemove) then
  begin
    if FEMSProvider = AComponent then
      FEMSProvider := nil;
  end;
end;

end.
