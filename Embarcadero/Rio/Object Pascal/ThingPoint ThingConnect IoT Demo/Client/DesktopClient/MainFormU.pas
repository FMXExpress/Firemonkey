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
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  EndpointResultFrameU, ClientSettingsU,
  FMX.TabControl, FMX.Layouts, FMX.Controls.Presentation,
  FMX.StdCtrls, FMX.ListBox, ConnectionFrameU, System.Actions, FMX.ActnList;

type
  TEMSThingPointForm = class(TForm)
    EMSServerConnectionFrame1: TEMSServerConnectionFrame;
    MeasurementsResultFrame: TEMSEndpointResultFrame;
    TabControl1: TTabControl;
    Layout1: TLayout;
    Layout2: TLayout;
    TabItemServerRequest: TTabItem;
    TabItemEdgeRequest: TTabItem;
    Layout3: TLayout;
    EdgeResultsFrame: TEMSEndpointResultFrame;
    ComboBoxEdgeName: TComboBox;
    ButtonRefreshEdgeNames: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MeasurementsResultFrameButtonExecuteClick(Sender: TObject);
    procedure ButtonRefreshEdgeNamesClick(Sender: TObject);
    procedure ComboBoxEdgeNameChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure EdgeResultsFrameButtonExecuteClick(Sender: TObject);
    procedure TabItemEdgeRequestClick(Sender: TObject);
  private
    { Private declarations }
    FSettingsList: TSettingsList;
    procedure OnIdle(Sender: TObject; var ADone: Boolean);
    procedure RefreshEdgeNames;
  public
    { Public declarations }
  end;

var
  EMSThingPointForm: TEMSThingPointForm;

implementation

{$R *.fmx}

uses ClientDataModuleU;

procedure TEMSThingPointForm.ButtonRefreshEdgeNamesClick(Sender: TObject);
begin
  RefreshEdgeNames;
end;

procedure TEMSThingPointForm.RefreshEdgeNames;
var
  LSave: string;
  S: string;
  LItems: TStrings;
  LIndex: Integer;
begin
  LSave := EMSClientDataModule.CurrentEdge;
  LItems := ComboBoxEdgeName.Items;
  LItems.BeginUpdate;
  try
    LItems.Clear;
    for S in EMSClientDataModule.GetEdgeNames do
      LItems.Add(S);
  finally
    LItems.EndUpdate;
  end;
  LIndex := LItems.IndexOf(LSave);
  if (LIndex < 0) and (LItems.Count > 0) then
    LIndex := 0;
  ComboBoxEdgeName.ItemIndex := LIndex;
  ComboBoxEdgeNameChange(Self);
end;


procedure TEMSThingPointForm.ComboBoxEdgeNameChange(Sender: TObject);
var
  LEdge: string;
begin
  if ComboBoxEdgeName.Selected <> nil then
    LEdge :=  ComboBoxEdgeName.Selected.Text;
  if LEdge <> EMSClientDataModule.CurrentEdge then
  begin
    EMSClientDataModule.CurrentEdge := LEdge;
    EdgeResultsFrame.Clear;
  end;
end;

procedure TEMSThingPointForm.EdgeResultsFrameButtonExecuteClick(
  Sender: TObject);
begin
  EdgeResultsFrame.ButtonExecuteClick(Sender);
  if EdgeResultsFrame.Empty then
    ShowMessage('No data');

end;

procedure TEMSThingPointForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  EdgeResultsFrame.SaveSettings;
  MeasurementsResultFrame.SaveSettings;
  FSettingsList.SaveToFile;
end;

procedure TEMSThingPointForm.FormCreate(Sender: TObject);
begin
  FSettingsList := TSettingsList.Create(SETTINGSDBFILE);
  EMSServerConnectionFrame1.EMSProvider := EMSClientDataModule.EMSProvider1;
  MeasurementsResultFrame.BackendEndpoint := EMSClientDataModule.BackendEndpointMeasurements;
  MeasurementsResultFrame.SettingsList := FSettingsList;
  EdgeResultsFrame.BackendEndpoint := EMSClientDataModule.BackendEndpointEdgeMeasurements;
  EdgeResultsFrame.SettingsList := FSettingsList;
  Application.OnIdle := OnIdle;
  TabControl1.ActiveTab := TabItemServerRequest; // First tab
end;

procedure TEMSThingPointForm.OnIdle(Sender: TObject; var ADone: Boolean);
begin
  EdgeResultsFrame.ButtonExecute.Enabled := EMSClientDataModule.CurrentEdge <> '';

end;

procedure TEMSThingPointForm.TabItemEdgeRequestClick(Sender: TObject);
begin
  RefreshEdgeNames;
end;

procedure TEMSThingPointForm.FormDestroy(Sender: TObject);
begin
  FSettingsList.Free;
end;

procedure TEMSThingPointForm.MeasurementsResultFrameButtonExecuteClick(
  Sender: TObject);
begin
  MeasurementsResultFrame.ButtonExecuteClick(Sender);
  if MeasurementsResultFrame.Empty then
    ShowMessage('No data');

end;

end.
