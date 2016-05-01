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
  FMX.ListView.Types, FMX.StdCtrls, FMX.ListView, FMX.Controls.Presentation,
  FMX.MultiView, FMX.ListBox, FMX.Layouts, System.Actions, FMX.ActnList,
  FMX.TabControl, ConnectionFrameU, ClientDataModuleU, System.JSON,
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base;

type
  TMainForm = class(TForm)
    MultiView1: TMultiView;
    PanelDetail: TPanel;
    ToolBar1: TToolBar;
    Label2: TLabel;
    MasterButton: TSpeedButton;
    ListBox1: TListBox;
    ListView1: TListView;
    TabControl1: TTabControl;
    TabItemSettings: TTabItem;
    TabItemThings: TTabItem;
    ActionList1: TActionList;
    ActionBack: TAction;
    ActionForward: TAction;
    EMSServerConnectionFrame1: TEMSServerConnectionFrame;
    ButtonBack: TButton;
    ButtonForward: TButton;
    Label1: TLabel;
    ActionCaption: TAction;
    ButtonRefreshNames: TButton;
    ToolBar2: TToolBar;
    ButtonRefreshThing: TButton;
    SwitchDetailed: TSwitch;
    Layout1: TLayout;
    Timer1: TTimer;
    ActionAutoRefresh: TAction;
    Label3: TLabel;
    Layout2: TLayout;
    SwitchAutoRefresh: TSwitch;
    Label4: TLabel;
    ActionDetailed: TAction;
    procedure FormCreate(Sender: TObject);
    procedure ListBox1ItemClick(const Sender: TCustomListBox;
      const Item: TListBoxItem);
    procedure ActionBackExecute(Sender: TObject);
    procedure ActionBackUpdate(Sender: TObject);
    procedure ActionForwardExecute(Sender: TObject);
    procedure ActionForwardUpdate(Sender: TObject);
    procedure ActionCaptionUpdate(Sender: TObject);
    procedure ButtonRefreshNamesClick(Sender: TObject);
    procedure ListBox1Change(Sender: TObject);
    procedure ButtonRefreshThingClick(Sender: TObject);
    procedure SetAutoRefresh(AValue: Boolean);
    procedure Timer1Timer(Sender: TObject);
    procedure SwitchDetailedSwitch(Sender: TObject);
    procedure ActionAutoRefreshExecute(Sender: TObject);
    procedure ActionAutoRefreshUpdate(Sender: TObject);
    procedure ActionDetailedExecute(Sender: TObject);
    procedure ActionDetailedUpdate(Sender: TObject);
  private
    procedure ShowSettings;
    procedure ShowThings;
    procedure UpdateMultiview;
    procedure ListThings;
    procedure RefreshThing;
    function ExecuteEndpoint: TJSONArray;
    function ShowingAll: Boolean;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses REST.Backend.Endpoint;

procedure TMainForm.ActionCaptionUpdate(Sender: TObject);
var
  LShowingThings: Boolean;
begin
  LShowingThings := TabControl1.ActiveTab = TabItemThings;
  if LShowingThings then
  begin
    if ShowingAll then
      TAction(Sender).Text := 'All things'
    else if EMSClientDataModule.CurrentEdge <> '' then
      TAction(Sender).Text := EMSClientDataModule.CurrentEdge
    else
      TAction(Sender).Text := '(No Thing)'
  end
  else
    TAction(Sender).Text := 'Settings';
end;

procedure TMainForm.ActionDetailedExecute(Sender: TObject);
begin
  //
end;

procedure TMainForm.ActionDetailedUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not ShowingAll;
end;

function TMainForm.ShowingAll: Boolean;
begin
  Result := (ListBox1.Count > 0) and (ListBox1.ItemIndex = 0);
end;

procedure TMainForm.ActionAutoRefreshExecute(Sender: TObject);
begin
  SetAutoRefresh(TAction(Sender).Checked);
end;

procedure TMainForm.ActionAutoRefreshUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := EMSClientDataModule.CurrentEdge <> '';
  TAction(Sender).Checked := TAction(Sender).Enabled and Timer1.Enabled;
end;

procedure TMainForm.ActionBackExecute(Sender: TObject);
begin
  ShowSettings;
end;

procedure TMainForm.ActionBackUpdate(Sender: TObject);
begin
  TAction(Sender).Text := '';
  TAction(Sender).Visible := TabControl1.ActiveTab <> TabItemSettings;
  UpdateMultiview;
end;

procedure TMainForm.ActionForwardExecute(Sender: TObject);
begin
  ShowThings;
end;

procedure TMainForm.ActionForwardUpdate(Sender: TObject);
begin
  TAction(Sender).Text := 'Things';
  TAction(Sender).Visible := TabControl1.ActiveTab <> TabItemThings;
  UpdateMultiview;
end;

procedure TMainForm.ButtonRefreshNamesClick(Sender: TObject);
begin
  ListThings;
end;

procedure TMainForm.ButtonRefreshThingClick(Sender: TObject);
var
  LShowingThings: Boolean;
begin
  LShowingThings := TabControl1.ActiveTab = TabItemThings;
  if LShowingThings then
    RefreshThing;
end;

procedure TMainForm.UpdateMultiview;
var
  LShowingThings: Boolean;
begin
  LShowingThings := TabControl1.ActiveTab = TabItemThings;
  if LShowingThings then
  begin
    MultiView1.Visible := True;
    MultiView1.MasterButton := MasterButton;
    MasterButton.Visible := True;
  end
  else
  begin
    MultiView1.Visible := False;
    MultiView1.MasterButton := nil;
    MasterButton.Visible := False;
  end;
end;

procedure TMainForm.ShowThings;
begin
  if ListBox1.Items.Count = 0 then
    ListThings;
  if ListBox1.Items.Count = 0 then
  begin
    ShowMessage('No Things found');
  end
  else
    TabControl1.ActiveTab := TabItemThings;
end;

procedure TMainForm.SwitchDetailedSwitch(Sender: TObject);
begin
  SetAutoRefresh(SwitchAutoRefresh.IsChecked);
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  try
    RefreshThing;
  except
    Timer1.Enabled := False;
    raise;
  end;
end;

procedure TMainForm.SetAutoRefresh(AValue: Boolean);
begin
  Timer1.Enabled := AValue;
end;

procedure TMainForm.ShowSettings;
begin
  TabControl1.ActiveTab := TabItemSettings;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  EMSServerConnectionFrame1.EMSProvider := EMSClientDataModule.EMSProvider1;
  ShowSettings;
end;

procedure TMainForm.ListBox1Change(Sender: TObject);
var
  LEdge: string;
begin
  if ListBox1.Selected <> nil then
    LEdge :=  ListBox1.Selected.Text;
  if LEdge <> EMSClientDataModule.CurrentEdge then
  begin
    EMSClientDataModule.CurrentEdge := LEdge;
    RefreshThing;
  end;
end;

procedure TMainForm.ListBox1ItemClick(const Sender: TCustomListBox;
  const Item: TListBoxItem);
begin
  MultiView1.HideMaster;
end;

procedure TMainForm.ListThings;
var
  LSave: string;
  S: string;
  LItems: TStrings;
  LIndex: Integer;
begin
  LSave := EMSClientDataModule.CurrentEdge;
  LItems := ListBox1.Items;
  LItems.BeginUpdate;
  try
    LItems.Clear;
    for S in EMSClientDataModule.GetEdgeNames do
      LItems.Add(S);
    if LItems.Count > 0 then
      LItems.Insert(0, 'All');
 finally
    LItems.EndUpdate;
  end;
  LIndex := LItems.IndexOf(LSave);
  if (LIndex < 0) and (LItems.Count > 0) then
    LIndex := 0;
  ListBox1.ItemIndex := LIndex;
  ListBox1Change(Self);
end;


function TMainForm.ExecuteEndpoint: TJSONArray;
var
  LEndpoint: TBackendEndpoint;
begin
  if ShowingAll then
    LEndPoint := EMSClientDataModule.BackendEndpointMeasurements
  else if SwitchDetailed.IsChecked then
    LEndPoint := EMSClientDataModule.BackenEndpointEdgeDetailedMeasurements
  else
    LEndPoint := EMSClientDataModule.BackendEndpointEdgeMeasurements;
  LEndpoint.Execute;
  Result := LEndpoint.Response.JSONValue as TJSONArray;
end;

procedure TMainForm.RefreshThing;

  procedure ListObject(const APrefix: string; const AJSONObject: TJSONObject);
  var
    LPair: TJSONPair;
  begin
    for LPair in AJSONObject do
    begin
      if LPair.JsonValue is TJSONObject then
      begin
        ListObject(LPair.JSONString.Value, TJSONObject(LPair.JsonValue));
      end
      else
        with ListView1.Items.Add do
        begin
          //Purpose := TListItemPurpose.Header;
          if APrefix <> '' then
            Text := APrefix + '.' +  LPair.JsonString.Value
          else
            Text := LPair.JsonString.Value;
          if LPair.JsonValue is TJSONNumber then
            Detail := FormatFloat('.##', TJSONNumber(LPair.JsonValue).AsDouble)
          else
            Detail := LPair.JsonValue.Value;
        end;

    end;
  end;
var
  LJSONValue, LJSONValue2: TJSONValue;
  LJSONObject, LJSONObject2: TJSONObject;
  LDevice: string;
  LTime: TDateTime;
  LDataObject: TJSONObject;
  LEdge: string;
  LDataArray: TJSONArray;
begin
  ListView1.BeginUpdate;
  try
    ListView1.Items.Clear;
    if EMSClientDataModule.CurrentEdge <> '' then
    begin
      for LJSONValue in ExecuteEndpoint do
      begin
        LJSONObject := LJSONValue as TJSONObject;
        if LJSONObject.TryGetValue<string>('edge', LEdge) then
        begin
          // Summary by edge name
          with ListView1.Items.Add do
          begin
            Purpose := TListItemPurpose.Header;
            Text := LEdge;
          end;
          if LJSONObject.TryGetValue<TJSONArray>('data', LDataArray) then
            for LJSONValue2 in LDataArray do   // Array
            begin
              LJSONObject2 := LJSONValue2 as TJSONObject;
              LDevice := LJSONObject2.GetValue<string>('device');
              LTime := LJSONObject2.GetValue<TDateTime>('time');
              with ListView1.Items.Add do
              begin
                Text := LDevice;
                Detail := TimeToStr(LTime);
              end;
              if LJSONObject2.TryGetValue<TJSONObject>('data', LDataObject) then
                ListObject(LDevice, LDataObject as TJSONObject);
            end;
        end
        else
        begin
          // Informatation about a single edge.  Edge may have multiple devices.
          LDevice := LJSONObject.GetValue<string>('device');
          LTime := LJSONObject.GetValue<TDateTime>('time');
          with ListView1.Items.Add do
          begin
            Purpose := TListItemPurpose.Header;
            Text := LDevice;
            Detail := TimeToStr(LTime);
          end;
          if LJSONObject.TryGetValue<TJSONObject>('data', LDataObject) then
            ListObject('', LDataObject);
        end;
      end;
    end;

  finally
    ListView1.EndUpdate;
  end;

end;

end.
