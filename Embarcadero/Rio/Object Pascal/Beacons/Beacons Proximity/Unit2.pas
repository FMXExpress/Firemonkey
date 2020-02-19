//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit Unit2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.DialogService, FMX.StdCtrls, FMX.EditBox,
  FMX.SpinBox, FMX.Controls.Presentation, FMX.Edit, FMX.Layouts, FMX.ListBox, FMX.TabControl, System.Beacon,
  FMX.ListView.Types, FMX.ListView, BeaconsRender, FMX.Objects, System.Generics.Collections, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base;

type
  TForm2 = class(TForm)
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    Panel1: TPanel;
    Button1: TButton;
    btnStop: TButton;
    BtnAddRegion: TButton;
    BtnDeleteRegion: TButton;
    ListBox1: TListBox;
    EdGuid: TEdit;
    sbMinor: TSpinBox;
    sbMajor: TSpinBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    TabItem3: TTabItem;
    Panel3: TPanel;
    Timer1: TTimer;
    LvMonitoring: TListView;
    Button2: TButton;
    ScrollBox2: TScrollBox;
    TabItem4: TTabItem;
    BeaconsRectangle: TRectangle;
    Panel5: TPanel;
    Label10: TLabel;
    LbUUID: TLabel;
    LbMajor: TLabel;
    LbMinor: TLabel;
    Label14: TLabel;
    LbDistance: TLabel;
    Label16: TLabel;
    LbRssi: TLabel;
    SpinBox1: TSpinBox;
    ComboBox1: TComboBox;
    StyleBook1: TStyleBook;
    Label11: TLabel;
    Panel6: TPanel;
    TabControl2: TTabControl;
    TabItem5: TTabItem;
    TabItem6: TTabItem;
    TabItem7: TTabItem;
    TabItem8: TTabItem;
    TabItem9: TTabItem;
    LvProximity: TListView;
    LvExitedRegion: TListView;
    LvExitedBeacon: TListView;
    LvEnteredBeacon: TListView;
    LvEnteredRegion: TListView;
    procedure BtnAddRegionClick(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure BtnDeleteRegionClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure BeaconsRectanglePaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure BeaconsRectangleMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure SpinBox1Change(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure sbMajorChange(Sender: TObject);
  private
    FBeaconManager: TBeaconManager;
    FLock: TObject;
    FCurrentBeaconList: TBeaconList;
    FRenderer: TRenderer;
    FMaxDistance: Double;
    FList: TList<TBeaconGraphicInfo>;
    FSelectedBeacon: string;
    procedure CheckManager;
    procedure StringToRegion(AString: string; var Guid: string; var Major, Minor: Integer);
    procedure BeaconEnter(const Sender: TObject; const ABeacon: IBeacon; const CurrentBeaconList: TBeaconList);
    procedure BeaconExit(const Sender: TObject; const ABeacon: IBeacon; const CurrentBeaconList: TBeaconList);
    procedure EnterRegion(const Sender: TObject; const UUID: TGUID; Major, Minor: Integer);
    procedure ExitRegion(const Sender: TObject; const UUID: TGUID; Major, Minor: Integer);
    procedure BeaconProximity(const Sender: TObject; const ABeacon: IBeacon; Proximity: TBeaconProximity);

    procedure BtnAddRegionCloseEvent(Sender: TObject; const AResult: TModalResult);
    procedure AddRegion;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}


type
  TDummyInt = class
  public
    Number: Integer;
  end;


procedure TForm2.BtnAddRegionClick(Sender: TObject);
var
  LStringGuid: string;
  LMajor: Integer;
  LMinor: Integer;
  I: Integer;
  LNumberObj: TDummyInt;
begin  
  for I := 0 to ListBox1.Items.Count - 1 do
  begin
    StringToRegion(ListBox1.Items[I], LStringGuid, LMajor, LMinor);
    if LStringGuid = EdGuid.Text then
    begin
      if (Trunc(sbMajor.Value) = LMajor) and (Trunc(sbMinor.Value) = LMinor) then
        Exit
      else
      begin
        LNumberObj := TDummyInt.Create;
        LNumberObj.Number := I;
        TDialogService.MessageDialog('This region will replace previous region with same GUID ' + LStringGuid,
          TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbOk, TMsgDlgBtn.mbCancel], TMsgDlgBtn.mbCancel, 0,
          BtnAddRegionCloseEvent, LNumberObj);
        LNumberObj := nil;
        Exit;
      end;
    end;
  end;
  AddRegion;
end;

procedure TForm2.BtnAddRegionCloseEvent(Sender: TObject; const AResult: TModalResult);
var
  I: Integer;
begin
  I := TDummyInt(Sender).Number;
  TDummyInt(Sender).Free;
  if AResult <> mrOk then
    Exit;
  ListBox1.Items.Delete(I);
  AddRegion;
end;

procedure TForm2.AddRegion();
var
  LNewItem: string;
  ShouldAdd: boolean;
begin
  CheckManager;
  LNewItem := EdGuid.Text + ';' + Trunc(sbMajor.Value).ToString + ';' + Trunc(sbMinor.Value).ToString;
  if ListBox1.Items.IndexOf(LNewItem) < 0 then
  begin
    ShouldAdd := False;
    if sbMajor.Value > -1 then
    begin
      if sbMinor.Value > -1 then
       ShouldAdd := FBeaconManager.RegisterBeacon(TGUID.Create(EdGuid.Text), Trunc(sbMajor.Value), Trunc(sbMinor.Value))
      else
       ShouldAdd := FBeaconManager.RegisterBeacon(TGUID.Create(EdGuid.Text), Trunc(sbMajor.Value));
    end
    else
      ShouldAdd := FBeaconManager.RegisterBeacon(TGUID.Create(EdGuid.Text));

    if ShouldAdd then
      ListBox1.Items.Add(LNewItem);
  end;
end;

procedure TForm2.BtnDeleteRegionClick(Sender: TObject);
var
  LStringGuid: string;
  LMajor: Integer;
  LMinor: Integer;
  LUnregistered: Boolean;
begin
  CheckManager;
  if ListBox1.ItemIndex >= 0 then
  begin
    StringToRegion(ListBox1.Items[ListBox1.ItemIndex], LStringGuid, LMajor, LMinor);
    if LMajor = -1 then
      LUnregistered := FBeaconManager.UnregisterBeacon(TGUID.Create(LStringGuid))
    else if LMinor = -1 then
      LUnregistered := FBeaconManager.UnregisterBeacon(TGUID.Create(LStringGuid), LMajor)
    else
      LUnregistered := FBeaconManager.UnregisterBeacon(TGUID.Create(LStringGuid), LMajor, LMinor);

    if LUnregistered then
      ListBox1.Items.Delete(ListBox1.ItemIndex);
  end;

end;

procedure TForm2.btnStopClick(Sender: TObject);
begin
  CheckManager;
  if not FBeaconManager.StopScan then
    ShowMessage('Cannot stop to scan beacons')
  else
    Timer1.Enabled := False;
end;

procedure TForm2.Button1Click(Sender: TObject);
begin
  CheckManager;
  if not FBeaconManager.StartScan then
    ShowMessage('Cannot start to scan beacons')
  else
    Timer1.Enabled := True;
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  LvEnteredBeacon.Items.Clear;
  LvExitedBeacon.Items.Clear;
  LvEnteredRegion.Items.Clear;
  LvExitedRegion.Items.Clear;
  LvMonitoring.Items.Clear;
end;

procedure TForm2.BeaconProximity(const Sender: TObject; const ABeacon: IBeacon; Proximity: TBeaconProximity);
var
  LProximityText : string;
  LNewitem: TListViewItem;
  I: Integer;
begin

  case Proximity of
    TBeaconProximity.Immediate: LProximityText := 'Immediate';
    TBeaconProximity.Near: LProximityText := 'Near';
    TBeaconProximity.Far: LProximityText := 'Far';
    TBeaconProximity.Away: LProximityText := 'Away';
  end;

  LNewitem := lvProximity.Items.Add;
  LNewitem.Text := ABeacon.Guid.ToString;
  LNewitem.Detail := ' Ma:' + ABeacon.Major.ToString + ' Mi:' + ABeacon.Minor.ToString + ' Dist:' + ABeacon.Distance.ToString +
                     #13 + 'Proximity: ' +  LProximityText + ' time ' + TimeToStr(now);

  for I := 0 to FList.Count -1 do
  begin
    if FList.List[I].FName =  Abeacon.Guid.ToString + ';' + Abeacon.Major.ToString + ';' + Abeacon.Minor.ToString then
    begin
      case Proximity of
        TBeaconProximity.Immediate: FList.List[I].FOriginalColor := TAlphaColorRec.Green;
        TBeaconProximity.Near: FList.List[I].FOriginalColor := TAlphaColorRec.Yellow;
        TBeaconProximity.Far: FList.List[I].FOriginalColor := TAlphaColorRec.Red;
        TBeaconProximity.Away: FList.List[I].FOriginalColor := TAlphaColorRec.Black;
      end;
      Break;
    end;
  end;
end;

procedure TForm2.BeaconsRectangleMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  LName: string;
begin
  LName := FRenderer.GetObjectUnderMouse(BeaconsRectangle, TPointF.Create(X, Y));

  if LName <> '' then
  begin
    if FSelectedBeacon = LName then
      FSelectedBeacon := ''
    else
      FSelectedBeacon := LName;
    BeaconsRectangle.Repaint;
  end;
end;

procedure TForm2.BeaconsRectanglePaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
var
  I: Integer;
  LGuid: string;
  LMajor: Integer;
  LMinor: Integer;
begin
  if FSelectedBeacon <> '' then
  begin
    StringToRegion(FSelectedBeacon, LGuid, LMajor, LMinor);
    LbUUID.Text := LGuid;
    LbMajor.Text := LMajor.ToString;
    LbMinor.Text := LMinor.ToString;
    try
      TMonitor.Enter(FLock);
      for I := 0 to Length(FCurrentBeaconList) - 1 do
      begin
        if (FCurrentBeaconList[I] <> nil) and FCurrentBeaconList[I].ItsAlive then
        begin
          if (FCurrentBeaconList[I].Guid.ToString = LGuid) and (LMajor = FCurrentBeaconList[I].Major)
              and (LMinor = FCurrentBeaconList[I].Minor) then
          begin
            LbDistance.Text := FCurrentBeaconList[I].Distance.ToString;
            LbRssi.Text := FCurrentBeaconList[I].Rssi.ToString;
            break;
          end;
        end;
      end;
    finally
      TMonitor.Exit(FLock);
    end;
  end;

  for I:= 0 to FList.Count - 1 do
  begin
    if FList.Items[I].FName = FSelectedBeacon then
      FList.List[I].FColor := TAlphaColorRec.Blueviolet
    else
      FList.List[I].FColor := FList.List[I].FOriginalColor;
  end;
  FRenderer.Render(FMaxDistance, FList, BeaconsRectangle, Canvas, ARect);

end;

procedure TForm2.CheckManager;
begin
  if FBeaconManager = nil then
  begin
    FBeaconManager := TBeaconManager.GetBeaconManager(TBeaconScanMode(ComboBox1.ItemIndex));
    FBeaconManager.OnBeaconEnter := BeaconEnter;
    FBeaconManager.OnBeaconExit := BeaconExit;
    FBeaconManager.OnEnterRegion := EnterRegion;
    FBeaconManager.OnExitRegion := ExitRegion;
    FBeaconManager.OnBeaconProximity := BeaconProximity;
  end;
end;

procedure TForm2.ComboBox1Change(Sender: TObject);
var
  I: Integer;
  LGuid: string;
  LMajor: Integer;
  LMinor: Integer;
begin
  if FBeaconManager <> nil then
  begin
    FBeaconManager.StopScan;
    FBeaconManager.Free;
    FBeaconManager := nil;
  end;
  try
    CheckManager;
    for I := 0 to ListBox1.Count - 1 do
    begin
      StringToRegion(ListBox1.Items[I], LGuid, LMajor, LMinor);
      if LMajor = -1 then
        FBeaconManager.RegisterBeacon(TGuid.Create(LGuid))
      else if LMinor = -1 then
        FBeaconManager.RegisterBeacon(TGuid.Create(LGuid), LMajor)
      else
        FBeaconManager.RegisterBeacon(TGuid.Create(LGuid), LMajor, LMinor);
    end;
  except
    on e: exception do
      Showmessage(e.Message);
  end;
end;

procedure TForm2.ListBox1Click(Sender: TObject);
var
  LGuid: string;
  LMajor: Integer;
  LMinor: Integer;
begin
  if ListBox1.ItemIndex >= 0 then
  begin
    StringToRegion(ListBox1.Items[ListBox1.ItemIndex], LGuid, LMajor, LMinor);
    EdGuid.Text := LGuid;
    sbMajor.Value := LMajor;
    sbMinor.Value := LMinor;
  end;
end;

procedure TForm2.sbMajorChange(Sender: TObject);
begin
  if SbMajor.Value = -1 then
    SbMinor.Value := -1;
  if (SbMajor.Value >= 0) and (SbMinor.Value = -1) then
    SbMinor.Value := 0;
end;

procedure TForm2.SpinBox1Change(Sender: TObject);
begin
  FMaxDistance := SpinBox1.Value;
end;

procedure TForm2.StringToRegion(AString: string; var Guid: string; var Major: Integer; var Minor: Integer);
var
  LSplitted: TArray<string>;
begin
  LSplitted := AString.Split([';']);
  Guid := LSplitted[0];
  Major := LSplitted[1].ToInteger;
  Minor := LSplitted[2].ToInteger;
end;

procedure TForm2.Timer1Timer(Sender: TObject);
var
  I: Integer;
  LItem: TListViewItem;
begin
  TMonitor.Enter(FLock);
  try
    LvMonitoring.Items.Clear;
    FList.Count := Length(FCurrentBeaconList);

    for I := 0 to FList.Count - 1 do
    begin
      if (FCurrentBeaconList[I] <> nil) then
        if FCurrentBeaconList[I].ItsAlive then
        begin
          LItem := LvMonitoring.Items.Add;
          LItem.Text := FCurrentBeaconList[I].Guid.ToString;
          LItem.Detail := 'Major: ' + FCurrentBeaconList[I].Major.ToString + ' Minor: ' + FCurrentBeaconList[I].Minor.ToString +
                      'Proximity: ' + Integer(FCurrentBeaconList[I].GetProximity).ToString + #13 +
                     'Rssi: ' + FCurrentBeaconList[I].Rssi.ToString + ' Distance: ' + FCurrentBeaconList[I].Distance.ToString;
          FList.List[I].FOriginalColor := TAlphaColorRec.Blue;
          FList.List[I].FDistance := FCurrentBeaconList[I].Distance;
          FList.List[I].FName :=  FCurrentBeaconList[I].Guid.ToString + ';' + FCurrentBeaconList[I].Major.ToString + ';' + FCurrentBeaconList[I].Minor.ToString;
          case FCurrentBeaconList[I].Proximity of
            TBeaconProximity.Immediate: FList.List[I].FOriginalColor := TAlphaColorRec.Green;
            TBeaconProximity.Near: FList.List[I].FOriginalColor := TAlphaColorRec.Yellow;
            TBeaconProximity.Far: FList.List[I].FOriginalColor := TAlphaColorRec.Red;
            TBeaconProximity.Away: FList.List[I].FOriginalColor := TAlphaColorRec.Black;
          end;
        end;
    end;
    BeaconsRectangle.Repaint;
  finally
    TMonitor.Exit(FLock);
  end;
end;

procedure TForm2.EnterRegion(const Sender: TObject; const  UUID: TGUID; Major, Minor: Integer);
var
  LItem: TListViewItem;
begin
  LItem := LvEnteredRegion.Items.Add;
  LItem.Text := UUID.ToString;
  LItem.Detail := 'Major: ' + Major.ToString + ' Minor: ' + Minor.ToString  + ' time :' + TimeToStr(now) ;;
end;

procedure Tform2.ExitRegion(const Sender: TObject; const UUID: TGUID; Major, Minor: Integer);
var
  LItem: TListViewItem;
begin
  LItem := LvExitedRegion.Items.Add;
  LItem.Text := UUID.ToString;
  LItem.Detail := 'Major: ' + Major.ToString + ' Minor: ' + Minor.ToString + ' time :' + TimeToStr(now) ;;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  FLock := TObject.Create;
  FList := TList<TBeaconGraphicInfo>.Create;
  FRenderer := TRenderer.Create;
  FMaxDistance := 30;

  CheckManager;
  if FBeaconManager.RegisterBeacon(TGUID.Create('{B9407F30-F5F8-466E-AFF9-25556B57FE6D}')) then
      ListBox1.Items.Add('{B9407F30-F5F8-466E-AFF9-25556B57FE6D};-1;-1');
  if FBeaconManager.RegisterBeacon(TGUID.Create('{BFC8442B-819F-40C9-B56A-8B37FB9421E0}')) then
      ListBox1.Items.Add('{BFC8442B-819F-40C9-B56A-8B37FB9421E0};-1;-1');
  if FBeaconManager.RegisterBeacon(TGUID.Create('{2F234454-CF6D-4A0F-ADF2-F4911BA9FFA6}')) then
      ListBox1.Items.Add('{2F234454-CF6D-4A0F-ADF2-F4911BA9FFA6};-1;-1');
  if FBeaconManager.RegisterBeacon(TGUID.Create('{699EBC80-E1F3-11E3-9A0F-0CF3EE3BC012}')) then
      ListBox1.Items.Add('{699EBC80-E1F3-11E3-9A0F-0CF3EE3BC012};-1;-1');
end;

procedure TForm2.BeaconEnter(const Sender: TObject; const ABeacon: IBeacon; const CurrentBeaconList: TBeaconList);
var
  LItem: TListViewItem;
begin
  LItem := LvEnteredBeacon.Items.Add;
  LItem.Text := ABeacon.Guid.ToString;
  LItem.Detail := 'Major: ' + ABeacon.Major.ToString + ' Minor: ' + ABeacon.Minor.ToString  + ' time :' + TimeToStr(now) ;

  TMonitor.Enter(FLock);
  try
    FCurrentBeaconList := CurrentBeaconList;
  finally
    TMonitor.Exit(FLock);
  end;
end;

procedure TForm2.BeaconExit(const Sender: TObject; const ABeacon: IBeacon; const CurrentBeaconList: TBeaconList);
var
  LItem: TListViewItem;
begin
  LItem := LvExitedBeacon.Items.Add;
  LItem.Text := ABeacon.Guid.ToString;
  LItem.Detail := 'Major: ' + ABeacon.Major.ToString + ' Minor: ' + ABeacon.Minor.ToString + ' time :' + TimeToStr(now) ;

  TMonitor.Enter(FLock);
  try
    FCurrentBeaconList := CurrentBeaconList;
  finally
    TMonitor.Exit(FLock);
  end;
end;

end.
