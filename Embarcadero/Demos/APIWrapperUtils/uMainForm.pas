unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, System.Actions, System.IOUtils, System.Math,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ListBox,
  FMX.Layouts, FMX.StdCtrls, FMX.TabControl, FMX.Objects, System.UIConsts,
  FMX.Gestures, FMX.ActnList, FMX.Memo, FMX.Edit,
  NetworkState, OpenViewUrl, SendMail, BarCodeReader,
  ToastMessage
{$IFDEF ANDROID}
    , Androidapi.JNI.JavaTypes, Androidapi.JNIBridge;
{$ENDIF}
{$IFDEF IOS}
;
{$ENDIF}

const
  bufferSize = 500; // BLUETOOTH receive buffer

type
  TMainForm = class(TForm)
    TabControlMain: TTabControl;
    tabOpenURL: TTabItem;
    tabNetworkStatus: TTabItem;
    ToolBar1: TToolBar;
    butOpen: TSpeedButton;
    lsbMain: TListBox;
    ListBoxGroupHeader1: TListBoxGroupHeader;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ListBoxItem3: TListBoxItem;
    ListBoxItem4: TListBoxItem;
    ListBoxItem6: TListBoxItem;
    ListBoxItem7: TListBoxItem;
    ListBoxItem8: TListBoxItem;
    ListBoxGroupHeader2: TListBoxGroupHeader;
    ListBoxItem5: TListBoxItem;
    ListBoxGroupHeader3: TListBoxGroupHeader;
    ListBoxItem9: TListBoxItem;
    ListBoxItem10: TListBoxItem;
    ListBoxItem11: TListBoxItem;
    ListBoxItem12: TListBoxItem;
    ToolBar2: TToolBar;
    CircleNET: TCircle;
    CircleWiFi: TCircle;
    Circle3G: TCircle;
    timNetwork: TTimer;
    SwitchNetwork: TSwitch;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    GestureManager1: TGestureManager;
    ActionList1: TActionList;
    ChangeTabAction0: TChangeTabAction;
    ChangeTabAction1: TChangeTabAction;
    tabSendMail: TTabItem;
    edtTo: TEdit;
    edtSubject: TEdit;
    memBody: TMemo;
    butSend: TButton;
    ChangeTabAction2: TChangeTabAction;
    tabBarCode: TTabItem;
    ToolBar3: TToolBar;
    butBarcode: TButton;
    lstBarCode: TListBox;
    butQRCode: TSpeedButton;
    ChangeTabAction3: TChangeTabAction;
    tabToast: TTabItem;
    Button1: TButton;
    ChangeTabAction4: TChangeTabAction;
    procedure butOpenClick(Sender: TObject);
    procedure timNetworkTimer(Sender: TObject);
    procedure SwitchNetworkSwitch(Sender: TObject);
    procedure TabControlMainGesture(Sender: TObject;
      const EventInfo: TGestureEventInfo; var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure butSendClick(Sender: TObject);
    procedure butBarcodeClick(Sender: TObject);
    procedure butQRCodeClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    fZBarReader: TBarCodeReader;
    fNetworkStatus: TMobileNetworkStatus;
    procedure MyZBarReaderGetResult(Sender: TBarCodeReader; AResult: string);
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}
{$IFDEF ANDROID}

uses uSplashScreen;
{$ENDIF}

procedure TMainForm.butOpenClick(Sender: TObject);
begin
  if lsbMain.Selected <> nil then
    if lsbMain.Selected.Text <> '' then
      OpenURL(lsbMain.Selected.Text);
end;

procedure TMainForm.butBarcodeClick(Sender: TObject);
begin
  fZBarReader.Show;
end;

procedure TMainForm.butQRCodeClick(Sender: TObject);
begin
  fZBarReader.Show(True);
end;

procedure TMainForm.butSendClick(Sender: TObject);
begin
  CreateEmail(edtTo.Text, edtSubject.Text, memBody.Text, '');
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  AndroidToast('Using Native Toast for Android!', ShortToast);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
{$IFDEF ANDROID}
  ShowSplashScreen;
  butQRCode.Visible := True;
{$ENDIF}
  fZBarReader := TBarCodeReader.Create(Self);
  fZBarReader.OnGetResult := MyZBarReaderGetResult;
  fNetworkStatus := TMobileNetworkStatus.Create;
end;

procedure TMainForm.MyZBarReaderGetResult(Sender: TBarCodeReader;
  AResult: string);
begin
  lstBarCode.Items.Insert(0, AResult);
end;

procedure TMainForm.SwitchNetworkSwitch(Sender: TObject);
begin
  timNetwork.Enabled := SwitchNetwork.Enabled;
end;

procedure TMainForm.TabControlMainGesture(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  case EventInfo.GestureID of
    sgiLeft:
      begin
        if TabControlMain.TabIndex = 0 then
        begin
          ChangeTabAction1.ExecuteTarget(Self);
          Handled := True;
        end
        else if TabControlMain.TabIndex = 1 then
        begin
          ChangeTabAction2.ExecuteTarget(Self);
          Handled := True;
        end
        else if TabControlMain.TabIndex = 2 then
        begin
          ChangeTabAction3.ExecuteTarget(Self);
          Handled := True;
        end
        else if TabControlMain.TabIndex = 3 then
        begin
          ChangeTabAction4.ExecuteTarget(Self);
          Handled := True;
        end;
      end;
    sgiRight:
      begin
        if TabControlMain.TabIndex = 4 then
        begin
          ChangeTabAction3.ExecuteTarget(Self);
          Handled := True;
        end
        else if TabControlMain.TabIndex = 3 then
        begin
          ChangeTabAction2.ExecuteTarget(Self);
          Handled := True;
        end
        else if TabControlMain.TabIndex = 2 then
        begin
          ChangeTabAction1.ExecuteTarget(Self);
          Handled := True;
        end
        else if TabControlMain.TabIndex = 1 then
        begin
          ChangeTabAction0.ExecuteTarget(Self);
          Handled := True;
        end;
      end;
  end;
end;

procedure TMainForm.timNetworkTimer(Sender: TObject);
begin
  if fNetworkStatus.isConnected then
    CircleNET.Fill.Color := claGreen
  else
    CircleNET.Fill.Color := claRed;

  if fNetworkStatus.IsWiFiConnected then
    CircleWiFi.Fill.Color := claGreen
  else
    CircleWiFi.Fill.Color := claRed;

  if fNetworkStatus.IsMobileConnected then
    Circle3G.Fill.Color := claGreen
  else
    Circle3G.Fill.Color := claRed;
end;

end.
