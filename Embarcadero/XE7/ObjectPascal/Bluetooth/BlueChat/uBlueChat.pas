unit uBlueChat;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ListBox, FMX.StdCtrls, FMX.Edit,
  FMX.Layouts, FMX.Memo, System.Bluetooth, uChatManager, FMX.Controls.Presentation, uPairDevices;

type
  TFrmMainChatForm = class(TForm)
    MmReceived: TMemo;
    EdNewText: TEdit;
    BtSend: TButton;
    PnSelectDevice: TPanel;
    CbDevices: TComboBox;
    BtnUpdate: TButton;
    PnSend: TPanel;
    BtnFindNew: TButton;
    PnMain: TPanel;
    BackgroundImage: TImageControl;
    procedure FormShow(Sender: TObject);
    procedure BtnUpdateClick(Sender: TObject);
    procedure BtSendClick(Sender: TObject);
    procedure CbDevicesChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EdNewTextKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure BtnFindNewClick(Sender: TObject);
    procedure FormVirtualKeyboardShown(Sender: TObject; KeyboardVisible: Boolean; const Bounds: TRect);
    procedure FormVirtualKeyboardHidden(Sender: TObject; KeyboardVisible: Boolean; const Bounds: TRect);
  private
    FChatManager: TChatManager;
    FLastName: string;
    procedure UpdateKnownDevices;
    procedure OnNewText(const Sender: TObject; const AText: string; const aDeviceName: string);
  public
    { Public declarations }
  end;

var
  FrmMainChatForm: TFrmMainChatForm;

implementation

{$R *.fmx}

procedure TFrmMainChatForm.BtSendClick(Sender: TObject);
begin
  if cbDevices.Selected <> nil then
  begin
    FChatManager.SendText(EdNewText.Text);
    EdNewText.Text := '';
  end;
end;

procedure TFrmMainChatForm.BtnFindNewClick(Sender: TObject);
begin
  FrmPairdevices:= TFrmPairdevices.Create(nil);
  FrmPairdevices.ChatManager := FChatManager;
  FrmPairdevices.Show;
end;

procedure TFrmMainChatForm.CbDevicesChange(Sender: TObject);
begin
  FChatManager.SelectedDevice := CbDevices.Selected.Index;
end;

procedure TFrmMainChatForm.EdNewTextKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if key = vkReturn then
    BtSendClick(BtSend);
end;

procedure TFrmMainChatForm.FormCreate(Sender: TObject);
begin
  FChatManager := TChatManager.Create;
  if not FChatManager.HasBluetoothDevice then
  begin
    ShowMessage('You don''t have a bluetooth adapter');
    Application.Terminate;
  end;
  FChatManager.OnTextReceived := OnNewText;
  FChatManager.OnTextSent := OnNewText;
end;

procedure TFrmMainChatForm.FormShow(Sender: TObject);
begin
  UpdateKnownDevices;
end;

procedure TFrmMainChatForm.FormVirtualKeyboardHidden(Sender: TObject; KeyboardVisible: Boolean; const Bounds: TRect);
begin
  PnMain.align := TAlignLayout.Client;
end;

procedure TFrmMainChatForm.FormVirtualKeyboardShown(Sender: TObject; KeyboardVisible: Boolean; const Bounds: TRect);
begin
  PnMain.align := TAlignLayout.Top;
  PnMain.Height := ClientHeight - Bounds.height {$IFDEF ANDROID} + 20 {$ENDIF};
  MmReceived.GoToTextEnd;
end;

procedure TFrmMainChatForm.OnNewText(const Sender: TObject; const AText, aDeviceName: string);
begin
  TThread.Synchronize(nil,
    procedure begin
      if FLastName <> aDeviceName then
      begin
        MmReceived.Lines.Add(' ' + aDeviceName + ' :');
        FLastName := aDeviceName;
      end;
      MmReceived.Lines.Add('     ' + AText);
      MmReceived.GoToTextEnd;
    end);
end;

procedure TFrmMainChatForm.BtnUpdateClick(Sender: TObject);
begin
  UpdateKnownDevices;
end;

procedure TFrmMainChatForm.UpdateKnownDevices;
var
  I: Integer;
begin
  CbDevices.Clear;
  if FChatManager.KnownDevices <> nil then
    for I := 0 to FChatManager.KnownDevices.Count - 1 do
      CbDevices.Items.Add(FChatManager.KnownDevices.Items[I].DeviceName);
end;

end.
