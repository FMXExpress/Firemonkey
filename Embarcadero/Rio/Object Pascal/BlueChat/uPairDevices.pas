//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit uPairDevices;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Layouts, FMX.ListBox, uChatManager,
  System.Bluetooth, FMX.Controls.Presentation;

type
  TFrmPairdevices = class(TForm)
    LbNewDevices: TListBox;
    BtnDiscover: TButton;
    BtnPair: TButton;
    BtnClose: TButton;
    procedure BtnCloseClick(Sender: TObject);
    procedure BtnDiscoverClick(Sender: TObject);
    procedure BtnPairClick(Sender: TObject);
  private
    { Private declarations }
    procedure DoDiscoverEnd(const Sender: TObject; const ADeviceList: TBluetoothDeviceList);
  public
    ChatManager: TChatManager;
    { Public declarations }
  end;

var
  FrmPairdevices: TFrmPairdevices;

implementation

{$R *.fmx}

procedure TFrmPairdevices.BtnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFrmPairdevices.BtnDiscoverClick(Sender: TObject);
begin
  ChatManager.OnDiscoveryEnd := DoDiscoverEnd;
  ChatManager.DiscoverDevices;
end;

procedure TFrmPairdevices.BtnPairClick(Sender: TObject);
begin
  if LbNewDevices.Selected <> nil then
    ChatManager.PairTo(LbNewDevices.Selected.Text);
end;

procedure TFrmPairdevices.DoDiscoverEnd(const Sender: TObject; const ADeviceList: TBluetoothDeviceList);
begin
  TThread.Synchronize(nil, procedure
  var
    I: Integer;
  begin
    LbNewDevices.Items.Clear;
    for I := 0 to ADeviceList.Count - 1 do
      if AdeviceList[I].IsPaired then
        LbNewDevices.Items.Add(AdeviceList[I].DeviceName + '*')
      else
        LbNewDevices.Items.Add(AdeviceList[I].DeviceName);
  end);
end;

end.
