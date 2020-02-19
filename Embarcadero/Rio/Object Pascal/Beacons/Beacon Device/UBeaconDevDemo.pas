//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit UBeaconDevDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Beacon, System.Beacon.Components, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Ani, FMX.Objects, FMX.Edit, FMX.ListBox, System.Bluetooth;

type
  TForm37 = class(TForm)
    EdtBeaconUUID: TEdit;
    EdtBeaconMajor: TEdit;
    EdtBeaconMinor: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    ImageControl1: TImageControl;
    Animation: TBitmapListAnimation;
    StyleBook1: TStyleBook;
    PnlBeaconInfo: TPanel;
    Panel2: TPanel;
    Label6: TLabel;
    EdTxPower: TEdit;
    BtnEnableBeacon: TButton;
    BeaconDevice1: TBeaconDevice;
    CbbBeaconType: TComboBox;
    Label2: TLabel;
    Label7: TLabel;
    EdtEddyURL: TEdit;
    EdtEddyNamespace: TEdit;
    Label8: TLabel;
    Label9: TLabel;
    EdtEddyInstance: TEdit;
    CbbNamespaceGeneration: TComboBox;
    Label10: TLabel;
    PnlEddyInfo: TPanel;
    BtnRandom: TButton;
    BtnRandomNamespace: TButton;
    procedure BtnRandomClick(Sender: TObject);
    procedure BtnEnableBeaconClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CbbBeaconTypeChange(Sender: TObject);
    procedure BtnRandomNamespaceClick(Sender: TObject);
  private
    { Private declarations }
    FGuid: TGUID;
    FMajor: Integer;
    FMinor: Integer;
    FTxPower: Integer;
    FEddyNamespace: string;
    FEddyInstance: string;
    FEddyURL: string;
    function RandomHexString(ABytesCount: Integer): string;
    function CheckValues: Boolean;
    procedure SetNamespaceGeneration;
  public
    { Public declarations }
  end;

var
  Form37: TForm37;

implementation

uses
  System.TypInfo;

{$R *.fmx}

procedure TForm37.BtnRandomClick(Sender: TObject);
var
  LGuid: TGUID;
begin
  CreateGUID(LGuid);
  EdtBeaconUUID.Text := Copy(LGuid.ToString, 2, Length(LGuid.ToString) - 2);
  if EdtBeaconMajor.Enabled then
  begin
    EdtBeaconMajor.Text := IntToStr(Random(High(Word)));
    EdtBeaconMinor.Text := IntToStr(Random(High(Word)));
  end;
end;

function TForm37.RandomHexString(ABytesCount: Integer): string;
const
  HEX_CHARS: string = '0123456789ABCDEF';
var
  I: Integer;
begin
  Result := '';
  for I := 1 to (ABytesCount * 2) do
    Result := Result + HEX_CHARS[Random(Length(HEX_CHARS)) + Low(HEX_CHARS)];
end;

procedure TForm37.BtnRandomNamespaceClick(Sender: TObject);
begin
  EdtEddyNamespace.Text := RandomHexString(10);
  EdtEddyInstance.Text := RandomHexString(6);
end;

procedure TForm37.BtnEnableBeaconClick(Sender: TObject);
begin
  if BtnEnableBeacon.IsPressed then
    if CheckValues then
    begin
      BeaconDevice1.GUID := FGuid;
      BeaconDevice1.Major := FMajor;
      BeaconDevice1.Minor := FMinor;
      BeaconDevice1.TxPower := FTxPower;
      BeaconDevice1.EddystoneNamespace := FEddyNamespace;
      BeaconDevice1.EddystoneInstance := FEddyInstance;
      BeaconDevice1.EddystoneURL := FEddyURL;
    end
    else
      BtnEnableBeacon.IsPressed := False;

  try
    BeaconDevice1.Enabled := BtnEnableBeacon.IsPressed;
  except
    on E: Exception do
    begin
      ShowMessage('Failed to start beacon: ' + E.Message);
      BeaconDevice1.Enabled := False;
      BtnEnableBeacon.IsPressed := False;
    end;
  end;

  if BtnEnableBeacon.IsPressed then
    BtnEnableBeacon.Text := 'Disable Beacon'
  else
    BtnEnableBeacon.Text := 'Enable Beacon';

  ImageControl1.Visible := BeaconDevice1.Enabled;
  Animation.Enabled := BeaconDevice1.Enabled;
  PnlBeaconInfo.Enabled := not BeaconDevice1.Enabled;
  PnlEddyInfo.Enabled := not BeaconDevice1.Enabled;
end;

procedure TForm37.CbbBeaconTypeChange(Sender: TObject);
begin
  BeaconDevice1.BeaconType :=  TBeaconDeviceMode(GetEnumValue(Typeinfo(TBeaconDeviceMode), CbbBeaconType.Selected.Text));
  PnlEddyInfo.Enabled := not (BeaconDevice1.BeaconType in [Standard, Alternative]);
  EdtEddyURL.Enabled := BeaconDevice1.BeaconType = EddystoneURL;
  EdtEddyNamespace.Enabled := BeaconDevice1.BeaconType = EddystoneUID;
  BtnRandomNamespace.Enabled := EdtEddyNamespace.Enabled;
  EdtEddyInstance.Enabled := BeaconDevice1.BeaconType = EddystoneUID;
  CbbNamespaceGeneration.Enabled := BeaconDevice1.BeaconType = EddystoneUID;
  EdtBeaconUUID.Enabled := BeaconDevice1.BeaconType in [Standard, Alternative, EddystoneUID];
  BtnRandom.Enabled := EdtBeaconUUID.Enabled;
  EdtBeaconMajor.Enabled := BeaconDevice1.BeaconType in [Standard, Alternative];
  EdtBeaconMinor.Enabled := BeaconDevice1.BeaconType in [Standard, Alternative];
end;

function TForm37.CheckValues: Boolean;

  function CheckUUID: Boolean;
  begin
    try
      FGuid := StringToGUID('{' + EdtBeaconUUID.Text + '}');
      Result := True;
    except
      on EConvertError do
      begin
        ShowMessage(EdtBeaconUUID.Text + ' is not a valid UUID value');
        Result := False;
      end;
    end;
  end;

begin

  case BeaconDevice1.BeaconType of
    Standard, Alternative:
      if not CheckUUID then
          Exit(False);

    EddystoneUID:
      begin
        SetNamespaceGeneration;
        case BeaconDevice1.NamespaceGenerator of
          ngNone, ngHashFQDN:
            if EdtEddyNamespace.Text.IsEmpty then
            begin
              ShowMessage('Please, enter a valid Namespace');
              EdtEddyNamespace.SetFocus;
              Exit(False);
            end;

          ngElidedUUID:
            if not CheckUUID then
              Exit(False);
        end;
        if EdtEddyInstance.Text.IsEmpty then
        begin
          ShowMessage('Please, enter a valid Instance value');
          EdtEddyInstance.SetFocus;
          Exit(False);
        end;

      end;

    EddystoneURL:
      if EdtEddyURL.Text.IsEmpty then
      begin
        ShowMessage('Please, enter a URL');
        EdtEddyURL.SetFocus;
        Exit(False);
      end;
  end;

  FMajor := StrToIntDef(EdtBeaconMajor.Text, 0);
  EdtBeaconMajor.Text := IntToStr(FMajor);
  FMinor := StrToIntDef(EdtBeaconMinor.Text, 0);
  EdtBeaconMinor.Text := IntToStr(FMinor);
  FTxPower := StrToIntDef(EdTxPower.Text, -56);
  EdTxPower.Text := IntToStr(FTxPower);
  FEddyNamespace := EdtEddyNamespace.Text;
  FEddyInstance := EdtEddyInstance.Text;
  FEddyURL := EdtEddyURL.Text;
  Result := True;
end;

procedure TForm37.FormCreate(Sender: TObject);
begin
{$IFNDEF MACOS}
  CbbBeaconType.Items.Add('Alternative');
{$ENDIF}
{$IFDEF ANDROID}
  CbbBeaconType.Items.Add('EddystoneUID');
  CbbBeaconType.Items.Add('EddystoneURL');
{$ENDIF}
{$IFNDEF ANDROID}
  PnlEddyInfo.Visible := False;
{$ENDIF}
  CbbBeaconTypeChange(nil);
end;

procedure TForm37.SetNamespaceGeneration;
begin
  BeaconDevice1.NamespaceGenerator := TNamespaceGeneratorMethod(CbbNamespaceGeneration.ItemIndex);
end;

end.
