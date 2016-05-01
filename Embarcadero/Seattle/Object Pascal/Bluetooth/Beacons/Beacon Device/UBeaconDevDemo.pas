unit UBeaconDevDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Beacon, System.Beacon.Components, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Ani, FMX.Objects, FMX.Edit;

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
    Label1: TLabel;
    Panel2: TPanel;
    BtnRandom: TButton;
    Label6: TLabel;
    EdTxPower: TEdit;
    BtnEnableBeacon: TButton;
    BeaconDevice1: TBeaconDevice;
    procedure BtnRandomClick(Sender: TObject);
    procedure BtnEnableBeaconClick(Sender: TObject);
  private
    { Private declarations }
    FGuid: TGUID;
    FMajor: Integer;
    FMinor: Integer;
    FTxPower: Integer;
    function CheckValues: Boolean;
  public
    { Public declarations }
  end;

var
  Form37: TForm37;

implementation

{$R *.fmx}

procedure TForm37.BtnRandomClick(Sender: TObject);
var
  LGuid: TGUID;
begin
  CreateGUID(LGuid);
  EdtBeaconUUID.Text := Copy(LGuid.ToString, 2, Length(LGuid.ToString) - 2);
  EdtBeaconMajor.Text := IntToStr(Random(High(Word)));
  EdtBeaconMinor.Text := IntToStr(Random(High(Word)));
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
    end
    else
      BtnEnableBeacon.IsPressed := False;

  if BtnEnableBeacon.IsPressed then
    BtnEnableBeacon.Text := 'Disable Beacon'
  else
    BtnEnableBeacon.Text := 'Enable Beacon';

  BeaconDevice1.Enabled := BtnEnableBeacon.IsPressed;
  ImageControl1.Visible := BeaconDevice1.Enabled;
  Animation.Enabled := BeaconDevice1.Enabled;
  PnlBeaconInfo.Enabled := not BeaconDevice1.Enabled;
end;

function TForm37.CheckValues: Boolean;
begin
  try
    FGuid := StringToGUID('{' + EdtBeaconUUID.Text + '}');
  except
    on EConvertError do
    begin
      ShowMessage(EdtBeaconUUID.Text + ' is not a valid UUID value');
      Exit(False);
    end;
  end;
  FMajor := StrToIntDef(EdtBeaconMajor.Text, 0);
  EdtBeaconMajor.Text := IntToStr(FMajor);
  FMinor := StrToIntDef(EdtBeaconMinor.Text, 0);
  EdtBeaconMinor.Text := IntToStr(FMinor);
  FTxPower := StrToIntDef(EdTxPower.Text, -56);
  EdTxPower.Text := IntToStr(FTxPower);
  Result := True;
end;

end.
