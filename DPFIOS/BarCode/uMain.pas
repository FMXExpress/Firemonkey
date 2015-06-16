unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,

  DPF.iOS.BaseControl,
  DPF.iOS.BarCodeReader,
  DPF.iOS.UIButton,
  DPF.iOS.UILabel;

type
  TQRCode = class( TForm )
    DPFQRCodeScanner1: TDPFQRCodeScanner;
    DPFButton1: TDPFButton;
    DPFLabel1: TDPFLabel;
    DPFButton2: TDPFButton;
    procedure DPFButton1Click( Sender: TObject );
    procedure DPFQRCodeScanner1Scan( Sender: TObject; AText: string );
    procedure DPFButton2Click( Sender: TObject );
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  QRCode: TQRCode;

implementation

{$R *.fmx}

procedure TQRCode.DPFButton1Click( Sender: TObject );
begin
  DPFQRCodeScanner1.Start;
end;

procedure TQRCode.DPFButton2Click( Sender: TObject );
begin
  DPFQRCodeScanner1.Stop;
end;

procedure TQRCode.DPFQRCodeScanner1Scan( Sender: TObject; AText: string );
begin
  DPFLabel1.Text := AText;
end;

end.
