unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,

  DPF.iOS.Common,
  DPF.iOS.BaseControl,
  DPF.iOS.UIButton,
  DPF.iOS.CTTelephonyNetwork,
  DPF.iOS.UILabel,
  DPF.iOS.UIView;

type
  TFCTInfo = class( TForm )
    DPFLabel2: TDPFLabel;
    DPFUIView1: TDPFUIView;
    DPFButtonCTInfo: TDPFButton;
    DPFLabel1: TDPFLabel;
    DPFLabel3: TDPFLabel;
    DPFLabel4: TDPFLabel;
    DPFLabel6: TDPFLabel;
    DPFLabel5: TDPFLabel;
    DPFLabel7: TDPFLabel;
    procedure DPFButtonCTInfoClick( Sender: TObject );
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FCTInfo: TFCTInfo;

implementation

{$R *.fmx}

// ------------------------------------------------------------------------------
procedure TFCTInfo.DPFButtonCTInfoClick( Sender: TObject );
var
  CarrierInfo: TCarrierInfo;
begin
  CarrierInfo := GetCarrierInfo;

  DPFLabel2.Text := 'Carrier Name: ' + CarrierInfo.carrierName;
  DPFLabel1.Text := 'ISO CountryCode: ' + CarrierInfo.isoCountryCode;
  DPFLabel3.Text := 'Mobile CountryCode: ' + CarrierInfo.mobileCountryCode;
  DPFLabel4.Text := 'Mobile NetworkCode: ' + CarrierInfo.mobileNetworkCode;
  DPFLabel5.Text := 'SignalStrength: ' + IntToStr( CarrierInfo.SignalStrength );
  DPFLabel6.Text := 'MyPhoneNumber: ' + CarrierInfo.MyPhoneNumber;
  DPFLabel7.Text := 'IMSI: ' + CarrierInfo.IMSI;
end;

// ------------------------------------------------------------------------------
procedure TFCTInfo.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

// ------------------------------------------------------------------------------
end.
