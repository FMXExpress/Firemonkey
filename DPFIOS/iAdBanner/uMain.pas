unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,

  DPF.iOS.BaseControl,
  DPF.iOS.UILabel,
  DPF.iOS.UIButton,
  DPF.iOS.UIView,
  DPF.iOS.ADBanner;

type
  TFiAdBanner = class( TForm )
    DPFUIView1: TDPFUIView;
    DPFADBanner1: TDPFADBanner;
    DPFLabel1: TDPFLabel;
    procedure DPFADBanner1ActionDidFinish( Sender: TObject );
    procedure DPFADBanner1ActionShouldBegin( Sender: TObject );
    procedure DPFADBanner1DidLoad( Sender: TObject );
    procedure DPFADBanner1Error( Sender: TObject; AMessage: string );
    procedure FormShow( Sender: TObject );
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FiAdBanner: TFiAdBanner;

implementation

{$R *.fmx}

procedure TFiAdBanner.DPFADBanner1ActionDidFinish( Sender: TObject );
begin
  DPFLabel1.Text := 'Action Did Finish';
end;

procedure TFiAdBanner.DPFADBanner1ActionShouldBegin( Sender: TObject );
begin
  DPFLabel1.Text := 'Action Should Begin';
end;

procedure TFiAdBanner.DPFADBanner1DidLoad( Sender: TObject );
begin
  DPFLabel1.Text := 'iAd Loaded';
end;

procedure TFiAdBanner.DPFADBanner1Error( Sender: TObject; AMessage: string );
begin
  DPFLabel1.Text := 'iAd Error: ' + AMessage;

end;

procedure TFiAdBanner.FormShow( Sender: TObject );
begin
  DPFADBanner1.Resize;
end;

procedure TFiAdBanner.PaintRects( const UpdateRects: array of TRectF );
begin
  { }

end;

end.
