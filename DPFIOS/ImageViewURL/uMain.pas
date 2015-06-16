unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, DPF.iOS.BaseControl,
  DPF.iOS.UIImageView, DPF.iOS.UIView, DPF.iOS.UIButton, DPF.iOS.UIProgressView,
  DPF.iOS.UILabel;

type
  TForm3 = class( TForm )
    DPFImageView1: TDPFImageView;
    DPFUIView1: TDPFUIView;
    DPFButton1: TDPFButton;
    DPFProgress1: TDPFProgress;
    DPFLabel1: TDPFLabel;
    procedure DPFButton1Click( Sender: TObject );
    procedure DPFImageView1Progress( Sender: TObject; const DownloadSize, DownloadedSize: Int64 );
    procedure FormShow( Sender: TObject );
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.fmx}

// ------------------------------------------------------------------------------
procedure TForm3.DPFButton1Click( Sender: TObject );
begin
  DPFImageView1.ClearImage;
  DPFImageView1.ImageList.Text := 'http://freebigpictures.com/wp-content/uploads/2009/09/spring-oak.jpg';
  DPFImageView1.ReloadImage;
end;

// ------------------------------------------------------------------------------
procedure TForm3.DPFImageView1Progress( Sender: TObject; const DownloadSize, DownloadedSize: Int64 );
begin
  if DownloadSize = 0 then
    exit;
  DPFProgress1.Progress := DownloadedSize / DownloadSize;

  DPFLabel1.Text := '%' + FormatFloat( '0.00', DPFProgress1.Progress * 100 );
end;

procedure TForm3.FormShow( Sender: TObject );
begin
  DPFProgress1.Progress := 0;
end;

procedure TForm3.PaintRects( const UpdateRects: array of TRectF );
begin
  { }

end;

// ------------------------------------------------------------------------------
end.
