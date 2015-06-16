unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  DPF.iOS.BaseControl,
  DPF.iOS.UILabel, DPF.iOS.UIView, DPF.iOS.UIButton, DPF.iOS.UIImageView,
  DPF.iOS.Classes,
  DPF.iOS.Common,
  DPF.iOS.UIToolbar, DPF.iOS.UIViewController, DPF.iOS.UINavigationController,
  DPF.iOS.QLPreviewController;

type
  TFPDFExport = class( TForm )
    DPFUIViewController1: TDPFUIViewController;
    DPFImageView1: TDPFImageView;
    DPFLabel1: TDPFLabel;
    DPFQLPreviewController1: TDPFQLPreviewController;
    DPFToolbar1: TDPFToolbar;
    DPFImageView2: TDPFImageView;
    procedure DPFNavigationControllerPage1BarButtons0Click( Sender: TObject );
    procedure DPFToolbar1BarItems2Click( Sender: TObject );
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FPDFExport: TFPDFExport;

implementation

{$R *.fmx}

procedure TFPDFExport.DPFNavigationControllerPage1BarButtons0Click( Sender: TObject );
begin
  if CreatePDFfromView( DPFUIViewController1.FUIViewController, GetTempDirectory + 'myPDF.pdf' ) then
  begin
    ShowMessage( 'PDF Created.' );
  end
  else
    ShowMessage( 'PDF Not Created !' )
end;

procedure TFPDFExport.DPFToolbar1BarItems2Click( Sender: TObject );
begin
  DPFQLPreviewController1.ShowDoc( GetTempDirectory + 'myPDF.pdf', false );
end;

procedure TFPDFExport.PaintRects( const UpdateRects: array of TRectF );
begin
  { }

end;

end.
