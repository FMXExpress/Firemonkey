unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  DPF.iOS.BaseControl,
  DPF.iOS.UILabel,
  DPF.iOS.UIView,
  DPF.iOS.QLPreviewController,
  DPF.iOS.Common,
  DPF.iOS.UIToolbar;

type
  TFQuickLook = class( TForm )
    DPFUIView1: TDPFUIView;
    DPFQLPreviewController1: TDPFQLPreviewController;
    DPFToolbar1: TDPFToolbar;
    procedure DPFToolbar1BarItems0Click( Sender: TObject );
    procedure DPFToolbar1BarItems2Click( Sender: TObject );
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FQuickLook: TFQuickLook;

implementation

{$R *.fmx}

procedure TFQuickLook.DPFToolbar1BarItems0Click( Sender: TObject );
begin
  DPFQLPreviewController1.ShowDoc( GetDocumentsFolder + 'DPFiNC.pdf', false );
end;

procedure TFQuickLook.DPFToolbar1BarItems2Click( Sender: TObject );
begin
  DPFQLPreviewController1.ShowDoc( 'http://www.ztsinc.com/MBTMILSF_OI.pdf', true );
end;

procedure TFQuickLook.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.
