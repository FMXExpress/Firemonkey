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
  DPF.iOS.UIToolbar, DPF.iOS.UIPrintPageRenderer;

type
  TFPrintPage = class( TForm )
    DPFUIView1: TDPFUIView;
    DPFQLPreviewController1: TDPFQLPreviewController;
    DPFToolbar1: TDPFToolbar;
    DPFUIPrintPageRenderer1: TDPFUIPrintPageRenderer;
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
  FPrintPage: TFPrintPage;

implementation

{$R *.fmx}

procedure TFPrintPage.DPFToolbar1BarItems0Click( Sender: TObject );
begin
  DPFUIPrintPageRenderer1.PrintToPDF( '<center></br></br></br></br></br><b><i><font size="18" color = "blue">D.P.F</font></i> PDF file generator from HTML !</b></center>', GetDocumentsFolder + 'test.pdf' );
  ShowMessage( 'PDF file created!' );
end;

procedure TFPrintPage.DPFToolbar1BarItems2Click( Sender: TObject );
begin
  if FileExists( GetDocumentsFolder + 'test.pdf' ) then
    DPFQLPreviewController1.ShowDoc( GetDocumentsFolder + 'test.pdf', false )
  else
    ShowMessage( 'PDF file not exists!' );
end;

procedure TFPrintPage.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.
