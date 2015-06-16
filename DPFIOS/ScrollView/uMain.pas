unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, DPF.iOS.BaseControl,
  DPF.iOS.UIPageControl, DPF.iOS.UIButton, DPF.iOS.UIScrollView, Math,
  DPF.iOS.UIView, DPF.iOS.CheckBox, DPF.iOS.UILabel;

type
  TForm3 = class( TForm )
    DPFUIScrollView1: TDPFUIScrollView;
    DPFButton1: TDPFButton;
    DPFButton2: TDPFButton;
    DPFCheckBox1: TDPFCheckBox;
    procedure DPFButton1Click( Sender: TObject );
    procedure FormShow( Sender: TObject );
    procedure DPFUIScrollView1ViewForZoomingInScrollView( Sender: TObject; var ViewForZoomObject: TDPFiOSBaseControl );
  private
    { Private declarations }
    C: TDPFButton;
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.fmx}

procedure TForm3.DPFButton1Click( Sender: TObject );
begin
  C                          := TDPFButton.Create( DPFUIScrollView1 );
  C.ButtonType               := TDPFButtonType.btCustom;
  C.BackgroundColorNormal    := TAlphaColors.Lightgreen;
  C.BackgroundColorHighlight := TAlphaColors.Green;
  C.Text                     := 'Custom Button';
  C.SetBounds( RandomRange( 0, 1000 ), RandomRange( 0, 450 ), 100, 50 );
  C.OnClick := DPFButton1Click;
  DPFUIScrollView1.AddControl( C );
end;

procedure TForm3.DPFUIScrollView1ViewForZoomingInScrollView( Sender: TObject; var ViewForZoomObject: TDPFiOSBaseControl );
begin
  ViewForZoomObject := DPFButton1;
end;

procedure TForm3.FormShow( Sender: TObject );
begin
  Randomize;
  DPFUIScrollView1.SetScrollPageContentSize( Width * 3, Height );
end;

procedure TForm3.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.
