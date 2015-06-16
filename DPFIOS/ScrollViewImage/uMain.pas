unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, DPF.iOS.BaseControl, DPF.iOS.UIScrollView,
  DPF.iOS.UIImageView, FMX.Objects, DPF.iOS.UIButton, DPF.iOS.UIView;

type
  TForm3 = class( TForm )
    DPFUIScrollView1: TDPFUIScrollView;
    DPFImageView1: TDPFImageView;
    DPFUIView1: TDPFUIView;
    DPFButton1: TDPFButton;
    DPFButton2: TDPFButton;
    procedure DPFButton1Click( Sender: TObject );
    procedure DPFButton2Click( Sender: TObject );
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

procedure TForm3.DPFButton1Click( Sender: TObject );
begin
  DPFUIScrollView1.DeleteAllControls;
end;

procedure TForm3.DPFButton2Click( Sender: TObject );
var
  C: TDPFImageView;
  I: INteger;
begin
  for I := 0 to 10 do
  begin
    C        := TDPFImageView.Create( nil );
    C.Align  := TAlignLayout.alTop;
    C.Height := 100;
    C.Bitmap.Assign( DPFImageView1.Bitmap );
    DPFUIScrollView1.AddControl( C );
  end;
end;

procedure TForm3.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.
