unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, DPF.iOS.BaseControl,
  DPF.iOS.UIImageView, DPF.iOS.UIView;

type
  TFImageView = class( TForm )
    DPFImageView1: TDPFImageView;
    DPFImageView2: TDPFImageView;
    DPFUIView1: TDPFUIView;
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FImageView: TFImageView;

implementation

{$R *.fmx}

procedure TFImageView.PaintRects( const UpdateRects: array of TRectF );
begin
  { }

end;

end.
