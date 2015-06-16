unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  DPF.iOS.BaseControl,
  DPF.iOS.UILabel, DPF.iOS.UIView, DPF.iOS.UIButton, DPF.iOS.UIPageControl;

type
  TFPageControl = class( TForm )
    DPFUIView1: TDPFUIView;
    DPFUIPageControl1: TDPFUIPageControl;
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FPageControl: TFPageControl;

implementation

{$R *.fmx}

procedure TFPageControl.PaintRects( const UpdateRects: array of TRectF );
begin
  { }

end;

end.
