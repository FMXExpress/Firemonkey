unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, DPF.iOS.BaseControl,
  DPF.iOS.UIPageControl, DPF.iOS.UIButton, DPF.iOS.UIScrollView,
  DPF.iOS.UITableView, DPF.iOS.UIView;

type
  TForm3 = class( TForm )
    DPFUITableView1: TDPFUITableView;
    DPFUIView1: TDPFUIView;
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
{ TForm3 }

procedure TForm3.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.
