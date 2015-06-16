unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,

  DPF.iOS.Common,
  DPF.iOS.BaseControl,
  DPF.iOS.UIButton,
  DPF.iOS.UILabel,
  DPF.iOS.UIView, DPF.iOS.UITableView;

type
  TFLabels = class( TForm )
    DPFUIView1: TDPFUIView;
    DPFUITableView1: TDPFUITableView;
    DPFUIView2: TDPFUIView;
    DPFUIView3: TDPFUIView;
    DPFUIView4: TDPFUIView;
    DPFUIView5: TDPFUIView;
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FLabels: TFLabels;

implementation

{$R *.fmx}

procedure TFLabels.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.
