unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  System.TypInfo,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  DPF.iOS.BaseControl,
  DPF.iOS.Common,
  DPF.iOS.UIViewController,
  DPF.iOS.UIView,
  DPF.iOS.UIButton;

type
  TFViewControllerOverride = class( TForm )
    DPFUIViewController1: TDPFUIViewController;
    DPFButton1: TDPFButton;
    procedure DPFButton1Click( Sender: TObject );
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FViewControllerOverride: TFViewControllerOverride;

implementation

{$R *.fmx}

procedure TFViewControllerOverride.DPFButton1Click( Sender: TObject );
begin
  DPFUIViewController1.DisposeOf;
end;

procedure TFViewControllerOverride.PaintRects( const UpdateRects: array of TRectF );
begin
  { }

end;

end.
