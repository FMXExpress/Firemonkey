unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  DPF.iOS.Common,
  DPF.iOS.BaseControl,
  DPF.iOS.UIButton,
  DPF.iOS.UIView,
  DPF.iOS.UIViewController,
  DPF.iOS.LocalAuthentication;

type
  TFLocalAuthentication = class( TForm )
    DPFButton2: TDPFButton;
    DPFUIViewController1: TDPFUIViewController;
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FLocalAuthentication: TFLocalAuthentication;

implementation

{$R *.fmx}

// ------------------------------------------------------------------------------
procedure TFLocalAuthentication.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.
