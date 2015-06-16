unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,

  FMX.Platform,
  FMX.Pickers,
  FMX.Pickers.iOS,

  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, DPF.iOS.BaseControl,
  uEmbForm1, uEmbForm2, DPF.iOS.UIButton, DPF.iOS.UIToolbar,
  DPF.iOS.UIViewController, DPF.iOS.UIView;

type
  TFFormEmbed = class( TForm )
    DPFUIViewController1: TDPFUIViewController;
    DPFUIViewController2: TDPFUIViewController;
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FFormEmbed: TFFormEmbed;

implementation

{$R *.fmx}

procedure TFFormEmbed.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.
