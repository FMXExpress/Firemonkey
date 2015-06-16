unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  DPF.iOS.BaseControl,
  DPF.iOS.Common,
  DPF.iOS.UILabel, DPF.iOS.UIButton, DPF.iOS.UIView, DPF.iOS.UISwitch,
  DPF.iOS.UIToolbar;

type
  TFViewCurl = class( TForm )
    DPFUIView1: TDPFUIView;
    DPFSwitch1: TDPFSwitch;
    DPFUIView2: TDPFUIView;
    DPFToolbar1: TDPFToolbar;
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
  FViewCurl: TFViewCurl;

implementation

{$R *.fmx}

procedure TFViewCurl.DPFButton1Click( Sender: TObject );
begin

  DPFSwitch1.Visible := DPFButton1.TagBoolean;
  DPFUIView1.doCurl( DPFButton1.TagBoolean );
  DPFButton1.TagBoolean := not DPFButton1.TagBoolean;
end;

procedure TFViewCurl.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.
