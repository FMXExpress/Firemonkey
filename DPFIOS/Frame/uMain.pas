unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  DPF.iOS.BaseControl,
  DPF.iOS.UILabel,
  DPF.iOS.UIView,
  uFrame, uFrame2,
  DPF.iOS.UIButton,
  DPF.iOS.UIViewController,
  DPF.iOS.UITabBarController;

type
  TFMainForm = class( TForm )
    DPFUIView1: TDPFUIView;
    DPFUIView2: TDPFUIView;
    DPFButton1: TDPFButton;
    DPFButton2: TDPFButton;
    DPFUIViewController1: TDPFUIViewController;
    procedure DPFButton1Click( Sender: TObject );
    procedure DPFButton2Click( Sender: TObject );
  private
    { Private declarations }
  protected
    F: TFrame1;
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FMainForm: TFMainForm;

implementation

{$R *.fmx}

procedure TFMainForm.DPFButton1Click( Sender: TObject );
begin
  DPFUIViewController1.Frame := TFrame1;
end;

procedure TFMainForm.DPFButton2Click( Sender: TObject );
begin
  DPFUIViewController1.Frame := nil;
end;

procedure TFMainForm.PaintRects( const UpdateRects: array of TRectF );
begin
  { }

end;

end.
