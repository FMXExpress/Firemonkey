unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, DPF.iOS.BaseControl,
  DPF.iOS.UIStepper, DPF.iOS.UILabel, DPF.iOS.UIView;

type
  TForm3 = class( TForm )
    DPFStepper1: TDPFStepper;
    DPFLabel1: TDPFLabel;
    DPFUIView1: TDPFUIView;
    procedure DPFStepper1Changed( Sender: TObject; CurValue: Double );
    procedure FormShow( Sender: TObject );
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

procedure TForm3.DPFStepper1Changed( Sender: TObject; CurValue: Double );
begin
  DPFLabel1.Text := FloatToStr( CurValue );
end;

procedure TForm3.FormShow( Sender: TObject );
begin
  DPFLabel1.Text := FloatToStr( DPFStepper1.Value );
end;

procedure TForm3.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.
