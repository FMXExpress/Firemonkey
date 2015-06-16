unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, DPF.iOS.BaseControl,
  DPF.iOS.UILabel, DPF.iOS.UISwitch, DPF.iOS.UIView;

type
  TForm3 = class( TForm )
    DPFSwitch1: TDPFSwitch;
    DPFLabel1: TDPFLabel;
    DPFUIView1: TDPFUIView;
    procedure DPFSwitch1Changed( Sender: TObject; ISON: Boolean );
    procedure FormCreate( Sender: TObject );
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

procedure TForm3.DPFSwitch1Changed( Sender: TObject; ISON: Boolean );
begin
  DPFLabel1.Text := BoolToStr( ISON, True );
end;

procedure TForm3.FormCreate( Sender: TObject );
begin
  DPFLabel1.Text := BoolToStr( DPFSwitch1.ISON, True );
end;

procedure TForm3.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.
