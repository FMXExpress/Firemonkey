unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, DPF.iOS.BaseControl,
  DPF.iOS.UISlider, DPF.iOS.UILabel, DPF.iOS.UISegmentedControl, DPF.iOS.UIView,
  DPF.iOS.UIButton;

type
  TFSegmentedControl = class( TForm )
    DPFSegmentedControl1: TDPFSegmentedControl;
    DPFSegmentedControl2: TDPFSegmentedControl;
    DPFSegmentedControl3: TDPFSegmentedControl;
    DPFSegmentedControl4: TDPFSegmentedControl;
    DPFSegmentedControl5: TDPFSegmentedControl;
    DPFUIView1: TDPFUIView;
    DPFSegmentedControl6: TDPFSegmentedControl;
    DPFButton1: TDPFButton;
    DPFSegmentedControl7: TDPFSegmentedControl;
    procedure DPFSegmentedControl2Changed( Sender: TObject; SegmentIndex: Integer );
    procedure DPFSegmentedControl1Changed( Sender: TObject; SegmentIndex: Integer );
    procedure DPFSegmentedControl3Changed( Sender: TObject; SegmentIndex: Integer );
    procedure DPFSegmentedControl4Changed( Sender: TObject; SegmentIndex: Integer );
    procedure DPFSegmentedControl5Changed( Sender: TObject; SegmentIndex: Integer );
    procedure DPFButton1Click( Sender: TObject );
    procedure DPFSegmentedControl7Changed( Sender: TObject; SegmentIndex: Integer );
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FSegmentedControl: TFSegmentedControl;

implementation

{$R *.fmx}

procedure TFSegmentedControl.DPFButton1Click( Sender: TObject );
var
  I: Integer;
begin
  DPFSegmentedControl2.Segments[1].Visible := not DPFSegmentedControl2.Segments[1].Visible;
  exit;

  DPFSegmentedControl6.ClearAll;

  for I := 1 to 3 do
    with DPFSegmentedControl6.Segments.Add do
    begin
      Title := 'Seg ' + I.ToString;
      Apply( false );
    end;
end;

procedure TFSegmentedControl.DPFSegmentedControl1Changed( Sender: TObject; SegmentIndex: Integer );
begin
  ShowMessage( IntToStr( SegmentIndex ) );
end;

procedure TFSegmentedControl.DPFSegmentedControl2Changed( Sender: TObject; SegmentIndex: Integer );
begin
  ShowMessage( IntToStr( SegmentIndex ) );
end;

procedure TFSegmentedControl.DPFSegmentedControl3Changed( Sender: TObject; SegmentIndex: Integer );
begin
  ShowMessage( IntToStr( SegmentIndex ) );
end;

procedure TFSegmentedControl.DPFSegmentedControl4Changed( Sender: TObject; SegmentIndex: Integer );
begin
  ShowMessage( IntToStr( SegmentIndex ) );
end;

procedure TFSegmentedControl.DPFSegmentedControl5Changed( Sender: TObject; SegmentIndex: Integer );
begin
  ShowMessage( IntToStr( SegmentIndex ) );
end;

procedure TFSegmentedControl.DPFSegmentedControl7Changed( Sender: TObject; SegmentIndex: Integer );
begin
  ShowMessage( IntToStr( SegmentIndex ) );
end;

procedure TFSegmentedControl.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.
