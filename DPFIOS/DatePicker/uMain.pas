unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,

  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,

  DPF.iOS.Common,
  DPF.iOS.BaseControl,
  DPF.iOS.UIDatePicker,
  DPF.iOS.UILabel,
  DPF.iOS.UIView;

type
  TFDatePicker = class( TForm )
    DPFDatePicker1: TDPFDatePicker;
    DPFLabel1: TDPFLabel;
    DPFUIView1: TDPFUIView;
    procedure DPFDatePicker1Change( Sender: TObject );
    procedure FormShow( Sender: TObject );
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FDatePicker: TFDatePicker;

implementation

{$R *.fmx}

procedure TFDatePicker.DPFDatePicker1Change( Sender: TObject );
begin
  DPFLabel1.Text := DateTimeToStr( DPFDatePicker1.PickerDate );
end;

procedure TFDatePicker.FormShow( Sender: TObject );
begin
  DPFDatePicker1.PickerDate := GetGMTDateTime( Now );
  DPFLabel1.Text            := DateTimeToStr( DPFDatePicker1.PickerDate );
end;

procedure TFDatePicker.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.
