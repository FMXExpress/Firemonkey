unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, DPF.iOS.BaseControl,
  DPF.iOS.UIPickerView, DPF.iOS.UILabel, DPF.iOS.UIButton, DPF.iOS.UIView;

type
  TFListPicker = class( TForm )
    DPFPickerView1: TDPFPickerView;
    DPFLabel1: TDPFLabel;
    DPFButton1: TDPFButton;
    DPFUIView1: TDPFUIView;
    procedure DPFPickerView1Changed( Sender: TObject; Component, Row: Integer );
    procedure DPFButton1Click( Sender: TObject );
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FListPicker: TFListPicker;

implementation

{$R *.fmx}

procedure TFListPicker.DPFButton1Click( Sender: TObject );
begin
  DPFPickerView1.PickerComponents.Items[0].SelectedRow := 4;
  DPFPickerView1.PickerComponents.Items[1].SelectedRow := 8;
  DPFPickerView1.PickerComponents.Items[2].SelectedRow := 18;

  DPFPickerView1.RefreshNeeded
end;

procedure TFListPicker.DPFPickerView1Changed( Sender: TObject; Component, Row: Integer );
begin
  DPFLabel1.Text := DPFPickerView1.PickerComponents.Items[0].Items[DPFPickerView1.PickerComponents.Items[0].SelectedRow] + '/' + DPFPickerView1.PickerComponents.Items[1].Items[DPFPickerView1.PickerComponents.Items[1].SelectedRow] + '/' + DPFPickerView1.PickerComponents.Items[2].Items[DPFPickerView1.PickerComponents.Items[2].SelectedRow];
end;

procedure TFListPicker.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.
