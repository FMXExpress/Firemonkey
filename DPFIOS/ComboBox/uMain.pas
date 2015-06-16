unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, DPF.iOS.BaseControl,
  DPF.iOS.UIComboBox,
  DPF.iOS.UIButton,
  DPF.iOS.UILabel,
  DPF.iOS.UITextField,
  DPF.iOS.UIDateTimeComboBox,
  DPF.iOS.UIDatePicker,
  DPF.iOS.UIPickerView,
  DPF.iOS.UIScrollView,
  DPF.iOS.UIView, DPF.iOS.Keyboard;

type
  TFComboBox = class( TForm )
    DPFUIView1: TDPFUIView;
    DPFUIScrollView1: TDPFUIScrollView;
    DPFComboBox1: TDPFComboBox;
    DPFDateTimeComboBox1: TDPFDateTimeComboBox;
    DPFLabel1: TDPFLabel;
    DPFLabel2: TDPFLabel;
    DPFButton1: TDPFButton;
    DPFButton2: TDPFButton;
    DPFKeyboard1: TDPFKeyboard;
    DPFButton3: TDPFButton;
    procedure FormCreate( Sender: TObject );
    procedure DPFButton1Click( Sender: TObject );
    procedure DPFButton2Click( Sender: TObject );
    procedure DPFButton3Click( Sender: TObject );
  private
    { Private declarations }
  protected
    procedure DoShow; override;
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
    procedure OnPickerChanged( Sender: TObject; Component: Integer; Row: Integer );
  public
    { Public declarations }
  end;

var
  FComboBox: TFComboBox;

implementation

{$R *.fmx}

// ------------------------------------------------------------------------------
procedure TFComboBox.DoShow;
begin
  inherited;
  SendToBack;
end;

procedure TFComboBox.DPFButton1Click( Sender: TObject );
begin
  DPFComboBox1.Enabled := not DPFComboBox1.Enabled;
end;

procedure TFComboBox.DPFButton2Click( Sender: TObject );
begin
  DPFDateTimeComboBox1.Enabled := not DPFDateTimeComboBox1.Enabled;
end;

procedure TFComboBox.DPFButton3Click( Sender: TObject );
begin
  DPFDateTimeComboBox1.Clear;
end;

procedure TFComboBox.FormCreate( Sender: TObject );
var
  PVI: TDPFPickerViewItem;
begin
  PVI := DPFComboBox1.Picker.PickerComponents.Add;
  PVI.Items.Add( 'Picker Item 0' );
  PVI.Items.Add( 'Picker Item 1' );
  PVI.Items.Add( 'Picker Item 2' );
  PVI.Items.Add( 'Picker Item 3' );
  PVI.Items.Add( 'Picker Item 4' );
  PVI.Items.Add( 'Picker Item 5' );
  PVI.Items.Add( 'Picker Item 6' );
  PVI.Items.Add( 'Picker Item 7' );
  PVI.Items.Add( 'Picker Item 8' );
  PVI.Items.Add( 'Picker Item 9' );
  PVI.SelectedRow := 5;
  DPFComboBox1.Loaded;

  DPFDateTimeComboBox1.Picker.TimeZoneType := TDPFTimeZoneType.tztSystem;
  DPFDateTimeComboBox1.Picker.PickerDate   := now;

end;

// ------------------------------------------------------------------------------
procedure TFComboBox.OnPickerChanged( Sender: TObject; Component, Row: Integer );
begin

end;

// ------------------------------------------------------------------------------
procedure TFComboBox.PaintRects( const UpdateRects: array of TRectF );
begin
  { }

end;

// ------------------------------------------------------------------------------
end.
