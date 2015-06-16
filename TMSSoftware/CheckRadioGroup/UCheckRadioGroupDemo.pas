unit UCheckRadioGroupDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TMSRadioGroup, FMX.TMSBaseControl, FMX.TMSBaseGroup, FMX.TMSCheckGroup,
  FMX.ListBox, FMX.TMSBitmapContainer, FMX.Objects, FMX.TMSRadioGroupPicker,
  FMX.TMSCustomPicker, FMX.TMSCheckGroupPicker;

type
  TForm3 = class(TForm)
    TMSFMXCheckGroup1: TTMSFMXCheckGroup;
    TMSFMXRadioGroup1: TTMSFMXRadioGroup;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    cbColumns: TComboBox;
    TMSFMXBitmapContainer1: TTMSFMXBitmapContainer;
    cbGroupCheck: TCheckBox;
    cbBitmaps: TCheckBox;
    Label2: TLabel;
    Label3: TLabel;
    TMSFMXCheckGroupPicker1: TTMSFMXCheckGroupPicker;
    TMSFMXRadioGroupPicker1: TTMSFMXRadioGroupPicker;
    Label4: TLabel;
    Label5: TLabel;
    procedure cbColumnsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbGroupCheckChange(Sender: TObject);
    procedure cbBitmapsChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetColumns;
    procedure SetGroupCheckBox;
  end;

var
  Form3: TForm3;

implementation

{$R *.fmx}

procedure TForm3.SetColumns;
begin
  TMSFMXCheckGroup1.Columns := cbColumns.ItemIndex + 1;
  TMSFMXRadioGroup1.Columns := cbColumns.ItemIndex + 1;

  TMSFMXCheckGroupPicker1.Columns := cbColumns.ItemIndex + 1;
  TMSFMXRadioGroupPicker1.Columns := cbColumns.ItemIndex + 1;
end;

procedure TForm3.SetGroupCheckBox;
begin
  TMSFMXCheckGroup1.GroupCheckBox := cbGroupCheck.IsChecked;
  TMSFMXRadioGroup1.GroupCheckBox := cbGroupCheck.IsChecked;

  TMSFMXCheckGroupPicker1.GroupCheckBox := cbGroupCheck.IsChecked;
end;

procedure TForm3.cbBitmapsChange(Sender: TObject);
begin
  if cbBitmaps.IsChecked then
  begin
    TMSFMXCheckGroup1.BitmapContainer := TMSFMXBitmapContainer1;
    TMSFMXRadioGroup1.BitmapContainer := TMSFMXBitmapContainer1;

    TMSFMXCheckGroupPicker1.BitmapContainer := TMSFMXBitmapContainer1;
    TMSFMXRadioGroupPicker1.BitmapContainer := TMSFMXBitmapContainer1;
  end
  else
  begin
    TMSFMXCheckGroup1.BitmapContainer := nil;
    TMSFMXRadioGroup1.BitmapContainer := nil;

    TMSFMXCheckGroupPicker1.BitmapContainer := nil;
    TMSFMXRadioGroupPicker1.BitmapContainer := nil;
  end;
end;

procedure TForm3.cbColumnsChange(Sender: TObject);
begin
  SetColumns;
end;

procedure TForm3.cbGroupCheckChange(Sender: TObject);
begin
  SetGroupCheckBox;
end;

procedure TForm3.FormCreate(Sender: TObject);
var
  it: TTMSFMXGroupItem;
begin
  TMSFMXRadioGroup1.BeginUpdate;
  TMSFMXRadioGroup1.Width := 300;
  TMSFMXRadioGroup1.Height := 185;
  TMSFMXRadioGroup1.Text := 'Select OS';
  TMSFMXRadioGroup1.Items.Clear;
  it := TMSFMXRadioGroup1.Items.Add;
  it.Text := 'Windows <b>7</b>';
  it := TMSFMXRadioGroup1.Items.Add;
  it.Text := 'Windows <b>8</b>';
  it := TMSFMXRadioGroup1.Items.Add;
  it.Text := 'Mac OS X';
  it := TMSFMXRadioGroup1.Items.Add;
  it.Text := 'iOS';
  it := TMSFMXRadioGroup1.Items.Add;
  it.Text := 'Android';
  SetColumns;
  SetGroupCheckBox;
  TMSFMXRadioGroup1.GroupCheckBoxType := TTMSRadioGroupCheckBoxType.ctToggleEnabled;
  TMSFMXRadioGroup1.GroupCheckBoxChecked := true;
  TMSFMXRadioGroup1.ItemIndex := 1;
  TMSFMXRadioGroup1.EndUpdate;

  TMSFMXCheckGroup1.BeginUpdate;
  TMSFMXCheckGroup1.Width := 300;
  TMSFMXCheckGroup1.Height := 185;
  TMSFMXCheckGroup1.Text := 'Available Browsers';
  TMSFMXCheckGroup1.BitmapContainer := TMSFMXBitmapContainer1;
  TMSFMXCheckGroup1.Items.Clear;
  it := TMSFMXCheckGroup1.Items.Add;
  it.Text := 'Chrome';
  it.BitmapName := 'Chrome';
  it := TMSFMXCheckGroup1.Items.Add;
  it.Text := 'Internet Explorer';
  it.BitmapName := 'IE';
  it := TMSFMXCheckGroup1.Items.Add;
  it.Text := 'Firefox';
  it.BitmapName := 'Firefox';
  it := TMSFMXCheckGroup1.Items.Add;
  it.Text := 'Opera';
  it.BitmapName := 'Opera';
  it := TMSFMXCheckGroup1.Items.Add;
  it.Text := 'Safari';
  it.BitmapName := 'Safari';
  it := TMSFMXCheckGroup1.Items.Add;
  it.Text := 'Other';
  SetColumns;
  SetGroupCheckBox;
  TMSFMXCheckGroup1.GroupCheckBoxType := TTMSCheckGroupCheckBoxType.ctCheckAll;
  TMSFMXCheckGroup1.GroupCheckBoxChecked := false;
  TMSFMXCheckGroup1.IsChecked[0] := true;
  TMSFMXCheckGroup1.IsChecked[4] := true;
  TMSFMXCheckGroup1.EndUpdate;

  TMSFMXRadioGroupPicker1.BeginUpdate;
  TMSFMXRadioGroupPicker1.DropDownAutoSize := true;
  TMSFMXRadioGroupPicker1.Items.Clear;
  it := TMSFMXRadioGroupPicker1.Items.Add;
  it.Text := 'Windows <b>7</b>';
  it := TMSFMXRadioGroupPicker1.Items.Add;
  it.Text := 'Windows <b>8</b>';
  it := TMSFMXRadioGroupPicker1.Items.Add;
  it.Text := 'Mac OS X';
  it := TMSFMXRadioGroupPicker1.Items.Add;
  it.Text := 'iOS';
  it := TMSFMXRadioGroupPicker1.Items.Add;
  it.Text := 'Android';
  SetColumns;
  SetGroupCheckBox;
  TMSFMXRadioGroupPicker1.ItemIndex := 1;
  TMSFMXRadioGroupPicker1.EndUpdate;

  TMSFMXCheckGroupPicker1.BeginUpdate;
  TMSFMXCheckGroupPicker1.DropDownAutosize := true;
  TMSFMXCheckGroupPicker1.GroupCheckBoxText := 'Check All';
  TMSFMXCheckGroupPicker1.BitmapContainer := TMSFMXBitmapContainer1;
  TMSFMXCheckGroupPicker1.Items.Clear;
  it := TMSFMXCheckGroupPicker1.Items.Add;
  it.Text := 'Chrome';
  it.BitmapName := 'Chrome';
  it := TMSFMXCheckGroupPicker1.Items.Add;
  it.Text := 'Internet Explorer';
  it.BitmapName := 'IE';
  it := TMSFMXCheckGroupPicker1.Items.Add;
  it.Text := 'Firefox';
  it.BitmapName := 'Firefox';
  it := TMSFMXCheckGroupPicker1.Items.Add;
  it.Text := 'Opera';
  it.BitmapName := 'Opera';
  it := TMSFMXCheckGroupPicker1.Items.Add;
  it.Text := 'Safari';
  it.BitmapName := 'Safari';
  it := TMSFMXCheckGroupPicker1.Items.Add;
  it.Text := 'Other';
  SetColumns;
  SetGroupCheckBox;
  TMSFMXCheckGroupPicker1.GroupCheckBoxType := TTMSFMXPickerGroupCheckBoxType.ctCheckAll;
  TMSFMXCheckGroupPicker1.GroupCheckBoxChecked := false;
  TMSFMXCheckGroupPicker1.IsChecked[0] := true;
  TMSFMXCheckGroupPicker1.IsChecked[4] := true;
  TMSFMXCheckGroupPicker1.EndUpdate;
end;

end.
