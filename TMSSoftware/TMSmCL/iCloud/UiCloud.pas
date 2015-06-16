unit UiCloud;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, rtti, Math,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.ListBox, FMX.Edit, FMX.TabControl, FMX.TMSNativeMaciCloud;

type
  TForm1144 = class(TForm)
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    Label1: TLabel;
    Label2: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Button1: TButton;
    ListBox2: TListBox;
    Button2: TButton;
    Button3: TButton;
    TabItem2: TTabItem;
    ListBox1: TListBox;
    TMSFMXNativeMaciCloud1: TTMSFMXNativeMaciCloud;
    RadioButton3: TRadioButton;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    Label3: TLabel;
    procedure TMSFMXNativeMaciCloud1KeysChanged(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListBox2Change(Sender: TObject);
    procedure TMSFMXNativeMaciCloud1KeyAdded(Sender: TObject; AKeyName: string;
      AValue: TValue);
    procedure TMSFMXNativeMaciCloud1KeyRemoved(Sender: TObject;
      AKeyName: string);
    procedure TMSFMXNativeMaciCloud1KeyUpdate(Sender: TObject; AKeyName: string;
      APreviousValue, ACurrentValue: TValue);
  private
    { Private declarations }
    ActiveKey: Integer;
  public
    { Public declarations }
    procedure UpdateKeys;
    procedure UpdateActiveKey;
  end;

var
  Form1144: TForm1144;

implementation

{$R *.fmx}

procedure TForm1144.Button1Click(Sender: TObject);
var
  val: String;
begin
  if (Edit1.Text <> '') and (Edit2.Text <> '') then
  begin
    val := Edit1.Text;
    if RadioButton1.IsChecked then
      TMSFMXNativeMaciCloud1.AddKey(Edit2.Text, StrToInt(Edit1.Text))
    else if RadioButton2.IsChecked then
      TMSFMXNativeMaciCloud1.AddKey(Edit2.Text, StrToBool(Edit1.Text))
    else
      TMSFMXNativeMaciCloud1.AddKey(Edit2.Text, Edit1.Text);

    ActiveKey := TMSFMXNativeMaciCloud1.Keys.Count - 1;
    UpdateKeys;
  end
  else
    ShowMessage('Please enter Name and Value');
end;

procedure TForm1144.Button2Click(Sender: TObject);
begin
  if (ActiveKey >= 0) and (ActiveKey <= TMSFMXNativeMaciCloud1.Keys.Count - 1) then
  begin
    TMSFMXNativeMaciCloud1.RemoveKey(TMSFMXNativeMaciCloud1.Keys[ActiveKey]);
    ListBox2.Items.Delete(ActiveKey);
    ListBox2.ItemIndex := Max(0,Min(ListBox2.Items.Count - 1, ActiveKey));
  end;
end;

procedure TForm1144.Button3Click(Sender: TObject);
begin
  if (ActiveKey >= 0) and (ActiveKey <= TMSFMXNativeMaciCloud1.Keys.Count - 1) then
    TMSFMXNativeMaciCloud1.KeyValues[TMSFMXNativeMaciCloud1.Keys[ActiveKey].Name] := Edit2.Text;
end;

procedure TForm1144.FormCreate(Sender: TObject);
begin
  ActiveKey := -1;
end;

procedure TForm1144.ListBox2Change(Sender: TObject);
begin
  ActiveKey := ListBox2.ItemIndex;
  UpdateActiveKey;
end;

procedure TForm1144.TMSFMXNativeMaciCloud1KeyAdded(Sender: TObject;
  AKeyName: string; AValue: TValue);
begin
  ListBox1.Items.Insert(0, 'Add: Key ['+ AKeyName +'], Value ['+ AValue.ToString +']');
end;

procedure TForm1144.TMSFMXNativeMaciCloud1KeyRemoved(Sender: TObject;
  AKeyName: string);
begin
  ListBox1.Items.Insert(0, 'Delete: Key ['+ AKeyName +']');
end;

procedure TForm1144.TMSFMXNativeMaciCloud1KeysChanged(Sender: TObject);
begin
  UpdateKeys;
end;

procedure TForm1144.TMSFMXNativeMaciCloud1KeyUpdate(Sender: TObject;
  AKeyName: string; APreviousValue, ACurrentValue: TValue);
begin
  ListBox1.Items.Insert(0, 'Update: Key ['+ AKeyName +'], Previous Value ['+ APreviousValue.ToString +'], Current Value ['+ ACurrentValue.ToString +']');
end;

procedure TForm1144.UpdateActiveKey;
var
  k: TTMSFMXNativeMaciCloudKey;
begin
  if (ActiveKey >= 0) and (ActiveKey <= TMSFMXNativeMaciCloud1.Keys.Count - 1) then
  begin
    k := TMSFMXNativeMaciCloud1.Keys[ActiveKey];
    Edit2.Text := k.Name;
    Edit1.Text := k.Value.ToString;
    Button3.Enabled := True;
    Button2.Enabled := True;
    if k.Value.IsType<String> then
      RadioButton3.IsChecked := True
    else if k.Value.IsType<Boolean> then
      RadioButton2.IsChecked := True
    else if k.Value.IsType<Integer> then
      RadioButton1.IsChecked := True;
  end
  else
  begin
    RadioButton3.IsChecked := False;
    RadioButton2.IsChecked := False;
    RadioButton1.IsChecked := False;
    Edit1.Text := '';
    Edit2.Text := '';
    Button3.Enabled := False;
    Button2.Enabled := False;
  end;
end;

procedure TForm1144.UpdateKeys;
var
  I: Integer;
begin
  ListBox2.Items.Clear;
  for I := 0 to TMSFMXNativeMaciCloud1.Keys.Count - 1 do
    ListBox2.Items.Add(TMSFMXNativeMaciCloud1.Keys[I].Name);

  UpdateActiveKey;
end;

end.
