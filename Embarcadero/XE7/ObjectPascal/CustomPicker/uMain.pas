unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Edit, FMX.ListBox,
  FMX.StdCtrls, FMX.Layouts;

type
  TCustomPickerForm = class(TForm)
    ComboBox1: TComboBox;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ListBoxItem3: TListBoxItem;
    ListBoxItem4: TListBoxItem;
    ListBox1: TListBox;
    ListBoxItem5: TListBoxItem;
    ListBoxItem6: TListBoxItem;
    ToolBar1: TToolBar;
    Label1: TLabel;
    procedure ComboBox1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  CustomPickerForm: TCustomPickerForm;

implementation

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}

procedure TCustomPickerForm.ComboBox1Change(Sender: TObject);
begin
  { update the label with the name of the picked item }
  ListBoxItem6.Text := Format('Picked %s', [ComboBox1.Items[ComboBox1.ItemIndex]]);
end;

end.
