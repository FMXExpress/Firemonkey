unit uMain;

interface

uses
  System.SysUtils, DateUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Edit,
  FMX.ExtCtrls, FMX.ListBox, FMX.Layouts, FMX.DateTimeCtrls;

type
  TDatePickerForm = class(TForm)
    ListBox1: TListBox;
    ListBoxItem5: TListBoxItem;
    ListBoxItem6: TListBoxItem;
    ToolBar1: TToolBar;
    Label1: TLabel;
    DateEdit1: TDateEdit;
    procedure DateEdit1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DatePickerForm: TDatePickerForm;

implementation

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}

procedure TDatePickerForm.DateEdit1Change(Sender: TObject);
begin
  { update the label with the date picked from the DateEdit component }
  ListBoxItem6.Text := Format('Picked %s', [FormatDateTime('dddd, mmmm d,yyyy', DateEdit1.Date)]);
end;

end.
