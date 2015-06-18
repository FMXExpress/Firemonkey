unit Unit3;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.ListBox, FMX.Layouts, FMX.Controls.Presentation, FMX.Edit, FMX.SearchBox;

type
  TForm3 = class(TForm)
    ListBox1: TListBox;
    ListBoxHeader1: TListBoxHeader;
    Label1: TLabel;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ListBoxGroupHeader1: TListBoxGroupHeader;
    ListBoxItem3: TListBoxItem;
    ListBoxItem4: TListBoxItem;
    Switch1: TSwitch;
    StyleBook1: TStyleBook;
    SearchBox1: TSearchBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.fmx}
{$R *.iPhone4in.fmx IOS}

end.
