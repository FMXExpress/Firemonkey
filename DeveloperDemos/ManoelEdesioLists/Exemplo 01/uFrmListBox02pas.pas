unit uFrmListBox02pas;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ListBox,
  FMX.Controls.Presentation, FMX.Edit, FMX.SearchBox, FMX.StdCtrls, FMX.Layouts;

type
  TForm1 = class(TForm)
    ListBox1: TListBox;
    ListBoxHeader1: TListBoxHeader;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ListBoxGroupHeader1: TListBoxGroupHeader;
    ListBoxItem3: TListBoxItem;
    ListBoxItem4: TListBoxItem;
    StyleBook1: TStyleBook;
    Label1: TLabel;
    SearchBox1: TSearchBox;
    MetropolisUIListBoxItem1: TMetropolisUIListBoxItem;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}
{$R *.iPhone4in.fmx IOS}

end.
