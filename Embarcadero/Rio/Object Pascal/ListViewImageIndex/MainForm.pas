unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.StdCtrls, FMX.Controls.Presentation, System.ImageList, FMX.ImgList,
  FMX.ListView, Data.Bind.GenData, Data.Bind.Components, Data.Bind.ObjectScope,
  Data.Bind.EngExt, Fmx.Bind.DBEngExt, System.Rtti, System.Bindings.Outputs,
  Fmx.Bind.Editors, FMX.TabControl;

type
  TForm1 = class(TForm)
    ListView1: TListView;
    ImageList1: TImageList;
    ToolBar1: TToolBar;
    Label1: TLabel;
    PrototypeBindSource1: TPrototypeBindSource;
    BindingsList1: TBindingsList;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    TabItem3: TTabItem;
    ListView2: TListView;
    LinkFillControlToField2: TLinkFillControlToField;
    ListView3: TListView;
    LinkFillControlToField1: TLinkFillControlToField;
    TabItem4: TTabItem;
    ListView4: TListView;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  // ListView1 uses a classic Appearance
  for I in [0..63] do
    with ListView1.Items.Add do
    begin
      Text := Format('%d pages', [1000 + Random(1234567)]);
      Detail := Format('%d kg of paper', [1000 + Random(1234)]);
      ImageIndex := Random(ImageList1.Count);
    end;

  // ListView4 uses a dynamic appearance with items named
  // Text1, Detail1, Portrait
  for I in [0..63] do
    with ListView4.Items.Add do
    begin
      Data['Text1'] := Format('%d pages', [1000 + Random(1234567)]);
      Data['Detail1'] := Format('%d kg of paper', [1000 + Random(1234)]);
      Data['Portrait'] := Random(ImageList1.Count);
    end;
end;

end.
