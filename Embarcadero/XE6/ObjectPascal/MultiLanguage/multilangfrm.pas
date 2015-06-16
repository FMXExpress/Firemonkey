
//---------------------------------------------------------------------------

// This software is Copyright (c) 2012 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit multilangfrm;

interface

uses
  System.SysUtils, System.Variants, System.Classes,
  FMX.Forms, FMX.Dialogs, FMX.Controls, FMX.Types, FMX.StdCtrls,
  FMX.Layouts, FMX.ListBox, FMX.Objects;

type
  TfrmMultilang = class(TForm)
    vgLang1: TLang;
    Button1: TButton;
    ToolBar1: TToolBar;
    HudButton1: TButton;
    HudLabel1: TLabel;
    Label1: TLabel;
    HudCheckBox1: TCheckBox;
    StringListBox1: TListBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    StringListBox2: TListBox;
    StatusBar1: TStatusBar;
    Label5: TLabel;
    ListBoxItem6: TListBoxItem;
    ListBoxItem7: TListBoxItem;
    ListBoxItem8: TListBoxItem;
    ListBoxItem9: TListBoxItem;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure StringListBox1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMultilang: TfrmMultilang;

implementation

{$R *.fmx}

procedure TfrmMultilang.Button1Click(Sender: TObject);
begin
  ShowMessage('This is a message text.');
end;

procedure TfrmMultilang.FormCreate(Sender: TObject);
var
  i: integer;
  Item: TListBoxItem;
begin
  Item := TListBoxItem.Create(Self);
  Item.Parent := StringListBox1;
  Item.Text := 'en';
  Item.AutoTranslate := false;
  for i := 0 to vgLang1.Resources.Count - 1 do
  begin
    Item := TListBoxItem.Create(Self);
    Item.Parent := StringListBox1;
    Item.Text := vgLang1.Resources[i];
    Item.AutoTranslate := false;
    if vgLang1.Lang = Item.Text then
      StringListBox1.ItemIndex := StringListBox1.Count - 1;
  end;
end;

procedure TfrmMultilang.StringListBox1Change(Sender: TObject);
begin
  if StringListBox1.Selected <> nil then
    vgLang1.Lang := StringListBox1.Selected.Text;
end;

end.
