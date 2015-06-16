unit UDemo;

interface

uses
  SysUtils, Types, UITypes, Classes, Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.ExtCtrls, FMX.TMSNavBar,
  FMX.TMSBaseControl, FMX.TreeView, FMX.Layouts, FMX.Edit, FMX.Ani, FMX.Objects,
  FMX.TMSHTMLText, FMX.TMSBitmapContainer, FMX.TMSBitmap, FMX.ListBox,
  FMX.Effects, FMX.StdCtrls, FMX.Calendar, FMX.Controls.Presentation
  {$if compilerversion > 25}
  ,FMX.DateTimeCtrls
  {$ifend}
  {$if compilerversion > 27}
  ,FMX.CalendarEdit
  {$ifend}
  ;

type
  TForm10 = class(TForm)
    TMSFMXNavBar1: TTMSFMXNavBar;
    TMSFMXNavBarPanel1: TTMSFMXNavBarPanel;
    TMSFMXNavBarPanel2: TTMSFMXNavBarPanel;
    TMSFMXNavBarPanel3: TTMSFMXNavBarPanel;
    TMSFMXNavBarPanel4: TTMSFMXNavBarPanel;
    Calendar1: TCalendar;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    Label1: TLabel;
    Edit1: TEdit;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    BitmapListAnimation1: TBitmapListAnimation;
    TMSFMXBitmap1: TTMSFMXBitmap;
    TMSFMXBitmap2: TTMSFMXBitmap;
    TMSFMXBitmap3: TTMSFMXBitmap;
    TMSFMXBitmap4: TTMSFMXBitmap;
    ListBox1: TListBox;
    TreeView1: TTreeView;
    TreeViewItem1: TTreeViewItem;
    TreeViewItem3: TTreeViewItem;
    TreeViewItem4: TTreeViewItem;
    TreeViewItem5: TTreeViewItem;
    TreeViewItem6: TTreeViewItem;
    TreeViewItem8: TTreeViewItem;
    TreeViewItem9: TTreeViewItem;
    TreeViewItem10: TTreeViewItem;
    TreeViewItem2: TTreeViewItem;
    TreeViewItem11: TTreeViewItem;
    TreeViewItem12: TTreeViewItem;
    ImageControl1: TImageControl;
    ShadowEffect1: TShadowEffect;
    ImageControl2: TImageControl;
    ShadowEffect2: TShadowEffect;
    ImageControl3: TImageControl;
    ShadowEffect3: TShadowEffect;
    ImageControl4: TImageControl;
    ShadowEffect4: TShadowEffect;
    ImageControl5: TImageControl;
    ShadowEffect5: TShadowEffect;
    TMSFMXHTMLText1: TTMSFMXHTMLText;
    TMSFMXHTMLText2: TTMSFMXHTMLText;
    TMSFMXHTMLText3: TTMSFMXHTMLText;
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form10: TForm10;

implementation

{$R *.fmx}

procedure TForm10.CheckBox1Change(Sender: TObject);
var
  idx: Integer;
begin
  idx := ListBox1.Items.IndexOf((Sender as TCheckBox).Text);
  if idx = -1 then
    ListBox1.Items.Add((Sender as TCheckBox).Text)
  else
    ListBox1.Items.Delete(idx);
end;

procedure TForm10.FormCreate(Sender: TObject);
begin
  TreeView1.ExpandAll;
  CheckBox1.IsChecked := True;
  CheckBox2.IsChecked := True;
  CheckBox4.IsChecked := True;
end;

end.
