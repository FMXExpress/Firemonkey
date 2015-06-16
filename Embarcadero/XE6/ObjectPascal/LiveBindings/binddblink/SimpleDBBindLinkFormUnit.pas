
//---------------------------------------------------------------------------

// This software is Copyright (c) 2012 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit SimpleDBBindLinkFormUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, Data.Bind.EngExt,
  Fmx.Bind.DBEngExt, Fmx.Bind.Editors, Fmx.Bind.DBLinks, FMX.StdCtrls, FMX.Styles,
  Data.Bind.Components, Data.Bind.DBLinks, FMX.Layouts, FMX.Memo, FMX.Edit,
  Fmx.Bind.Navigator, Data.Bind.DBScope, Data.DB, Datasnap.DBClient,
  FMX.ListBox, FMX.Grid, System.Rtti, System.Bindings.Outputs;

type
  TForm13 = class(TForm)
    ClientDataSet1: TClientDataSet;
    DataSource1: TDataSource;
    BindSourceDB1: TBindSourceDB;
    BindNavigator1: TBindNavigator;
    Edit1: TEdit;
    Memo1: TMemo;
    CheckBox1: TCheckBox;
    ImageControl1: TImageControl;
    BindingsList1: TBindingsList;
    DBLinkEdit1Category1: TBindDBEditLink;
    DBLinkMemo1Notes1: TBindDBMemoLink;
    DBLinkCheckBox1Endangered1: TBindDBCheckLink;
    DBLinkImageControl1Graphic1: TBindDBImageLink;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    StringGrid1: TStringGrid;
    ListBox1: TListBox;
    DBLinkListBox1Endangered1: TBindDBListLink;
    CheckBoxAutoEdit: TCheckBox;
    DBLinkStringGrid11: TBindDBGridLink;
    CheckBoxActive: TCheckBox;
    ComboBox1: TComboBox;
    DBLinkComboBox1Category1: TBindDBListLink;
    StyleBook1: TStyleBook;
    ComboEdit1: TComboEdit;
    DBLinkComboEdit1Endangered1: TBindDBEditLink;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    procedure FormCreate(Sender: TObject);
    procedure CheckBoxAutoEditChange(Sender: TObject);
    procedure CheckBoxActiveChange(Sender: TObject);
  private
    FChecking: Boolean;
    procedure OnIdle(Sender: TObject; var Done: Boolean);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form13: TForm13;

implementation

{$R *.fmx}

procedure TForm13.CheckBoxActiveChange(Sender: TObject);
begin
  if not FChecking then
    DataSource1.Enabled := CheckBoxActive.IsChecked;
end;

procedure TForm13.CheckBoxAutoEditChange(Sender: TObject);
begin
  if not FChecking then
    DataSource1.AutoEdit := CheckBoxAutoEdit.IsChecked;
end;

procedure TForm13.FormCreate(Sender: TObject);
begin
  Application.OnIdle := OnIdle;
end;

procedure TForm13.OnIdle(Sender: TObject; var Done: Boolean);
begin
  FChecking := True;
  try
    CheckBoxAutoEdit.IsChecked := DataSource1.AutoEdit;
    CheckBoxActive.IsChecked := DataSource1.Enabled;
  finally
    FChecking := False;
  end;
end;

end.
