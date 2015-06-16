
//---------------------------------------------------------------------------

// This software is Copyright (c) 2012 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit BindListFMXFormUnit1;

// Example demonstrating TBindList components.
// Double click BindCompList1 to view TBindList components, then
// double a TBindList component to view and edit expressions.
// AutoFill property causes lists to populate when app is loaded.

interface

uses
  System.SysUtils, System.Classes, System.Variants, FMX.Types, FMX.Controls, FMX.Forms,
  FMX.Dialogs, Data.Bind.Components, Data.Bind.DBScope, FMX.Layouts, FMX.ListBox, Data.DB, Datasnap.DBClient,
  Data.Bind.EngExt, Fmx.Bind.DBEngExt, Fmx.Bind.Editors, System.Rtti, FMX.StdCtrls, FMX.Styles,
  System.Bindings.Outputs;

type
  TForm1 = class(TForm)
    BindCompList1: TBindingsList;
    BindSourceDB1: TBindSourceDB;
    ListBox1: TListBox;
    CategoryField: TStringField;
    SpeciesNameField: TStringField;
    LengthCmField: TFloatField;
    LengthInField: TFloatField;
    CommonNameField: TStringField;
    NotesField: TMemoField;
    GraphicField: TBlobField;
    ClientDataSet1: TClientDataSet;
    ClientDataSetDataSource1: TDataSource;
    BindListListBox: TBindList;
    ButtonClearList: TButton;
    ButtonFillList: TButton;
    ListBox2: TListBox;
    BindListListBox2: TBindList;
    procedure ButtonClearListClick(Sender: TObject);
    procedure ButtonFillListClick(Sender: TObject);
    procedure ButtonClearList2Click(Sender: TObject);
    procedure ButtonFillList2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.ButtonClearList2Click(Sender: TObject);
begin
  BindListListBox2.ClearList;
end;

procedure TForm1.ButtonClearListClick(Sender: TObject);
begin
  BindListListBox.ClearList;
end;

procedure TForm1.ButtonFillList2Click(Sender: TObject);
begin
  BindListListBox2.FillList;
end;

procedure TForm1.ButtonFillListClick(Sender: TObject);
begin
  BindListListBox.FillList;
end;

end.
