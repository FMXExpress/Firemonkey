
//---------------------------------------------------------------------------

// This software is Copyright (c) 2012 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit FormUnit1;

interface

uses
  System.SysUtils, System.Classes, System.Variants, FMX.Types, FMX.Controls, FMX.Forms,
  FMX.Dialogs, Data.Bind.Components, Data.Bind.DBScope, FMX.Edit, FMX.Layouts, FMX.ListBox, Data.DB, Datasnap.DBClient,
  FMX.Memo, Fmx.Bind.Navigator, Fmx.Bind.DBEngExt, Data.Bind.EngExt, FMX.Menus, FMX.StdCtrls, FMX.Styles,
  Fmx.Bind.Editors, System.Rtti, System.Bindings.Outputs;

type
  TForm1 = class(TForm)
    EditWithHandler: TEdit;
    BindingsList1: TBindingsList;
    BindSourceDB1: TBindSourceDB;
    BindLinkEditHandler: TBindLink;
    CategoryField: TStringField;
    SpeciesNameField: TStringField;
    LengthCmField: TFloatField;
    LengthInField: TFloatField;
    CommonNameField: TStringField;
    NotesField: TMemoField;
    GraphicField: TBlobField;
    ClientDataSet1: TClientDataSet;
    DataSource1: TDataSource;
    MemoWithHandler: TMemo;
    BindLinkMemoHandler: TBindLink;
    ImageWithHandler: TImageControl;
    ListBoxWithHandler: TListBox;
    BindLinkListBoxHandler: TBindLink;
    BindLinkImageHandler: TBindLink;
    BindNavigator1: TBindNavigator;
    CheckBoxActiveDataSet: TCheckBox;
    LabelPosition: TLabel;
    BindLinkPosition: TBindLink;
    LabelFields: TLabel;
    BindLinkLabel: TBindLink;
    procedure FormCreate(Sender: TObject);
    procedure CheckBoxActiveDataSetChange(Sender: TObject);
  private
    FChecking: Boolean;
    procedure PopulateListBox;
    procedure OnIdle(Sender: TObject; var Done: Boolean);
  end;

var
  Form1: TForm1;

implementation


{$R *.fmx}

procedure TForm1.CheckBoxActiveDataSetChange(Sender: TObject);
begin
  DataSource1.Enabled := CheckBoxActiveDataSet.IsChecked;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  PopulateListBox;
  Application.OnIdle := OnIdle;
end;

procedure TForm1.PopulateListBox;
begin
  with ClientDataSet1 do
  begin
    // Populate category list
    First;
    while not EOF do
    begin
      ListBoxWithHandler.AddObject(TListBoxItem.Create(ListBoxWithHandler));
      ListBoxWithHandler.Items[ListBoxWithHandler.Count-1] := FieldByName('Category').AsString;
      Next;
    end;
    First;
  end;

end;

procedure TForm1.OnIdle(Sender: TObject; var Done: Boolean);
begin
  FChecking := True;
  try
    CheckBoxActiveDataSet.IsChecked := DataSource1.Enabled;
  finally
    FChecking := False;
  end;
end;

end.
