
//---------------------------------------------------------------------------

// This software is Copyright (c) 2012 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit FMXGridFormUnit1;

interface

uses
  System.SysUtils, System.Classes, System.Variants, FMX.Types, FMX.Controls, FMX.Forms,
  FMX.Dialogs, Data.Bind.Components, Data.Bind.DBScope, FMX.Edit, FMX.Layouts, FMX.ListBox, Data.DB, Datasnap.DBClient,
  FMX.Memo, Fmx.Bind.Navigator, Fmx.Bind.DBEngExt, Data.Bind.EngExt, FMX.Menus, FMX.StdCtrls, FMX.Styles,
  FMX.Grid, Fmx.Bind.Editors, System.Rtti,
  System.Bindings.Outputs;

type
  TForm1 = class(TForm)
    EditWithHandler: TEdit;
    BindingsList: TBindingsList;
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
    ClientDataSetDataSource1: TDataSource;
    ImageWithHandler: TImageControl;
    BindLinkImageHandler: TBindLink;
    BindNavigator1: TBindNavigator;
    CheckBoxActiveDataSet: TCheckBox;
    LabelPosition: TLabel;
    BindLinkPosition: TBindLink;
    LabelFields: TLabel;
    BindLinkLabel: TBindLink;
    BindGridLink1: TBindGridLink;
    StringGrid1: TStringGrid;
    StringColumn1: TStringColumn;
    StringColumn2: TStringColumn;
    StringColumn3: TStringColumn;
    EditWithHandler2: TEdit;
    BindLinkEditHandler2: TBindLink;
    procedure CheckBoxActiveDataSetChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FChecking: Boolean;
    { Private declarations }
    procedure OnIdle(Sender: TObject; var Done: Boolean);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation


{$R *.fmx}

procedure TForm1.CheckBoxActiveDataSetChange(Sender: TObject);
begin
  if not FChecking then
    ClientDataSetDataSource1.Enabled := CheckBoxActiveDataSet.IsChecked;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Application.OnIdle := OnIdle;
end;

procedure TForm1.OnIdle(Sender: TObject; var Done: Boolean);
begin
  FChecking := True;
  try
    CheckBoxActiveDataSet.IsChecked := ClientDataSetDataSource1.Enabled;
  finally
    FChecking := False;
  end;
end;

end.
