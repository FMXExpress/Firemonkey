
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit Unit7;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, Data.Bind.EngExt,
  Fmx.Bind.DBEngExt, Fmx.Bind.Editors, Data.Bind.Components, Data.Bind.DBScope,
  Data.Bind.DBLinks, Fmx.Bind.DBLinks, FMX.Layouts, FMX.Grid, Data.DB,
  Datasnap.DBClient, FMX.Edit, System.Rtti, System.Bindings.Outputs,
  FMX.StdCtrls;

type
  TForm7 = class(TForm)
    ClientDataSet1: TClientDataSet;
    DataSource1: TDataSource;
    BindingsList1: TBindingsList;
    StringGrid1: TStringGrid;
    BindScopeDB1: TBindScopeDB;
    DBLinkStringGrid11: TBindDBGridLink;
    StringGrid2: TStringGrid;
    ClientDataSet2: TClientDataSet;
    DataSource2: TDataSource;
    BindScopeDB2: TBindScopeDB;
    DBLinkStringGrid21: TBindDBGridLink;
    Button1: TButton;
    StringGrid3: TStringGrid;
    ClientDataSet3: TClientDataSet;
    DataSource3: TDataSource;
    BindScopeDB3: TBindScopeDB;
    DBLinkStringGrid31: TBindDBGridLink;
    ProgressBar1: TProgressBar;
    Edit1: TEdit;
    DBLinkEdit1Length_In1: TBindDBEditLink;
    BindLink1: TBindLink;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form7: TForm7;

implementation

{$R *.fmx}

procedure TForm7.Button1Click(Sender: TObject);
begin
  StringGrid1.AnimateFloat('Height',20,1);
  StringGrid2.AnimateFloat('Height',20,1);
  StringGrid3.AnimateFloat('Height',20,1);

  StringGrid1.AnimateFloatDelay('Position.Y',StringGrid2.Position.Y,1,1);
  StringGrid2.AnimateFloatDelay('Position.Y',StringGrid3.Position.Y,1,1);
  StringGrid3.AnimateFloatDelay('Position.Y',StringGrid1.Position.Y,1,1);

  StringGrid1.AnimateFloatDelay('Height',200,1,2);
  StringGrid2.AnimateFloatDelay('Height',200,1,2);
  StringGrid3.AnimateFloatDelay('Height',200,1,2);
end;

end.
