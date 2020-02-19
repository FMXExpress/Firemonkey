//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit NotesResourceU;

// EMS Resource Module

interface

uses
  System.SysUtils, System.Classes, System.JSON, EMS.ResourceAPI, EMS.ResourceTypes,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef,
  FireDAC.Stan.ExprFuncs, FireDAC.ConsoleUI.Wait, FireDAC.Stan.Param,
  FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, Data.DB, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, EMS.DataSetResource;

type
  [ResourceName('Notes')]
  TNotesResource1 = class(TDataModule)
    FDConnection1: TFDConnection;
    FDQuery1: TFDQuery;

    [ResourceSuffix('/')]
    EMSDataSetResource1: TEMSDataSetResource;
  end;

procedure Register;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

(*
create table notes (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  title varchar(100),
  content memo
)
*)

procedure Register;
begin
  RegisterResource(TypeInfo(TNotesResource1));
end;

end.


