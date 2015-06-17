//---------------------------------------------------------------------------

// This software is Copyright (c) 2014 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
program BaaS_ToDo;

uses
  System.StartUpCopy,
  FMX.MobilePreview,
  FMX.Forms,
  REST.OpenSSL,
  DataModuleUnit1 in 'DataModuleUnit1.pas' {DataModule1: TDataModule},
  ToDoItemTypes in 'ToDoItemTypes.pas',
  BaaS_ToDoForm in 'BaaS_ToDoForm.pas' {BaaSToDoList};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TDataModule1, DataModule1);
  Application.CreateForm(TBaaSToDoList, BaaSToDoList);
  Application.Run;
end.
