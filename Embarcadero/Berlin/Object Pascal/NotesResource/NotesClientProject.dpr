//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

program NotesClientProject;

uses
  System.StartUpCopy,
  FMX.Forms,
  NoteTypesU in 'NoteTypesU.pas',
  NotesClientModuleU in 'NotesClientModuleU.pas' {NotesClientModule: TDataModule},
  NotesAdapterModuleU in 'NotesAdapterModuleU.pas' {NotesAdapterModule: TDataModule},
  NotesClientFormU in 'NotesClientFormU.pas' {ToDoList};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TNotesClientModule, NotesClientModule);
  Application.CreateForm(TNotesAdapterModule, NotesAdapterModule);
  //Application.CreateForm(TNotesClientForm, NotesClientForm);
  Application.CreateForm(TNotesClientForm, NotesClientForm);
  Application.Run;
end.
