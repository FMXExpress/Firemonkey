//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit ClientFormU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, IPPeerClient, REST.Backend.EMSProvider,
  REST.Backend.EMSFireDAC, REST.Backend.ServiceTypes, System.JSON,
  REST.Backend.EMSServices, Vcl.StdCtrls, REST.Client, Data.Bind.Components,
  Data.Bind.ObjectScope, REST.Backend.EndPoint, Vcl.ExtCtrls, Vcl.Grids,
  Vcl.DBGrids, System.Actions, Vcl.ActnList;

type
  TClientForm = class(TForm)
    EMSFireDACClient1: TEMSFireDACClient;
    EMSProvider1: TEMSProvider;
    Button1: TButton;
    DBGrid1: TDBGrid;
    DBGrid2: TDBGrid;
    Panel1: TPanel;
    Button2: TButton;
    ActionList1: TActionList;
    ActionGetTables: TAction;
    ActionPostUpdates: TAction;
    procedure ActionGetTablesExecute(Sender: TObject);
    procedure ActionGetTablesUpdate(Sender: TObject);
    procedure ActionPostUpdatesExecute(Sender: TObject);
    procedure ActionPostUpdatesUpdate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ClientForm: TClientForm;

implementation

{$R *.dfm}

uses ClientModuleU;

procedure TClientForm.ActionGetTablesExecute(Sender: TObject);
begin
  EMSFireDACClient1.GetData;
end;

procedure TClientForm.ActionGetTablesUpdate(Sender: TObject);
begin
  //
end;

procedure TClientForm.ActionPostUpdatesExecute(Sender: TObject);
begin
  EMSFireDACClient1.PostUpdates;

end;

procedure TClientForm.ActionPostUpdatesUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := EMSFireDACClient1.CanPostUpdates;
end;

end.
