// Copyright (c) 2017 Embarcadero Technologies, Inc. All rights reserved.  
//
// This software is the copyrighted property of Embarcadero Technologies, Inc. 
// ("Embarcadero") and its licensors. You may only use this software if you 
// are an authorized licensee of Delphi, C++Builder or RAD Studio 
// (the "Embarcadero Products").  This software is subject to Embarcadero's 
// standard software license and support agreement that accompanied your 
// purchase of the Embarcadero Products and is considered a Redistributable, 
// as such term is defined thereunder. Your use of this software constitutes 
// your acknowledgement of your agreement to the foregoing software license 
// and support agreement. 
//---------------------------------------------------------------------------
unit DialogItem;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ScrollBox,
  FMX.Memo, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Edit, FMX.Layouts,
  ClientDataModule, System.Rtti, System.Bindings.Outputs, Fmx.Bind.Editors,
  Data.Bind.EngExt, Fmx.Bind.DBEngExt, Data.Bind.Components, Data.Bind.DBScope,
  FMX.EditBox, FMX.SpinBox;

type
  TDlgItem = class(TForm)
    GridPanelLayout1: TGridPanelLayout;
    EditBarcode: TEdit;
    EditItemName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    MemoDescription: TMemo;
    BindSourceDB1: TBindSourceDB;
    BindingsList1: TBindingsList;
    LinkControlToField2: TLinkControlToField;
    LinkControlToField3: TLinkControlToField;
    BindSourceDB2: TBindSourceDB;
    LinkControlToField1: TLinkControlToField;
    ButtonClose: TButton;
    Label4: TLabel;
    SpinBoxQTY: TSpinBox;
    LinkControlToField4: TLinkControlToField;
    procedure FormCreate(Sender: TObject);
    procedure SpinBoxQTYChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    class function ShowItemDialog: TModalResult;
  end;

implementation

{$R *.fmx}

class function TDlgItem.ShowItemDialog: TModalResult;
var
  LItem: TDlgItem;
begin
  LItem := TDlgItem.Create(nil);
  try
    Result := LItem.ShowModal;
  finally
    LItem.Free;
  end;
end;

procedure TDlgItem.SpinBoxQTYChange(Sender: TObject);
begin
  TLinkObservers.ControlChanged(SpinBoxQTY);
end;

procedure TDlgItem.FormCreate(Sender: TObject);
var
  LReadOnlyMode: Boolean;
begin
  LReadOnlyMode := not ClientDM.memCurrentUserHasWriteAccess.AsBoolean;
  
  EditBarcode.ReadOnly := LReadOnlyMode;
  EditItemName.ReadOnly := LReadOnlyMode;
  MemoDescription.ReadOnly := LReadOnlyMode;
  SpinBoxQTY.ReadOnly := LReadOnlyMode;
  if LReadOnlyMode then
  begin
    ButtonClose.Text := 'Close';
    ButtonClose.ModalResult := mrCancel;
  end;;  
end;

end.
