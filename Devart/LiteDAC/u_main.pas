unit u_main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Gestures, Data.DB, MemDS, DBAccess,
  FMX.Layouts, FMX.Memo, FMX.Objects, FMX.StdCtrls, FMX.Edit, LiteAccess,
  Data.Bind.EngExt, Fmx.Bind.DBEngExt, System.Rtti, System.Bindings.Outputs,
  Fmx.Bind.Editors, Data.Bind.Components, Data.Bind.DBScope, LiteCall;

type
  TMainForm = class(TForm)
    Expander: TExpander;
    lbDatabase: TLabel;
    lbPassword: TLabel;
    edDatabase: TEdit;
    edPassword: TEdit;
    btnConnect: TButton;
    pMain: TPanel;
    lbFishName: TLabel;
    iPicture: TImage;
    pInfo: TPanel;
    Label1: TLabel;
    lbSpeciesName: TLabel;
    Label2: TLabel;
    lbCategory: TLabel;
    meDescription: TMemo;
    GestureManager: TGestureManager;
    LiteConnection: TLiteConnection;
    LiteQuery: TLiteQuery;
    BindSourceDB1: TBindSourceDB;
    BindingsList1: TBindingsList;
    LinkPropertyToFieldText: TLinkPropertyToField;
    LinkControlToField1: TLinkControlToField;
    LinkPropertyToFieldBitmap: TLinkPropertyToField;
    LinkPropertyToFieldText2: TLinkPropertyToField;
    LinkPropertyToFieldText3: TLinkPropertyToField;
    cbDirect: TCheckBox;
    procedure btnConnectClick(Sender: TObject);
    procedure pMainGesture(Sender: TObject; const EventInfo: TGestureEventInfo;
      var Handled: Boolean);
  private

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

procedure TMainForm.pMainGesture(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  if LiteQuery.Active then
    if (EventInfo.GestureID = sgiLeft) and not LiteQuery.Eof then begin
      LiteQuery.Next;
    end
    else
    if (EventInfo.GestureID = sgiRight) and not LiteQuery.Bof then begin
      LiteQuery.Prior;
    end;
end;

procedure TMainForm.btnConnectClick(Sender: TObject);
begin
  if LiteConnection.Connected then begin
    LiteConnection.Disconnect;
    btnConnect.Text := 'Connect';
  end
  else begin
    LiteConnection.Database :={$IF Defined(IOS) or Defined(ANDROID)}GetHomePath + PathDelim{$IFNDEF ANDROID} + 'Documents'{$ENDIF} + PathDelim + {$ENDIF}edDatabase.Text;
    LiteConnection.EncryptionKey := edPassword.Text;
    LiteConnection.Options.Direct := cbDirect.IsChecked;
    LiteConnection.Connect;
    LiteQuery.Open;
    Expander.IsExpanded := False;
    btnConnect.Text := 'Disconnect';
  end;

end;

end.
