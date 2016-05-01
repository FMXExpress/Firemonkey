//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit NotesClientFormU;

interface


uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, IPPeerClient,
  FMX.ListView.Types, Data.Bind.GenData, Data.Bind.EngExt, Fmx.Bind.DBEngExt,
  System.Rtti, System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.Components,
  FMX.Layouts, FMX.Memo, FMX.StdCtrls, FMX.Edit, Data.Bind.ObjectScope,
  FMX.TabControl, FMX.ListView, REST.Backend.KinveyProvider,
  System.Generics.Collections,REST.Backend.ServiceTypes,
  System.Actions, FMX.ActnList,
  FMX.Controls.Presentation, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, FMX.ScrollBox;

type
  TNotesClientForm = class(TForm)
    ToolBar1: TToolBar;
    TabControl1: TTabControl;
    ListView1: TListView;
    TabItemList: TTabItem;
    TabItemDetails: TTabItem;
    EditTitle: TEdit;
    MemoContent: TMemo;
    ButtonAdd: TButton;
    TabItemAdd: TTabItem;
    MemoAddContent: TMemo;
    EditAddTitle: TEdit;
    ButtonSaveAdd: TButton;
    ButtonCancelAdd: TButton;
    TabItemEdit: TTabItem;
    EditEditTitle: TEdit;
    ButtonEditSave: TButton;
    ButtonEditCancel: TButton;
    MemoEditContent: TMemo;
    ActionList1: TActionList;
    ActionAdd: TAction;
    ActionAddSave: TAction;
    ActionAddCancel: TAction;
    ActionEditSave: TAction;
    ActionEditCancel: TAction;
    ButtonBack: TButton;
    ActionEdit: TAction;
    ActionBack: TAction;
    ButtonDelete: TButton;
    ActionDelete: TAction;
    Label1: TLabel;
    ActionLabel: TAction;
    ActionRefresh: TAction;
    ActionNext: TAction;
    ActionPrior: TAction;
    SpeedButton2: TSpeedButton;
    SpeedButton1: TSpeedButton;
    ButtonEdit: TButton;
    Layout5: TLayout;
    RefreshList: TButton;
    ToolBar4: TToolBar;
    TabItemLogin: TTabItem;
    BindSource1: TPrototypeBindSource;
    BindingsList1: TBindingsList;
    LinkControlToFieldTitle: TLinkControlToField;
    LinkControlToFieldDescription: TLinkControlToField;
    LinkListControlToField1: TLinkListControlToField;
    LinkControlToField1: TLinkControlToField;
    Button1: TButton;
    Button5: TButton;
    ActionLogin: TAction;
    EditPassword: TEdit;
    EditUserName: TEdit;
    ActionLogout: TAction;
    ActionSignup: TAction;
    Label2: TLabel;
    Label3: TLabel;
    Button4: TButton;
    ButtonNext: TButton;
    ActionForward: TAction;
    LayoutTabControl: TLayout;
    ToolBar2: TToolBar;
    LayoutUserName: TLayout;
    LayoutLogout: TLayout;
    Label4: TLabel;
    LabelUserName: TLabel;
    LinkControlToField2: TLinkControlToField;
    LinkControlToField3: TLinkControlToField;
    LinkControlToField4: TLinkControlToField;
    procedure FormCreate(Sender: TObject);
    procedure ActionAddExecute(Sender: TObject);
    procedure ActionAddUpdate(Sender: TObject);
    procedure ActionAddCancelExecute(Sender: TObject);
    procedure ActionAddSaveExecute(Sender: TObject);
    procedure ActionAddSaveUpdate(Sender: TObject);
    procedure ActionEditSaveExecute(Sender: TObject);
    procedure ActionEditCancelExecute(Sender: TObject);
    procedure ActionEditExecute(Sender: TObject);
    procedure ActionEditUpdate(Sender: TObject);
    procedure ActionBackExecute(Sender: TObject);
    procedure ActionBackUpdate(Sender: TObject);
    procedure ActionDeleteExecute(Sender: TObject);
    procedure ActionLabelUpdate(Sender: TObject);
    procedure ActionRefreshExecute(Sender: TObject);
    procedure ActionNextExecute(Sender: TObject);
    procedure ActionNextUpdate(Sender: TObject);
    procedure ActionPriorExecute(Sender: TObject);
    procedure ActionPriorUpdate(Sender: TObject);
    procedure ActionEditSaveUpdate(Sender: TObject);
    procedure ActionLabelExecute(Sender: TObject);
    procedure BindSource1CreateAdapter(Sender: TObject;
      var ABindSourceAdapter: TBindSourceAdapter);
    procedure ActionLoginExecute(Sender: TObject);
    procedure ActionLoginUpdate(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure ActionSignupExecute(Sender: TObject);
    procedure ActionSignupUpdate(Sender: TObject);
    procedure ActionLogoutExecute(Sender: TObject);
    procedure ActionLogoutUpdate(Sender: TObject);
    procedure ActionForwardExecute(Sender: TObject);
    procedure ActionForwardUpdate(Sender: TObject);
    procedure FormVirtualKeyboardHidden(Sender: TObject;
      KeyboardVisible: Boolean; const Bounds: TRect);
    procedure FormVirtualKeyboardShown(Sender: TObject;
      KeyboardVisible: Boolean; const Bounds: TRect);
    procedure ActionAddCancelUpdate(Sender: TObject);
    procedure ActionEditCancelUpdate(Sender: TObject);
    procedure ActionDeleteUpdate(Sender: TObject);
    procedure TabItemLoginClick(Sender: TObject);
    procedure ListView1ItemClick(const Sender: TObject;
      const AItem: TListViewItem);
  public
    type
      TView = (Login, List, Details, Add, Edit);
  private
    FBindSourceAdapter: TBindSourceAdapter;
    FViewStack: TStack<TView>;
    procedure ShowView(AView: TView);
    function CurrentView: TView;
    function GetTitleField: TBindSourceAdapterField;
    procedure PopView;
    procedure PushView(const AView: TView);
    procedure ShowNavigation(const AAction: TAction);
    function GetAdapter: TBindSourceAdapter;
    procedure UpdateLoginPage;
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  NotesClientForm: TNotesClientForm;

{$DEFINE CLOUDDATA}
implementation

{$R *.fmx}

uses NotesAdapterModuleU, NotesClientModuleU, FMX.VirtualKeyboard, FMX.Platform, FMX.DialogService, System.Math;

//uses RESTMobuleClientDataModule;

const
  sBaseURL = 'http://localhost:8080';


function TNotesClientForm.GetAdapter: TBindSourceAdapter;
begin
  Result := Self.BindSource1.InternalAdapter;
end;

procedure TNotesClientForm.UpdateLoginPage;
begin
  if NotesClientModule.LoggedIn then
  begin
    LayoutLogout.Visible := True;
    LayoutUserName.Visible := False;
    LabelUserName.Text := NotesClientModule.LoggedInUserName;
  end
  else
  begin
    LayoutUserName.Visible := True;
    LayoutLogout.Visible := False;
  end;
end;

procedure TNotesClientForm.ActionLoginExecute(Sender: TObject);
begin
  NotesClientModule.Login(EditUserName.Text, EditPassword.Text);
  UpdateLoginPage;
  ListView1.Items.Clear;
  PushView(TView.List);
  NotesAdapterModule.RefreshAdapter;
end;

procedure TNotesClientForm.ActionLoginUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not NotesClientModule.BackendAuth1.LoggedIn;
end;

procedure TNotesClientForm.ActionLogoutExecute(Sender: TObject);
begin
  NotesClientModule.BackendAuth1.Logout;
  UpdateLoginPage;
end;

procedure TNotesClientForm.ActionLogoutUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := NotesClientModule.BackendAuth1.LoggedIn;
end;

constructor TNotesClientForm.Create(AOwner: TComponent);
begin
  inherited;
  FViewStack := TStack<TView>.Create;
end;

function TNotesClientForm.CurrentView: TView;
begin
 if Self.TabControl1.ActiveTab = TabItemAdd then
   Result := TView.Add
 else if Self.TabControl1.ActiveTab = TabItemList then
   Result := TView.List
 else if Self.TabControl1.ActiveTab = TabItemEdit then
   Result := TView.Edit
 else if Self.TabControl1.ActiveTab = TabItemDetails then
   Result := TView.Details
 else if Self.TabControl1.ActiveTab = TabItemLogin then
   Result := TView.Login
 else
   raise Exception.Create('Unexpected');
end;

destructor TNotesClientForm.Destroy;
begin
  FViewStack.Free;
  inherited;
end;

procedure TNotesClientForm.PushView(const AView: TView);
begin
  FViewStack.Push(CurrentView);
  ShowView(AView);
end;

procedure TNotesClientForm.PopView;
begin
  if FViewStack.Count > 0 then
    ShowView(FViewStack.Pop);
end;

procedure TNotesClientForm.FormCreate(Sender: TObject);
var
  FService: IFMXVirtualKeyboardToolbarService;
begin
  ShowView(TView.Login);
  TabControl1.TabPosition := TTabPosition.None;
  LayoutLogout.Visible := False;
  if TPlatformServices.Current.SupportsPlatformService(IFMXVirtualKeyboardToolbarService, FService) then
  begin
    FService.SetToolbarEnabled(False);
    FService.SetHideKeyboardButtonVisibility(False);
  end;
  //VertScrollBox1.OnCalcContentBounds := CalcContentBoundsProc;
end;

resourcestring
  sFieldNotFound = 'Field not found.';

function TNotesClientForm.GetTitleField: TBindSourceAdapterField;
begin
  Result := GetAdapter.FindField('Title');
  if Result = nil then
    raise Exception.Create(sFieldNotFound);
end;

procedure TNotesClientForm.ListView1ItemClick(const Sender: TObject;
  const AItem: TListViewItem);
begin
  PushView(TView.Details);
end;

procedure TNotesClientForm.ActionAddCancelExecute(Sender: TObject);
begin
  PopView;
  if GetAdapter.State in seEditModes then
  begin
    GetAdapter.Cancel;
    GetAdapter.Delete;
  end;
end;

procedure TNotesClientForm.ActionAddCancelUpdate(Sender: TObject);
begin
  (Sender as TAction).Visible := CurrentView = TView.Add;
end;

procedure TNotesClientForm.ActionAddExecute(Sender: TObject);
begin
  PushView(TView.Add);
  GetAdapter.Insert;
end;


procedure TNotesClientForm.ActionAddSaveExecute(Sender: TObject);
begin
  GetAdapter.Post;
  TDialogService.MessageDialog(Format('"%s" saved', [GetTitleField.GetTValue.ToString]),
    TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0, nil);

  PopView;
end;

procedure TNotesClientForm.ActionAddSaveUpdate(Sender: TObject);
begin
  (Sender as TAction).Visible := CurrentView = TView.Add;
end;

procedure TNotesClientForm.ActionAddUpdate(Sender: TObject);
begin
  case CurrentView of
    TView.List:
      (Sender as TAction).Visible := True;
  else
      (Sender as TAction).Visible := False;
  end;
end;

procedure TNotesClientForm.ActionBackExecute(Sender: TObject);
begin
  if CurrentView = TView.List then
    ShowView(TView.Login)
  else if FViewStack.Count > 0 then
    PopView
  else
    ShowView(TView.List);
end;

procedure TNotesClientForm.ActionBackUpdate(Sender: TObject);
begin
  case CurrentView of
    TView.List:
    begin
      (Sender as TAction).Text := 'Login';
      (Sender as TAction).Visible := True;
    end;
    TView.Details:
    begin
      (Sender as TAction).Text := 'List';
      (Sender as TAction).Visible := True;
    end
  else
    (Sender as TAction).Visible := False
  end;

end;

procedure TNotesClientForm.ActionDeleteExecute(Sender: TObject);
var
  LTitle: string;
begin
  LTitle := GetTitleField.GetTValue.ToString;
  TDialogService.MessageDialog(Format('Delete "%s"?', [LTitle]), TMsgDlgType.mtConfirmation, mbOKCancel,
    TMsgDlgBtn.mbOK, 0,
    procedure(const AResult: TModalResult)
    begin
      if AResult <> mrCancel then
      begin
        FBindSourceAdapter.Delete;
        if FBindSourceAdapter.ItemIndex = -1 then
          // No more records
          ShowView(TView.List);
      end;
    end);
end;

procedure TNotesClientForm.ActionDeleteUpdate(Sender: TObject);
begin
  (Sender as TAction).Visible := CurrentView = TView.Details;
end;

procedure TNotesClientForm.ActionEditCancelExecute(Sender: TObject);
begin
  TDialogService.MessageDialog('Cancel changes?', TMsgDlgType.mtConfirmation, mbOKCancel, TMsgDlgBtn.mbOK, 0,
    procedure(const AResult: TModalResult)
    begin
      if (not FBindSourceAdapter.Modified) or (AResult <> mrCancel) then
      begin
        FBindSourceAdapter.Cancel;
        PopView;
      end;
    end);
end;

procedure TNotesClientForm.ActionEditCancelUpdate(Sender: TObject);
begin
  (Sender as TAction).Visible := CurrentView = TView.Edit;
end;

procedure TNotesClientForm.ActionEditExecute(Sender: TObject);
begin
  PushView(TView.Edit);
end;

procedure TNotesClientForm.ActionEditSaveExecute(Sender: TObject);
begin
  FBindSourceAdapter.Post;
  TDialogService.MessageDialog(Format('"%s" saved', [GetTitleField.GetTValue.ToString]), TMsgDlgType.mtInformation,
    [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0, nil);
  PopView;
end;

procedure TNotesClientForm.ActionEditSaveUpdate(Sender: TObject);
begin
  (Sender as TAction).Visible := CurrentView = TView.Edit;
  (Sender as TAction).Enabled := GetAdapter.Modified;
end;

procedure TNotesClientForm.ActionEditUpdate(Sender: TObject);
begin
  case CurrentView of
    TView.Details:
      (Sender as TAction).Visible := True;
  else
      (Sender as TAction).Visible := False;
  end;
end;

procedure TNotesClientForm.ActionForwardExecute(Sender: TObject);
begin
  ShowView(TView.List);
end;

procedure TNotesClientForm.ActionForwardUpdate(Sender: TObject);
begin
  TAction(Sender).Visible := (CurrentView = TView.Login) and
    NotesClientModule.LoggedIn;
  if  TAction(Sender).Visible then
    TAction(Sender).Text := 'List';
end;

procedure TNotesClientForm.ActionLabelExecute(Sender: TObject);
begin
//
end;

procedure TNotesClientForm.ActionLabelUpdate(Sender: TObject);
begin
  case CurrentView of
    TView.List: (Sender as TAction).Text := 'Notes';
    TView.Details: (Sender as TAction).Text := 'Note';
    TView.Add: (Sender as TAction).Text := 'Add Note';
    TView.Edit: (Sender as TAction).Text := 'Edit Note';
    TView.Login: (Sender as TAction).Text := 'Login';
  end;
end;

procedure TNotesClientForm.ShowNavigation(const AAction: TAction);
begin
  case CurrentView of
    TView.Details:  AAction.Visible := True;
  else
    AAction.Visible := False;
  end;
end;

procedure TNotesClientForm.ActionNextExecute(Sender: TObject);
begin
  BindSource1.Next;
end;

procedure TNotesClientForm.ActionNextUpdate(Sender: TObject);
begin
  ShowNavigation(Sender as TAction);
  if TAction(Sender).Visible then
    TAction(Sender).Enabled := not BindSource1.Eof;
end;

procedure TNotesClientForm.ActionPriorExecute(Sender: TObject);
begin
  BindSource1.Prior;
end;

procedure TNotesClientForm.ActionPriorUpdate(Sender: TObject);
begin
  ShowNavigation(Sender as TAction);
  if TAction(Sender).Visible then
    TAction(Sender).Enabled := not BindSource1.BOF;
end;

procedure TNotesClientForm.ActionRefreshExecute(Sender: TObject);
begin
  NotesAdapterModule.RefreshAdapter;
end;

procedure TNotesClientForm.ActionSignupExecute(Sender: TObject);
begin
  NotesClientModule.Signup(EditUserName.Text, EditPassword.Text);
  UpdateLoginPage;
  ListView1.Items.Clear;
  PushView(TView.List);
  NotesAdapterModule.RefreshAdapter;
end;

procedure TNotesClientForm.ActionSignupUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not NotesClientModule.BackendAuth1.LoggedIn;
end;

procedure TNotesClientForm.BindSource1CreateAdapter(Sender: TObject;
  var ABindSourceAdapter: TBindSourceAdapter);
begin
  Assert(FBindSourceAdapter = nil);
  FBindSourceAdapter := BindSource1.InternalAdapter;
{$IFDEF SAMPLEDATA}
  FBindSourceAdapter := LoadRecipesSampleData;
  ABindSourceAdapter := FBindSourceAdapter;
{$ELSE}
{$IFDEF CLOUDDATA}
  FBindSourceAdapter := NotesAdapterModule.BindSourceAdapter;
  ABindSourceAdapter := FBindSourceAdapter;
{$ENDIF}
{$ENDIF}
end;

procedure TNotesClientForm.Button6Click(Sender: TObject);
var
  LUserName: string;
  LPassword: string;
begin
  TLoginCredentialService.GetLoginCredentials('',
    function(const Username, Password, Domain: string): Boolean
    begin
      Result := True;
      LUserName := Username;
      LPassword := Password;
//      if Assigned(LTestConnectionMethod) then
//        LTestConnectionMethod;
    end);
end;

procedure TNotesClientForm.ShowView(AView: TView);
begin
  case AView of
    List:    Self.TabControl1.ActiveTab := TabItemList;
    Details: Self.TabControl1.ActiveTab := TabItemDetails;
    Add:     Self.TabControl1.ActiveTab := TabItemAdd;
    Edit:    Self.TabControl1.ActiveTab := TabItemEdit;
    Login:   Self.TabControl1.ActiveTab := TabItemLogin;
  else
    raise Exception.Create('Unexpected');
  end;
end;


procedure TNotesClientForm.TabItemLoginClick(Sender: TObject);
begin

end;

//procedure TNotesClientForm.FormCreate(Sender: TObject);
//begin
//  if TPlatformServices.Current.SupportsPlatformService(IFMXVirtualKeyboardToolbarService, IInterface(FService1)) then
//  begin
//    FService1.SetToolbarEnabled(True);
//    FService1.SetHideKeyboardButtonVisibility(True);
//  end;
//  VertScrollBox1.OnCalcContentBounds := CalcContentBoundsProc;
//end;

procedure TNotesClientForm.FormVirtualKeyboardHidden(Sender: TObject;
  KeyboardVisible: Boolean; const Bounds: TRect);
begin
  TabControl1.Align := TAlignLayout.Client;
end;

procedure TNotesClientForm.FormVirtualKeyboardShown(Sender: TObject;
  KeyboardVisible: Boolean; const Bounds: TRect);
begin
  TabControl1.Align := TAlignLayout.Horizontal;
  TabControl1.Height := LayoutTabControl.Height - Bounds.Height;
end;

end.
