//---------------------------------------------------------------------------

// This software is Copyright (c) 2014 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit BaaS_ToDoForm;

interface


uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, IPPeerClient,
  FMX.ListView.Types, Data.Bind.GenData, Data.Bind.EngExt, Fmx.Bind.DBEngExt,
  System.Rtti, System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.Components,
  FMX.Layouts, FMX.Memo, FMX.StdCtrls, FMX.Edit, Data.Bind.ObjectScope,
  FMX.TabControl, FMX.ListView, REST.Backend.KinveyProvider,
  System.Generics.Collections,REST.Backend.ServiceTypes,
  System.Actions, FMX.ActnList, FMX.Controls.Presentation;

type
  TBaaSToDoList = class(TForm)
    ToolBar1: TToolBar;
    TabControl1: TTabControl;
    ListView1: TListView;
    TabItemList: TTabItem;
    TabItemDetails: TTabItem;
    PrototypeBindSource1: TPrototypeBindSource;
    BindingsList1: TBindingsList;
    EditTitle: TEdit;
    LinkControlToFieldTitle: TLinkControlToField;
    MemoDescription: TMemo;
    LinkControlToFieldDescription: TLinkControlToField;
    LinkListControlToField1: TLinkListControlToField;
    ButtonAdd: TButton;
    TabItemAdd: TTabItem;
    MemoAddDescription: TMemo;
    EditAddTitle: TEdit;
    ButtonSaveAdd: TButton;
    Layout1: TLayout;
    ButtonCancelAdd: TButton;
    TabItemEdit: TTabItem;
    EditEditTitle: TEdit;
    Layout2: TLayout;
    ButtonEditSave: TButton;
    ButtonEditCancel: TButton;
    MemoEditContent: TMemo;
    LinkControlToField1: TLinkControlToField;
    LinkControlToField2: TLinkControlToField;
    ActionList1: TActionList;
    ActionAdd: TAction;
    ActionAddSave: TAction;
    ActionAddCancel: TAction;
    ActionEditSave: TAction;
    ActionEditCancel: TAction;
    ButtonBack: TButton;
    ActionEdit: TAction;
    ActionBack: TAction;
    Layout3: TLayout;
    Button2: TButton;
    ActionDelete: TAction;
    Label1: TLabel;
    ActionLabel: TAction;
    ActionRefresh: TAction;
    ActionNext: TAction;
    ActionPrior: TAction;
    SpeedButton2: TSpeedButton;
    SpeedButton1: TSpeedButton;
    Button3: TButton;
    Layout5: TLayout;
    RefreshList: TButton;
    BottomToolbar: TToolBar;
    ToolBar2: TToolBar;
    ToolBar3: TToolBar;
    ToolBar4: TToolBar;
    procedure ListView1ItemClick(const Sender: TObject;
      const AItem: TListViewItem);
    procedure PrototypeBindSource1CreateAdapter(Sender: TObject;
      var ABindSourceAdapter: TBindSourceAdapter);
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
  public
    type
      TView = (List, Details, Add, Edit);
  private
    FBindSourceAdapter: TBindSourceAdapter;
    FViewStack: TStack<TView>;
    procedure ShowView(AView: TView);
    function CurrentView: TView;
    procedure AddItem(const ATitle, AContent: string);
    function GetAdapter: TBindSourceAdapter;
    function GetTitleField: TBindSourceAdapterField;
    function GetContentField: TBindSourceAdapterField;
    procedure PopView;
    procedure PushView(const AView: TView);
    procedure ShowNavigation(const AAction: TAction);
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  BaaSToDoList: TBaaSToDoList;

implementation

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}

uses DataModuleUnit1, ToDoItemTypes;

procedure TBaaSToDoList.ListView1ItemClick(const Sender: TObject;
  const AItem: TListViewItem);
begin
  PushView(TView.Details);
end;

procedure TBaaSToDoList.PrototypeBindSource1CreateAdapter(Sender: TObject;
  var ABindSourceAdapter: TBindSourceAdapter);
begin
  Assert(FBindSourceAdapter = nil);
  FBindSourceAdapter := DataModule1.ItemAdapter;
  ABindSourceAdapter := FBindSourceAdapter;
end;

constructor TBaaSToDoList.Create(AOwner: TComponent);
begin
  inherited;
  FViewStack := TStack<TView>.Create;
end;

function TBaaSToDoList.CurrentView: TView;
begin
 if Self.TabControl1.ActiveTab = TabItemAdd then
   Result := TView.Add
 else if Self.TabControl1.ActiveTab = TabItemList then
   Result := TView.List
 else if Self.TabControl1.ActiveTab = TabItemEdit then
   Result := TView.Edit
 else if Self.TabControl1.ActiveTab = TabItemDetails then
   Result := TView.Details
 else
   raise Exception.Create('Unexpected');
end;

destructor TBaaSToDoList.Destroy;
begin
  FViewStack.Free;
  inherited;
end;

procedure TBaaSToDoList.PushView(const AView: TView);
begin
  FViewStack.Push(CurrentView);
  ShowView(AView);
end;

procedure TBaaSToDoList.PopView;
begin
  if FViewStack.Count > 0 then
    ShowView(FViewStack.Pop);
end;

procedure TBaaSToDoList.FormCreate(Sender: TObject);
begin
  TabControl1.ActiveTab := TabItemList;
  TabControl1.TabPosition := TTabPosition.None;
  DataModule1.RefreshAdapter;
  DataModule1.ItemAdapter.Active := True;
end;

function TBaaSToDoList.GetAdapter: TBindSourceAdapter;
begin
  Result := Self.PrototypeBindSource1.InternalAdapter;
end;

function TBaaSToDoList.GetContentField: TBindSourceAdapterField;
begin
  Result := GetAdapter.FindField(TToDoNames.ContentProperty);
  if Result = nil then
    raise Exception.Create('Field not found');
end;

function TBaaSToDoList.GetTitleField: TBindSourceAdapterField;
begin
  Result := GetAdapter.FindField(TToDoNames.TitleProperty);
  if Result = nil then
    raise Exception.Create('Field not found');
end;

procedure TBaaSToDoList.ActionAddCancelExecute(Sender: TObject);
begin
  PopView;
  EditAddTitle.Text := '';
  MemoAddDescription.Text := '';
end;

procedure TBaaSToDoList.ActionAddExecute(Sender: TObject);
begin
  PushView(TView.Add);
end;

procedure TBaaSToDoList.ActionAddSaveExecute(Sender: TObject);
begin
  AddItem(EditAddTitle.Text, MemoAddDescription.Text);
  MessageDlg(Format('"%s" added', [GetTitleField.GetTValue.ToString]),
    TMsgDlgType.mtInformation,
    [TMsgDlgBtn.mbOK],
    0,
    procedure(const AResult: TModalResult)
      begin

      end
  );
  PopView;
  EditAddTitle.Text := '';
  MemoAddDescription.Text := '';
end;

procedure TBaaSToDoList.ActionAddSaveUpdate(Sender: TObject);
begin
  //
end;

procedure TBaaSToDoList.ActionAddUpdate(Sender: TObject);
begin
  case CurrentView of
    TView.List:
      (Sender as TAction).Visible := True;
  else
      (Sender as TAction).Visible := False;
  end;
end;

procedure TBaaSToDoList.ActionBackExecute(Sender: TObject);
begin
  if FViewStack.Count > 0 then
    PopView
  else
    ShowView(TView.List);
end;

procedure TBaaSToDoList.ActionBackUpdate(Sender: TObject);
begin
  case CurrentView of
    TView.Details:
    begin
      (Sender as TAction).Text := 'List';
      (Sender as TAction).Visible := True;
    end
  else
    (Sender as TAction).Visible := False
  end;

end;

procedure TBaaSToDoList.ActionDeleteExecute(Sender: TObject);
var
  LTitle: string;
begin
  LTitle := GetTitleField.GetTValue.ToString;
  MessageDlg(Format('Delete "%s"?', [LTitle]), TMsgDlgType.mtConfirmation, mbOKCancel, 0,
    procedure(const AResult: TModalResult)
    begin
      if AResult = mrOk then
      begin
        FBindSourceAdapter.Delete;
        MessageDlg(Format('"%s" deleted', [LTitle]), TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0, nil);
        if FBindSourceAdapter.ItemIndex = -1 then
          // No more records
          ShowView(TView.List);
      end;
    end);
end;

procedure TBaaSToDoList.ActionEditCancelExecute(Sender: TObject);
begin
  if FBindSourceAdapter.Modified then
  begin
    MessageDlg('Cancel changes?', TMsgDlgType.mtConfirmation, mbOKCancel, 0,
      procedure(const AResult: TModalResult)
      begin
        if AResult = mrOk then
        begin
          FBindSourceAdapter.Cancel;
          PopView;
        end;
      end);
  end
  else
  begin
    FBindSourceAdapter.Cancel;
    PopView;
  end;
end;

procedure TBaaSToDoList.ActionEditExecute(Sender: TObject);
begin
  PushView(TView.Edit);
end;

procedure TBaaSToDoList.ActionEditSaveExecute(Sender: TObject);
begin
  FBindSourceAdapter.Post;
  MessageDlg(Format('"%s" saved', [GetTitleField.GetTValue.ToString]), TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0, nil);
  PopView;
end;

procedure TBaaSToDoList.ActionEditSaveUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := GetAdapter.Modified;
end;

procedure TBaaSToDoList.ActionEditUpdate(Sender: TObject);
begin
  case CurrentView of
    TView.Details:
      (Sender as TAction).Visible := True;
  else
      (Sender as TAction).Visible := False;
  end;
end;

procedure TBaaSToDoList.ActionLabelUpdate(Sender: TObject);
begin
  case CurrentView of
    TView.List: (Sender as TAction).Text := 'To Do List';
    TView.Details: (Sender as TAction).Text := 'To Do Item';
    TView.Add: (Sender as TAction).Text := 'Add To Do Item';
    TView.Edit: (Sender as TAction).Text := 'Edit To Do Item';
  end;
end;

procedure TBaaSToDoList.ShowNavigation(const AAction: TAction);
begin
  case CurrentView of
    TView.Details:  AAction.Visible := True;
  else
    AAction.Visible := False;
  end;
end;

procedure TBaaSToDoList.ActionNextExecute(Sender: TObject);
begin
  GetAdapter.Next;
end;

procedure TBaaSToDoList.ActionNextUpdate(Sender: TObject);
begin
  ShowNavigation(Sender as TAction);
  if TAction(Sender).Visible then
    TAction(Sender).Enabled := not GetAdapter.Eof;
end;

procedure TBaaSToDoList.ActionPriorExecute(Sender: TObject);
begin
  GetAdapter.Prior;
end;

procedure TBaaSToDoList.ActionPriorUpdate(Sender: TObject);
begin
  ShowNavigation(Sender as TAction);
  if TAction(Sender).Visible then
    TAction(Sender).Enabled := not GetAdapter.BOF;
end;

procedure TBaaSToDoList.ActionRefreshExecute(Sender: TObject);
begin
  DataModule1.RefreshAdapter;
  DataModule1.ItemAdapter.Active := True;
end;

procedure TBaaSToDoList.AddItem(const ATitle, AContent: string);
var
  LAdapter: TBindSourceAdapter;
begin
  LAdapter := GetAdapter;
  LAdapter.Append;
  GetTitleField.SetTValue(ATitle);
  GetContentField.SetTValue(AContent);
  LAdapter.Post;
end;

procedure TBaaSToDoList.ShowView(AView: TView);
begin
  case AView of
    List:    Self.TabControl1.ActiveTab := TabItemList;
    Details: Self.TabControl1.ActiveTab := TabItemDetails;
    Add:     Self.TabControl1.ActiveTab := TabItemAdd;
    Edit:    Self.TabControl1.ActiveTab := TabItemEdit;
  else
    raise Exception.Create('Unexpected');
  end;
end;

end.
