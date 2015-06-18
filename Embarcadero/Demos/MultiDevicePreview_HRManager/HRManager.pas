unit HRManager;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.ListBox, FMX.Layouts, FMX.Objects, FMX.TabControl, FMX.Memo, FMX.MultiView,
  FMX.ListView.Types, Data.Bind.GenData, Fmx.Bind.GenData, System.Rtti,
  System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.EngExt,
  Fmx.Bind.DBEngExt, Data.Bind.Components, Data.Bind.ObjectScope, FMX.ListView,
  FMX.Controls.Presentation, FMX.Edit, IPPeerClient, REST.Backend.ServiceTypes,
  REST.Backend.MetaTypes, System.JSON, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  REST.Backend.Providers, REST.Backend.ServiceComponents,
  REST.Backend.BindSource, FireDAC.Stan.StorageBin, Datasnap.DBClient,
  Data.Bind.DBScope, Data.Bind.Grid, FMX.ScrollBox;

type
  THRManagerForm = class(TForm)
    EmployeesTabToolbar: TToolBar;
    EmployeeProfile: TLabel;
    RefreshBtn: TSpeedButton;
    MultiViewBtn: TSpeedButton;
    EmploymentHistoryList: TListBox;
    ListBoxGroupHeader1: TListBoxGroupHeader;
    Position1: TListBoxItem;
    Position2: TListBoxItem;
    Namebadge: TButton;
    EmployeeName: TLabel;
    EmployeeTitle: TLabel;
    HireDate: TLabel;
    EmployeeBadgePhoto: TImage;
    TabControl1: TTabControl;
    Employees: TTabItem;
    Files: TTabItem;
    Search: TTabItem;
    Recent: TTabItem;
    EmployeeRecordNotes: TMemo;
    MultiView1: TMultiView;
    LeftToolbar: TToolBar;
    ToolbarTitle: TLabel;
    TabletHRList: TListBox;
    HRPanelHeader: TListBoxGroupHeader;
    DirectDepositItem: TListBoxItem;
    DirectDepositSwitch: TSwitch;
    PayrollInfoHeader: TListBoxGroupHeader;
    FileAttachmentsHeader: TListBoxGroupHeader;
    UploadListItem: TListBoxItem;
    Edit1: TEdit;
    UploadIcon: TImage;
    EmployeeListview: TListView;
    BindingsList1: TBindingsList;
    FDMemTable1: TFDMemTable;
    UploadBtn: TSpeedButton;
    BindSourceDB1: TBindSourceDB;
    LinkListControlToField1: TLinkListControlToField;
    LinkPropertyToFieldItemDataText: TLinkPropertyToField;
    LinkPropertyToFieldItemDataDetail: TLinkPropertyToField;
    LinkPropertyToFieldItemDataText2: TLinkPropertyToField;
    LinkPropertyToFieldItemDataDetail2: TLinkPropertyToField;
    LinkControlToField1: TLinkControlToField;
    LinkPropertyToFieldText: TLinkPropertyToField;
    LinkPropertyToFieldText2: TLinkPropertyToField;
    LinkPropertyToFieldText3: TLinkPropertyToField;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  HRManagerForm: THRManagerForm;

implementation

{$R *.fmx}
{$R *.iPhone47in.fmx IOS}
{$R *.iPad.fmx IOS}
{$R *.NmXhdpiPh.fmx ANDROID}
{$R *.XLgXhdpiTb.fmx ANDROID}
{$R *.Windows.fmx MSWINDOWS}
{$R *.iPhone55in.fmx IOS}

end.
