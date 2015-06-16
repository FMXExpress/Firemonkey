unit FormMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.TabControl,
  Data.DBXDataSnap, IndyPeerImpl, Data.DBXCommon, Datasnap.DBClient,
  Datasnap.DSConnect, Data.DB, Data.SqlExpr, Data.Bind.EngExt,
  FMX.Bind.DBEngExt, Data.Bind.Components, FMX.Layouts, FMX.Grid,
  FMX.Bind.Editors, Data.Bind.DBScope, Data.Bind.DBLinks,
  FMX.Bind.DBLinks, FMX.ListBox, FMX.Edit, FMX.Bind.Navigator, System.Rtti,
  System.Bindings.Outputs, FMX.Types3D, FMX.Objects3D, FMX.Ani,
  FMX.MaterialSources, FMX.Controls3D, FMX.Viewport3D, FMX.StdCtrls;

type
  TFrmMain = class(TForm)
    CustomerProvider: TDSProviderConnection;
    CustomerCDS: TClientDataSet;
    DSCustomer: TDataSource;
    CustomerCDSEMP_NO: TSmallintField;
    CustomerCDSFIRST_NAME: TStringField;
    CustomerCDSLAST_NAME: TStringField;
    CustomerCDSPHONE_EXT: TStringField;
    CustomerCDSHIRE_DATE: TSQLTimeStampField;
    CustomerCDSDEPT_NO: TStringField;
    CustomerCDSJOB_CODE: TStringField;
    CustomerCDSJOB_GRADE: TSmallintField;
    CustomerCDSJOB_COUNTRY: TStringField;
    CustomerCDSSALARY: TFMTBCDField;
    CustomerCDSFULL_NAME: TStringField;
    StringGrid1: TStringGrid;
    BindingsList1: TBindingsList;
    DBLinkStringGrid11: TBindDBGridLink;
    DBScope: TBindScopeDB;
    Edit1: TEdit;
    DBLinkEdit1EMP_NO1: TBindDBEditLink;
    BindLinkEdit11: TBindLink;
    BindNavigator1: TBindNavigator;
    Edit2: TEdit;
    DBLinkEdit2FIRST_NAME1: TBindDBEditLink;
    Edit3: TEdit;
    DBLinkEdit3LAST_NAME1: TBindDBEditLink;
    lbFirstName: TLabel;
    BindExpression1: TBindExpression;
    lbLastName: TLabel;
    BindExpressionLabel11: TBindExpression;
    lbEmployeeID: TLabel;
    BindExpressionLabel12: TBindExpression;
    Viewport3D1: TViewport3D;
    Text3D1: TText3D;
    FloatAnimation1: TFloatAnimation;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.fmx}

uses Login, DMMain;

procedure TFrmMain.FormCreate(Sender: TObject);
begin

  DM := TDM.Create(Self);
  FrmLogin := TFrmLogin.Create(Self);
  if not FrmLogin.DoLogin(DM) then
    Application.Terminate
  else
  begin

    case TOSVersion.Platform of
      pfWindows:
        begin

          case TOSVersion.Architecture of
            arIntelX86:
              Text3D1.Text := TOSVersion.Name + ' (Win32)';
            arIntelX64:
              Text3D1.Text := TOSVersion.Name + ' (Win64)';
          end;

        end;
      pfMacOS:
        Text3D1.Text := TOSVersion.Name;

    end;
    DSCustomer.DataSet.Open;
  end;

end;

end.
