unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, IPPeerClient, REST.Backend.ServiceTypes,
  REST.Backend.MetaTypes, System.JSON, System.Rtti, System.Bindings.Outputs,
  Vcl.Bind.Editors, Data.Bind.EngExt, Vcl.Bind.DBEngExt, Vcl.StdCtrls,
  Data.Bind.Components, Data.Bind.ObjectScope, REST.Backend.BindSource,
  REST.Backend.ServiceComponents, REST.Backend.KinveyServices, Vcl.ExtCtrls,
  REST.Backend.Providers, Vcl.ComCtrls, REST.Backend.ParseServices;

type
  TForm1 = class(TForm)
    BackendQuery1: TBackendQuery;
    MemoJSONResult: TMemo;
    LinkControlToFieldJSONResult: TLinkControlToField;
    BindingsList1: TBindingsList;
    BackendStorage1: TBackendStorage;
    PageControl1: TPageControl;
    tabFetch: TTabSheet;
    tabAdd: TTabSheet;
    tabDelete: TTabSheet;
    Button1: TButton;
    MemoQueryStrings: TMemo;
    Button2: TButton;
    edtPhone: TLabeledEdit;
    edtName: TLabeledEdit;
    Label1: TLabel;
    btnDelete: TButton;
    edtDeleteObjectID: TLabeledEdit;
    LinkControlToField3: TLinkControlToField;
    tabSettings: TTabSheet;
    EditBackendClassName: TLabeledEdit;
    ComboBoxBackendService: TComboBox;
    Label3: TLabel;
    LinkControlToField1: TLinkControlToField;
    LinkFillControlToField1: TLinkFillControlToField;
    ListView1: TListView;
    Panel1: TPanel;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


  TCustomer = class
  strict private
    FName: string;
    FPhone: string;
    FFoo: string;
  published
    property Phone : string read FPhone write FPhone;
    property Name : string read FName write FName;
    property Foo : string read FFoo write FFoo;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses Unit2;

procedure TForm1.btnDeleteClick(Sender: TObject);
begin
  BackendStorage1.Storage.DeleteObject(EditBackendClassName.Text,edtDeleteObjectID.Text)
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  Customer : TCustomer;
  LVI: TListItem;
  Customers: TBackendObjectList<TCustomer>;
  I: Integer;
  QueryStr: TArray<string>;
begin
  BackendQuery1.Execute;

  // OR for objects you can get the objects back using the following code. //
  Customers := TBackendObjectList<TCustomer>.Create;
  try
    QueryStr := TArray<string>.Create(MemoQueryStrings.Lines.Text);
    BackendStorage1.Storage.QueryObjects<TCustomer>('Customer',QueryStr,Customers);

    ListView1.Items.Clear;
    for I := 0 to Customers.Count -1 do begin
      Customer := Customers.Items[I];
      LVI := ListView1.Items.Add;
      LVI.Caption := Customer.Name;
      LVI.SubItems.Add(Customer.Phone);
      LVI.SubItems.Add(Customers.EntityValues[Customer].ObjectID);
    end;
  finally
    Customers.Free;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
Var
  LValue : TBackendEntityValue;
  LCustomer : TCustomer;
begin
  LCustomer := TCustomer.Create;
  try
    LCustomer.Name := edtName.Text;
    LCustomer.Phone := edtPhone.Text;
    LCustomer.Foo := 'Foo';
    BackendStorage1.Storage.CreateObject<TCustomer>(EditBackendClassName.Text,LCustomer, LValue);

    edtDeleteObjectID.Text := LValue.ObjectID;
  finally
    LCustomer.Free;
  end;
end;

end.
