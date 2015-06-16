unit UDemo;

interface

uses
  SysUtils, Types, UITypes, Classes, Variants,
  Data.Bind.EngExt, Fmx.Bind.Editors, FMX.Forms, System.Rtti,
  System.Bindings.Outputs, Data.Bind.Components, FMX.TMSTableView,
  Fmx.Bind.DBLinks, Data.Bind.DBLinks, Data.Bind.DBScope, Data.DB,
  Datasnap.DBClient, FMX.Layouts, Fmx.Bind.Navigator, FMX.Controls, FMX.Memo,
  FMX.Types, FMX.TMSBaseControl, Fmx.Bind.DBEngExt,
  FMX.TMSTableViewBinding, FMX.StdCtrls;

type
  TForm550 = class(TForm)
    DataSource1: TDataSource;
    BindScopeDB1: TBindScopeDB;
    TMSFMXTableView1: TTMSFMXTableView;
    BindingsList1: TBindingsList;
    Panel1: TPanel;
    DBLinkImageControl1Graphic1: TBindDBImageLink;
    Label1: TLabel;
    DBLinkLabel2SpeciesNo1: TBindDBTextLink;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    DBLinkLabel12Category1: TBindDBTextLink;
    DBLinkLabel10Common_Name1: TBindDBTextLink;
    DBLinkLabel6SpeciesName1: TBindDBTextLink;
    DBLinkLabel4Lengthcm1: TBindDBTextLink;
    Memo1: TMemo;
    Label2: TLabel;
    DBLinkMemo1Notes1: TBindDBMemoLink;
    ImageControl1: TImageControl;
    BindNavigator1: TBindNavigator;
    TMSFMXBindDBTableViewLink1: TTMSFMXBindDBTableViewLink;
    ClientDataSet1: TClientDataSet;
    procedure TMSFMXTableView1ItemCustomize(Sender: TObject;
      AItem: TTMSFMXTableViewItem; AItemShape: TTMSFMXTableViewItemShape;
      AItemControlShape: TControl);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form550: TForm550;

implementation

{$R *.fmx}

procedure TForm550.FormCreate(Sender: TObject);
begin
  ClientDataSet1.LoadFromFile(Extractfilepath(paramstr(0)) + 'biolife.xml');
  TMSFMXTableView1.ItemOptions := TMSFMXTableView1.ItemOptions + [ioLeftRectangle];
end;

procedure TForm550.TMSFMXTableView1ItemCustomize(Sender: TObject;
  AItem: TTMSFMXTableViewItem; AItemShape: TTMSFMXTableViewItemShape;
  AItemControlShape: TControl);
begin
  AItem.ShapeCaption.Height := 20;
  {$if compilerversion > 26}
  AItem.ShapeDescription.Align := TAlignLayout.None;
  AItem.ShapeDescription.Align := TAlignLayout.Client;
  {$else}
  AItem.ShapeDescription.Align := TAlignLayout.alNone;
  AItem.ShapeDescription.Align := TAlignLayout.alClient;
  {$ifend}
  AItem.ShapeDescription.AutoSize := True;
  AItemShape.Height := AItem.ShapeCaption.Height + AItem.ShapeDescription.Height + 10;
end;

end.
