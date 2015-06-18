unit ImageListForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.ListView.Types, System.Rtti, System.Bindings.Outputs, Fmx.Bind.Editors,
  Data.Bind.EngExt, Fmx.Bind.DBEngExt, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  Data.Bind.Components, FMX.StdCtrls, FMX.Controls.Presentation,
  System.ImageList, FMX.ImgList, FMX.ListView, FireDAC.Stan.StorageBin,
  Data.Bind.DBScope;

type
  TImageListDemo = class(TForm)
    ListView1: TListView;
    ImageList1: TImageList;
    ToolBar1: TToolBar;
    Label1: TLabel;
    BindingsList1: TBindingsList;
    FDMemTable1: TFDMemTable;
    BindSourceDB1: TBindSourceDB;
    LinkListControlToField1: TLinkListControlToField;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ImageListDemo: TImageListDemo;

implementation

{$R *.fmx}

end.
