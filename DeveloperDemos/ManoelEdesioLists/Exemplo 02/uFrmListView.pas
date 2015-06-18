unit uFrmListView;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, Data.DB,
  Datasnap.DBClient, FMX.ListView.Types, FMX.ListView, Data.Bind.EngExt,
  Fmx.Bind.DBEngExt, System.Rtti, System.Bindings.Outputs, Fmx.Bind.Editors,
  Data.Bind.Components, Data.Bind.DBScope;

type
  TForm9 = class(TForm)
    ClientDataSet1: TClientDataSet;
    ListView1: TListView;
    BindSourceDB1: TBindSourceDB;
    BindingsList1: TBindingsList;
    LinkFillControlToField1: TLinkFillControlToField;
    StyleBook1: TStyleBook;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form9: TForm9;

implementation

{$R *.fmx}
{$R *.iPhone4in.fmx IOS}
 //C:\Documents and Settings\All Users\Documentos\Embarcadero\Studio\15.0\Styles
end.
