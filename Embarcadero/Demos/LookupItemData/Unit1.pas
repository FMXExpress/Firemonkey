unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  Data.Bind.GenData, FMX.ListView.Types, Data.Bind.EngExt, Fmx.Bind.DBEngExt,
  System.Rtti, System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.Components,
  FMX.Objects, FMX.ListView, Data.Bind.ObjectScope;

type
  TForm1 = class(TForm)
    PrototypeBindSource1: TPrototypeBindSource;
    ListView1: TListView;
    BindingsList1: TBindingsList;
    Rectangle1: TRectangle;
    LinkFillControlToPropertyFillColor: TLinkFillControlToProperty;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

end.
