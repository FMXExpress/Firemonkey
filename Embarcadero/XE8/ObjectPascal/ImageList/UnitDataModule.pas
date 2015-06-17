unit UnitDataModule;

interface

uses
  System.SysUtils, System.Classes, System.ImageList, FMX.ImgList, System.Actions, FMX.ActnList, FMX.StdActns,
  FMX.Graphics;

type
  TMainDataModule = class(TDataModule)
    ImageList1: TImageList;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainDataModule: TMainDataModule;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}



end.
