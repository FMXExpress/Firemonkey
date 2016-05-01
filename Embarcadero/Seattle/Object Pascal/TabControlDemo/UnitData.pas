unit UnitData;

interface

uses
  System.SysUtils, System.Classes, System.ImageList, FMX.ImgList;

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
