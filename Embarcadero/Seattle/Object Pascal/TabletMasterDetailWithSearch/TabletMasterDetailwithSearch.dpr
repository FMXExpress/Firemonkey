program TabletMasterDetailwithSearch;

uses
  System.StartUpCopy,
  FMX.Forms,
  MasterDetailTablet_Search in 'MasterDetailTablet_Search.pas' {TabletSearchForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TTabletSearchForm, TabletSearchForm);
  Application.Run;
end.
