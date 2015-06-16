program GPlacesPhoneDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  UGPlacesPhoneDemo in 'UGPlacesPhoneDemo.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
