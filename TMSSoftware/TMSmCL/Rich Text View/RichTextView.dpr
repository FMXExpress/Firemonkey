program RichTextView;

uses
  FMX.Forms,
  URichTextView in 'URichTextView.pas' {Form1121};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1121, Form1121);
  Application.Run;
end.
