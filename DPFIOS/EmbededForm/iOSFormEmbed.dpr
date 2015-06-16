Program iOSFormEmbed;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FFormEmbed},
  uEmbForm1 in 'uEmbForm1.pas' {FEmbForm1},
  uEmbForm2 in 'uEmbForm2.pas' {FEmbForm2};

{$I DPF.iOS.Defs.inc}
{$R *.res}

Begin
  Application.Initialize;
  Application.CreateForm(TFFormEmbed, FFormEmbed);
  Application.CreateForm(TFEmbForm1, FEmbForm1);
  Application.CreateForm(TFEmbForm2, FEmbForm2);
  Application.Run;

End.
