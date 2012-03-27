program GongDiskUpload;

uses
  Forms,
  UploadMain in 'UploadMain.pas' {frmGongDiskUp},
  Common in 'Common.pas',
  UploadMgr in 'UploadMgr.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := GDefaultCaption;
  Application.CreateForm(TfrmGongDiskUp, frmGongDiskUp);
  Application.Run;
end.
