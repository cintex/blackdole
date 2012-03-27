program GongDiskDownload;

uses
  Forms,
  DownloadMain in 'DownloadMain.pas' {frmGongDiskDown},
  Common in 'Common.pas',
  DownloadMgr in 'DownloadMgr.pas',
  Config in 'Config.pas' {ConfigForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmGongDiskDown, frmGongDiskDown);
  Application.Run;
end.
