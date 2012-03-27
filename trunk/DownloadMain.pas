unit DownloadMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, AdvGlowButton, Grids, AdvObj, BaseGrid, AdvGrid,
  AdvOfficeButtons, AdvSmoothProgressBar, IdBaseComponent, IdComponent,
  IdTCPConnection, IdTCPClient, IdExplicitTLSClientServerBase, IdFTP,
  IdFTPCommon, IdAntiFreezeBase, IdAntiFreeze, ExtCtrls, AdvEdit, ComCtrls,
  Contnrs, DownloadMgr, IdThread, ImgList, Menus, pngimage;

type
  TfrmGongDiskDown = class(TForm)
    Label1: TLabel;
    lblFileName: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lblSpeed: TLabel;
    btnPause: TAdvGlowButton;
    btnPlay: TAdvGlowButton;
    CurrentProgressBar: TAdvSmoothProgressBar;
    TotalProgressBar: TAdvSmoothProgressBar;
    CaptionImage: TImage;
    lblRemainTime: TLabel;
    btnExit: TAdvGlowButton;
    btnMinimize: TAdvGlowButton;
    btnClose: TAdvGlowButton;
    lblVersion: TLabel;
    sgFiles: TAdvStringGrid;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    Label5: TLabel;
    lblSaveDirectory: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Shape4: TShape;
    Shape5: TShape;
    chkFinishedSysDown: TAdvOfficeCheckBox;
    btnListFirst: TAdvGlowButton;
    btnListUp: TAdvGlowButton;
    btnListDown: TAdvGlowButton;
    btnListLast: TAdvGlowButton;
    btnSelectListDelete: TAdvGlowButton;
    btnCompleteListDelete: TAdvGlowButton;
    Shape6: TShape;
    Shape7: TShape;
    btnOpenSaveDir: TAdvGlowButton;
    btnConfig: TAdvGlowButton;
    BtnImages: TImageList;
    BtnHotImages: TImageList;
    Label6: TLabel;
    lblTransferSize: TLabel;
    TrayIcon: TTrayIcon;
    pmTrayMenu: TPopupMenu;
    mnuShowMain: TMenuItem;
    mnuHideMain: TMenuItem;
    N3: TMenuItem;
    mnuExit: TMenuItem;
    N1: TMenuItem;
    mnuWebPage: TMenuItem;
    Shape8: TShape;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnPlayClick(Sender: TObject);
    procedure DoDownloadBegin(ASender: TObject; AMaximum: Int64);
    procedure DoDownloadComplete(ASender: TObject);
    procedure DoDownloadEnd(ASender: TObject);
    procedure DoDownloadError(ASender: TObject);
    procedure DoDownload(ASender: TObject; APosition: Int64);
    procedure DoGetFileList(ASender: TObject);
    procedure btnPauseClick(Sender: TObject);
    procedure CaptionImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnMinimizeClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnExitClick(Sender: TObject);
    procedure sgFilesCustomCellDraw(Sender: TObject; Canvas: TCanvas; ACol,
      ARow: Integer; AState: TGridDrawState; ARect: TRect; Printing: Boolean);
    procedure btnListFirstClick(Sender: TObject);
    procedure btnListUpClick(Sender: TObject);
    procedure btnListDownClick(Sender: TObject);
    procedure btnListLastClick(Sender: TObject);
    procedure btnSelectListDeleteClick(Sender: TObject);
    procedure btnCompleteListDeleteClick(Sender: TObject);
    procedure btnOpenSaveDirClick(Sender: TObject);
    procedure mnuShowMainClick(Sender: TObject);
    procedure mnuHideMainClick(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure btnConfigClick(Sender: TObject);
    procedure mnuWebPageClick(Sender: TObject);
  private
    FDownloadMgr: TDownloadMgr;
    procedure LoadSetting;
    procedure SaveSetting;
    procedure UpdateFileList;
    procedure WMCopyData(var Message: TWMCopyData); message WM_COPYDATA;
  public

  end;

var
  frmGongDiskDown: TfrmGongDiskDown;

implementation

{$R *.dfm}

uses
  CodeSiteLogging, HTTPApp, ShellApi, Registry,
  Common, Config;

procedure TfrmGongDiskDown.btnCloseClick(Sender: TObject);
begin
  btnPause.Click;
  Close;
end;

procedure TfrmGongDiskDown.btnCompleteListDeleteClick(Sender: TObject);
var
  I: Integer;
  FI: TFileInfo;
begin
  for I := FDownloadMgr.FileCount - 1 downto 1 do
  begin
    FI := FDownloadMgr.Files[I];
    if Assigned(FI) and (FI.State = dsFinished) then
      FDownloadMgr.DeleteQueue(FI.FileID);
  end;
  UpdateFileList;
end;

procedure TfrmGongDiskDown.btnConfigClick(Sender: TObject);
begin
  ConfigForm := TConfigForm.Create(Self);
  try
    ConfigForm.ShowModal;
  finally
    FreeAndNil(ConfigForm);
  end;
end;

procedure TfrmGongDiskDown.btnExitClick(Sender: TObject);
begin
  if not FDownloadMgr.Stopped then
    FDownloadMgr.Stop;
  Close;
end;

procedure TfrmGongDiskDown.btnListDownClick(Sender: TObject);
begin
  //
end;

procedure TfrmGongDiskDown.btnListFirstClick(Sender: TObject);
begin
  //
end;

procedure TfrmGongDiskDown.btnListLastClick(Sender: TObject);
begin
  //
end;

procedure TfrmGongDiskDown.btnListUpClick(Sender: TObject);
begin
  //
end;

procedure TfrmGongDiskDown.btnMinimizeClick(Sender: TObject);
begin
  mnuHideMain.Click;
end;

procedure TfrmGongDiskDown.btnOpenSaveDirClick(Sender: TObject);
begin
  if lblSaveDirectory.Caption <> '' then
    ShellExecute(Handle, 'open', PChar(lblSaveDirectory.Caption), nil, nil, SW_SHOWNORMAL);
end;

procedure TfrmGongDiskDown.btnPauseClick(Sender: TObject);
begin
  if FDownloadMgr.Stopped then
  begin
    btnPause.Caption := '일시정지';
    FDownloadMgr.Start;
  end
  else
  begin
    btnPause.Caption := '재시작';
    FDownloadMgr.Stop;
  end;
end;

procedure TfrmGongDiskDown.btnPlayClick(Sender: TObject);
var
  LRow: Integer;
  FI: TFileInfo;
begin
  if sgFiles.SelectedRowCount > 0 then
  begin
    LRow := sgFiles.SelectedRow[0];
    FI := TFileInfo(sgFiles.Objects[0, LRow]);
    if Assigned(FI) then
      ShellExecute(Handle, 'open', PChar(FI.Dir + FI.FileName), nil, nil, SW_SHOWNORMAL);
  end;
end;

procedure TfrmGongDiskDown.btnSelectListDeleteClick(Sender: TObject);
var
  LRow: Integer;
  FI: TFileInfo;
begin
  if sgFiles.SelectedRowCount > 0 then
  begin
    LRow := sgFiles.SelectedRow[0];
    FI := TFileInfo(sgFiles.Objects[0, LRow]);
    if Assigned(FI) and (FI.State <> dsDownload) and (FI.State <> dsError) then
    begin
      FDownloadMgr.DeleteQueue(FI.FileID);
      UpdateFileList;
    end;
  end;
end;

procedure TfrmGongDiskDown.CaptionImageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ReleaseCapture;
  SendMessage(Handle, WM_NCLBUTTONDOWN, HTCAPTION, 0);
end;

procedure TfrmGongDiskDown.DoDownload(ASender: TObject; APosition: Int64);
begin
  if not Assigned(FDownloadMgr) then Exit;
  lblSpeed.Caption := Format('%d kbps', [Trunc(FDownloadMgr.AverageSpeed)]);
  lblRemainTime.Caption := Format('%s:%s:%s',
                                  [FormatFloat('00',FDownloadMgr.RemainTime div 3600),
                                  FormatFloat('00',(FDownloadMgr.RemainTime div 60) mod 60),
                                  FormatFloat('00',FDownloadMgr.RemainTime mod 60)]);
  CurrentProgressBar.Position := APosition;
  TotalProgressBar.Position := FDownloadMgr.TotalPosition + APosition;
  lblTransferSize.Caption := FileSizeToString(Round(TotalProgressBar.Maximum))
                             + '/'
                             + FileSizeToString(FDownloadMgr.TotalPosition + APosition);
end;

procedure TfrmGongDiskDown.DoDownloadBegin(ASender: TObject; AMaximum: Int64);
var
  FI: TFileInfo;
begin
  if not Assigned(FDownloadMgr) then Exit;
  FI := FDownloadMgr.ActiveFileInfo;
  Assert(FI <> nil);
  lblFileName.Caption := FI.FileName;
  CurrentProgressBar.Maximum := AMaximum;
  CurrentProgressBar.Position := 0;
  TotalProgressBar.Maximum := FDownloadMgr.TotalSize;
  UpdateFileList;
end;

procedure TfrmGongDiskDown.DoDownloadEnd(ASender: TObject);
begin
  UpdateFileList;
end;

procedure TfrmGongDiskDown.DoDownloadError(ASender: TObject);
begin
  UpdateFileList;
end;

procedure TfrmGongDiskDown.DoGetFileList(ASender: TObject);
begin
  UpdateFileList;
end;

procedure TfrmGongDiskDown.FormCreate(Sender: TObject);
begin
  LoadSetting;
  lblVersion.Caption := 'Ver ' + GetVersionInfo(ParamStr(0));
  lblSaveDirectory.Caption := GSaveDirectory;
  with sgFiles do
  begin
    FixedCols := 0;
    FixedRows := 1;
    FixedRowHeight := 20;
    ColCount := 4;
    RowCount := 2;
    ColWidths[0] := 243;
    ColWidths[1] := 98;
    ColWidths[2] := 93;
    ColWidths[3] := 70;
    Cells[0, 0] := '이름';
    Cells[1, 0] := '종류';
    Cells[2, 0] := '크기';
    Cells[3, 0] := '상태';
    RowHeights[1] := 18;
  end;

  FDownloadMgr := TDownloadMgr.Create(Self);
  FDownloadMgr.OnDownload := DoDownload;
  FDownloadMgr.OnDownloadBegin := DoDownloadBegin;
  FDownloadMgr.OnDownloadComplete := DoDownloadComplete;
  FDownloadMgr.OnDownloadEnd := DoDownloadEnd;
  FDownloadMgr.OnDownloadError := DoDownloadError;
  FDownloadMgr.OnGetFileList := DoGetFileList;
  if ParamCount = 3 then
  begin
    if ParamStr(1) = '' then
      FDownloadMgr.ServerURL := GDefaultServerURL
    else
      FDownloadMgr.ServerURL := ParamStr(1);
    FDownloadMgr.UserID := ParamStr(2);
    FDownloadMgr.ArticleID := ParamStr(3);
    FDownloadMgr.SaveDir := GSaveDirectory;
    FDownloadMgr.Start;
  end;
end;

procedure TfrmGongDiskDown.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FDownloadMgr);
  SaveSetting;
end;

procedure TfrmGongDiskDown.LoadSetting;
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    with Reg do
    begin
      RootKey := HKEY_CURRENT_USER;
      if OpenKey('\Software\CanEasy\GongDisk', TRUE) then
      begin
        if ValueExists('SaveDirectory') then
          GSaveDirectory := ReadString('SaveDirectory')
        else
          GSaveDirectory := GetMyDocumentsPath;
        if ValueExists('SaveDirectory') then
          GSaveDirectory := ReadString('SaveDirectory');
        if ValueExists('SaveDirectorySetting') then
          GSaveDirectorySetting := ReadBool('SaveDirectorySetting');
      end;
    end;
  finally
    FreeAndNil(Reg);
  end;
end;

procedure TfrmGongDiskDown.mnuExitClick(Sender: TObject);
begin
  btnClose.Click;
end;

procedure TfrmGongDiskDown.mnuHideMainClick(Sender: TObject);
begin
  TrayIcon.Visible := True;
  if Assigned(Application.MainForm) then
    Application.MainForm.Visible := False;
  if IsWindowVisible(Application.Handle) then
    ShowWindow(Application.Handle, SW_HIDE);
end;

procedure TfrmGongDiskDown.mnuShowMainClick(Sender: TObject);
begin
  TrayIcon.Visible := False;
  if Assigned(Application.MainForm) then
  begin
    Application.Restore;
    if Application.MainForm.WindowState = wsMinimized then
      Application.MainForm.WindowState := wsNormal;
    Application.MainForm.Visible := True;
    SetForegroundWindow(Application.Handle);
  end;
end;

procedure TfrmGongDiskDown.mnuWebPageClick(Sender: TObject);
begin
  ShellExecute(0, 'open', PChar(GGongDiskWebPage), nil, nil, SW_SHOWNORMAL);
end;

procedure TfrmGongDiskDown.SaveSetting;
var
  Reg: TRegistry;
begin
  if GSaveDirectory = '' then Exit;
  Reg := TRegistry.Create;
  try
    with Reg do
    begin
      RootKey := HKEY_CURRENT_USER;
      if OpenKey('\Software\CanEasy\GongDisk', TRUE) then
      begin
        WriteString('SaveDirectory', GSaveDirectory);
        WriteBool('SaveDirectorySetting', GSaveDirectorySetting);
        CloseKey;
      end;
    end;
  finally
    FreeAndNil(Reg);
  end;
end;

procedure TfrmGongDiskDown.sgFilesCustomCellDraw(Sender: TObject; Canvas: TCanvas;
  ACol, ARow: Integer; AState: TGridDrawState; ARect: TRect; Printing: Boolean);
var
  FI: TFileInfo;
begin
  if ARow > 0 then
  begin
    FI := TFileInfo(sgFiles.Objects[0, ARow]);
    if Assigned(FI) then
    begin
      if FI.State = dsDownload then
      begin
        Canvas.Brush.Color := clWhite;
        InflateRect(ARect, -1, -1);
        Canvas.FillRect(ARect);
        Canvas.Font.Color := $000066FF;
        Canvas.Font.Style := [fsBold];
        Canvas.TextOut(ARect.Left + 2, ARect.Top + 2, sgFiles.Cells[ACol, ARow]);
      end;
    end;
  end;
end;

procedure TfrmGongDiskDown.UpdateFileList;
var
  LRow: Integer;
  FI: TFileInfo;
begin
  with sgFiles do
  begin
    if FDownloadMgr.FileCount = 0 then RowCount := 2
    else RowCount := FDownloadMgr.FileCount;
    for LRow := 1 to FDownloadMgr.FileCount - 1 do
    begin
      RowHeights[LRow] := 18;
      FI := FDownloadMgr.Files[LRow];
      Cells[0, LRow] := FI.FileName;
      Cells[1, LRow] := FI.Kind;
      Cells[2, LRow] := FileSizeToString(FI.Size);
      if FI.State = dsReady then
        Cells[3, LRow] := '대기중'
      else if FI.State = dsError then
        Cells[3, LRow] := '실패'
      else if FI.State = dsDownload then
      begin
        Cells[3, LRow] := '다운로드중';
        ScrollInView(0, LRow);
      end
      else if FI.State = dsFinished then
        Cells[3, LRow] := '완료';
      Objects[0, LRow] := FI;
    end;
  end;
end;

procedure TfrmGongDiskDown.WMCopyData(var Message: TWMCopyData);
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Delimiter := ',';
    SL.DelimitedText := StrPas(PAnsiChar(Message.CopyDataStruct.lpData));
    if SL.Count = 3 then
    begin
      mnuShowMain.Click;
      if SL.Strings[0] = '' then
        FDownloadMgr.ServerURL := GDefaultServerURL
      else
        FDownloadMgr.ServerURL := SL.Strings[0];
      FDownloadMgr.UserID := SL.Strings[1];
      FDownloadMgr.ArticleID := SL.Strings[2];
      FDownloadMgr.RequestFileInfo;
      FDownloadMgr.Start;
      UpdateFileList;
    end;
  finally
    FreeAndNil(SL);
  end;
end;

procedure TfrmGongDiskDown.DoDownloadComplete(ASender: TObject);

  function IsComplete: Boolean;
  var
    I: Integer;
    FI: TFileInfo;
  begin
    Result := True;
    for I := 0 to FDownloadMgr.FileCount - 1 do
    begin
      FI := FDownloadMgr.Files[I];
      if Assigned(FI) then
        if FI.State <> dsFinished then
        begin
          Result := False;
          Break;
        end;
    end;
  end;
begin
  if FDownloadMgr.Code <> 1 then
    MessageDlg(FDownloadMgr.CodeMsg, mtInformation, [mbOk], 0)
  else
  begin
    if chkFinishedSysDown.Checked then
      ShutDown;
    if IsComplete then
      Close;
  end;
end;

end.
