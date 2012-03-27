unit UploadMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, AdvGlowButton, Grids, AdvObj, BaseGrid, AdvGrid,
  AdvOfficeButtons, AdvSmoothProgressBar, IdBaseComponent, IdComponent,
  IdTCPConnection, IdTCPClient, IdExplicitTLSClientServerBase, IdFTP,
  IdFTPCommon, IdAntiFreezeBase, IdAntiFreeze, ExtCtrls, AdvEdit, ComCtrls,
  Contnrs, UploadMgr, IdThread, ImgList, Menus, pngimage;

type
  TfrmGongDiskUp = class(TForm)
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
    Label7: TLabel;
    Label8: TLabel;
    Shape4: TShape;
    Shape5: TShape;
    chkFinishedSysDown: TAdvOfficeCheckBox;
    btnCompleteListDelete: TAdvGlowButton;
    Shape6: TShape;
    Shape7: TShape;
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
    Shape8: TShape;
    DisplayInfoTimer: TTimer;
    TrayImages: TImageList;
    mnuGongDiskWebSite: TMenuItem;
    N1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnPlayClick(Sender: TObject);
    procedure DoUploadBegin(ASender: TObject; AMaximum: Int64);
    procedure DoUploadComplete(ASender: TObject);
    procedure DoUploadEnd(ASender: TObject);
    procedure DoGetFileList(ASender: TObject);
    procedure btnPauseClick(Sender: TObject);
    procedure CaptionImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnMinimizeClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnExitClick(Sender: TObject);
    procedure sgFilesCustomCellDraw(Sender: TObject; Canvas: TCanvas; ACol,
      ARow: Integer; AState: TGridDrawState; ARect: TRect; Printing: Boolean);
    procedure btnCompleteListDeleteClick(Sender: TObject);
    procedure mnuShowMainClick(Sender: TObject);
    procedure mnuHideMainClick(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure CurrentProgressBarDrawValue(Sender: TObject; ValueFormat: string;
      var ValueText: string);
    procedure TotalProgressBarDrawValue(Sender: TObject; ValueFormat: string;
      var ValueText: string);
    procedure DisplayInfoTimerTimer(Sender: TObject);
    procedure TrayIconDblClick(Sender: TObject);
    procedure mnuGongDiskWebSiteClick(Sender: TObject);
  private
    FSmallImages: TImageList;
    FUploadMgr: TUploadMgr;
    procedure ShowMainForm;
    procedure HideMainForm;
    procedure UpdateFileList;
    procedure WMCopyData(var Message: TWMCopyData); message WM_COPYDATA;
    procedure WMSysCommand(var Message: TWMSysCommand); message WM_SYSCOMMAND;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public

  end;

var
  frmGongDiskUp: TfrmGongDiskUp;

implementation

{$R *.dfm}

uses
  CodeSiteLogging, HTTPApp, ShellApi,
  Common;

procedure TfrmGongDiskUp.btnCloseClick(Sender: TObject);
begin
  btnPause.Click;
  Close;
end;

procedure TfrmGongDiskUp.btnCompleteListDeleteClick(Sender: TObject);
var
  I: Integer;
  FI: TFileInfo;
begin
  if not Assigned(FUploadMgr) then Exit;
  for I := FUploadMgr.FileCount - 1 downto 0 do
  begin
    FI := FUploadMgr.Files[I];
    if Assigned(FI) and (FI.State = dsFinished) then
      FUploadMgr.DeleteQueue(I);
  end;
  UpdateFileList;
end;

procedure TfrmGongDiskUp.btnExitClick(Sender: TObject);
begin
  if Assigned(FUploadMgr) and not FUploadMgr.Stopped then
    FUploadMgr.Stop;
  Close;
end;

procedure TfrmGongDiskUp.btnMinimizeClick(Sender: TObject);
begin
  PostMessage(Handle, WM_SYSCOMMAND, SC_MINIMIZE, 0);
end;

procedure TfrmGongDiskUp.btnPauseClick(Sender: TObject);
begin
  if not Assigned(FUploadMgr) then Exit;
  if FUploadMgr.Stopped then
  begin
    btnPause.Caption := '일시정지';
    FUploadMgr.Resume;
  end
  else
  begin
    btnPause.Caption := '재시작';
    FUploadMgr.Stop;
  end;
end;

procedure TfrmGongDiskUp.btnPlayClick(Sender: TObject);
var
  LRow: Integer;
  FI: TFileInfo;
begin
  if sgFiles.SelectedRowCount > 0 then
  begin
    LRow := sgFiles.SelectedRow[0];
    FI := TFileInfo(sgFiles.Objects[0, LRow]);
    if Assigned(FI) and (FI.State <> dsReady) then
      ShellExecute(Handle, 'open', PChar(FI.Dir + FI.FileName), nil, nil, SW_SHOWNORMAL);
  end;
end;

procedure TfrmGongDiskUp.CaptionImageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ReleaseCapture;
  SendMessage(Handle, WM_SYSCOMMAND, SC_MOVE or HTCAPTION, 0);
end;

procedure TfrmGongDiskUp.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    Style := Style or WS_MINIMIZEBOX;
end;

procedure TfrmGongDiskUp.CurrentProgressBarDrawValue(Sender: TObject;
  ValueFormat: string; var ValueText: string);
var
  LPercent: Integer;
begin
  LPercent := Round(CurrentProgressBar.Position / CurrentProgressBar.Maximum * 100);
  ValueText := FileSizeToString(Round(CurrentProgressBar.Position)) + ' / '
               + FileSizeToString(Round(CurrentProgressBar.Maximum))
               + ' (' + IntToStr(LPercent) + '%)';
end;

procedure TfrmGongDiskUp.DoUploadBegin(ASender: TObject; AMaximum: Int64);
var
  FI: TFileInfo;
begin
  if not Assigned(FUploadMgr) then Exit;
  FI := FUploadMgr.ActiveFileInfo;
  Assert(FI <> nil);
  lblFileName.Caption := FI.FileName;
  CurrentProgressBar.Position := 0;
  DisplayInfoTimer.Enabled := True;
  UpdateFileList;
end;

procedure TfrmGongDiskUp.DoUploadEnd(ASender: TObject);
begin
  DisplayInfoTimer.Enabled := False;
  CurrentProgressBar.Position := CurrentProgressBar.Maximum;
  UpdateFileList;
end;

procedure TfrmGongDiskUp.DisplayInfoTimerTimer(Sender: TObject);
var
  LPercent: Integer;
begin
  if not Assigned(FUploadMgr) then Exit;
  if FUploadMgr.Stopped then Exit;

  CurrentProgressBar.Maximum := FUploadMgr.CurrentMaximum;
  CurrentProgressBar.Position := FUploadMgr.CurrentPosition;
  TotalProgressBar.Maximum := FUploadMgr.TotalSize;
  TotalProgressBar.Position := FUploadMgr.TotalPosition + CurrentProgressBar.Position;
  LPercent := Round(TotalProgressBar.Position / TotalProgressBar.Maximum * 100);

  lblSpeed.Caption := Format('%s/Sec', [FileSizeToString(Round(FUploadMgr.AverageSpeed))]);
  lblRemainTime.Caption := Format('%s:%s:%s',
                                  [FormatFloat('00',FUploadMgr.RemainTime div 3600),
                                  FormatFloat('00',(FUploadMgr.RemainTime div 60) mod 60),
                                  FormatFloat('00',FUploadMgr.RemainTime mod 60)]);
  lblTransferSize.Caption := FileSizeToString(Round(TotalProgressBar.Maximum))
                             + '/'
                             + FileSizeToString(FUploadMgr.TotalPosition + Round(CurrentProgressBar.Position));

  case LPercent of
    0..9:   TrayIcon.IconIndex := 0;
    10..20: TrayIcon.IconIndex := 1;
    21..30: TrayIcon.IconIndex := 2;
    31..40: TrayIcon.IconIndex := 3;
    41..50: TrayIcon.IconIndex := 4;
    51..60: TrayIcon.IconIndex := 5;
    61..70: TrayIcon.IconIndex := 6;
    71..80: TrayIcon.IconIndex := 7;
    81..90: TrayIcon.IconIndex := 8;
    91..99: TrayIcon.IconIndex := 9;
    100:    TrayIcon.IconIndex := 10;
  end;
  Caption := '['+ IntToStr(LPercent) + '%] ' + GDefaultCaption;
  TrayIcon.Hint := '['+ IntToStr(LPercent) + '%] ' + GDefaultCaption;
  Application.ProcessMessages;
end;

procedure TfrmGongDiskUp.DoGetFileList(ASender: TObject);
begin
  UpdateFileList;
end;

procedure TfrmGongDiskUp.FormCreate(Sender: TObject);
var
  SysIL: THandle;
  Sfi: TSHFileInfo;
begin
  Caption := GDefaultCaption;
  lblVersion.Caption := 'Ver ' + GetVersionInfo(ParamStr(0));
  with sgFiles do
  begin
    FixedCols := 0;
    FixedRows := 1;
    FixedRowHeight := 20;
    ColCount := 4;
    RowCount := 2;
    ColWidths[0] := 287;
    ColWidths[1] := 85;
    ColWidths[2] := 80;
    ColWidths[3] := 60;
    Cells[0, 0] := '이름';
    Cells[1, 0] := '종류';
    Cells[2, 0] := '크기';
    Cells[3, 0] := '상태';
    RowHeights[1] := 18;
  end;

  FSmallImages := TImageList.Create(Self);
  SysIL := SHGetFileInfo('', 0, Sfi, SizeOf(TSHFileInfo), SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  if SysIL <> 0 then
    FSmallImages.Handle := SysIL;
  FSmallImages.ShareImages := True;

  FUploadMgr := TUploadMgr.Create(Self);
  FUploadMgr.OnUpload := nil;
  FUploadMgr.OnUploadBegin := DoUploadBegin;
  FUploadMgr.OnUploadComplete := DoUploadComplete;
  FUploadMgr.OnUploadEnd := DoUploadEnd;
  FUploadMgr.OnGetFileList := DoGetFileList;
end;

procedure TfrmGongDiskUp.FormDestroy(Sender: TObject);
begin
  if Assigned(FUploadMgr) then
    FreeAndNil(FUploadMgr);
  FreeAndNil(FSmallImages);
end;

procedure TfrmGongDiskUp.HideMainForm;
begin
  TrayIcon.Visible := True;
  if Assigned(FUploadMgr) and FUploadMgr.Stopped then
    TrayIcon.IconIndex := 10
  else
    TrayIcon.IconIndex := TrayIcon.IconIndex - 1;

  if Assigned(Application.MainForm) then
    Application.MainForm.Visible := False;
  if IsWindowVisible(Application.Handle) then
    ShowWindow(Application.Handle, SW_HIDE);
end;

procedure TfrmGongDiskUp.mnuExitClick(Sender: TObject);
begin
  if Assigned(FUploadMgr) then
    FUploadMgr.Stop;
  Close;
end;

procedure TfrmGongDiskUp.mnuGongDiskWebSiteClick(Sender: TObject);
begin
  ShellExecute(0, 'open', PChar(GGongDiskWebPage), nil, nil, SW_SHOWNORMAL);
end;

procedure TfrmGongDiskUp.mnuHideMainClick(Sender: TObject);
begin
  PostMessage(Handle, WM_SYSCOMMAND, SC_MINIMIZE, 0);
end;

procedure TfrmGongDiskUp.mnuShowMainClick(Sender: TObject);
begin
  PostMessage(Handle, WM_SYSCOMMAND, SC_RESTORE, 0);
end;

procedure TfrmGongDiskUp.sgFilesCustomCellDraw(Sender: TObject; Canvas: TCanvas;
  ACol, ARow: Integer; AState: TGridDrawState; ARect: TRect; Printing: Boolean);
var
  FI: TFileInfo;
begin
  if ARow > 0 then
  begin
    FI := TFileInfo(sgFiles.Objects[0, ARow]);
    if Assigned(FI) then
    begin
      if FI.State = dsUpload then
      begin
        if (gdSelected in AState) then
          Canvas.Brush.Color := sgFiles.SelectionColor
        else
          Canvas.Brush.Color := clWhite;
        InflateRect(ARect, -1, -1);
        Canvas.FillRect(ARect);
        Canvas.Font.Color := $000066FF;
        Canvas.Font.Style := [fsBold];
        Canvas.TextRect(ARect, ARect.Left + 2, ARect.Top + 2, sgFiles.Cells[ACol, ARow]);
      end;
    end;
  end;
end;

procedure TfrmGongDiskUp.ShowMainForm;
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

procedure TfrmGongDiskUp.TotalProgressBarDrawValue(Sender: TObject;
  ValueFormat: string; var ValueText: string);
var
  LPercent: Integer;
begin
  LPercent := Round(TotalProgressBar.Position / TotalProgressBar.Maximum * 100);
  ValueText := FileSizeToString(Round(TotalProgressBar.Position)) + ' / '
               + FileSizeToString(Round(TotalProgressBar.Maximum))
               + ' (' + IntToStr(LPercent) + '%)';
end;

procedure TfrmGongDiskUp.TrayIconDblClick(Sender: TObject);
begin
  ShowMainForm;
end;

procedure TfrmGongDiskUp.UpdateFileList;
var
  LRow: Integer;
  FI: TFileInfo;
begin
  if not Assigned(FUploadMgr) then Exit;
  with sgFiles do
  begin
    if FUploadMgr.FileCount = 0 then RowCount := 2
    else RowCount := FUploadMgr.FileCount + 1;
    for LRow := 0 to FUploadMgr.FileCount - 1 do
    begin
      RowHeights[LRow + 1] := 18;
      FI := FUploadMgr.Files[LRow];
      if Assigned(FI) then
      begin
        Cells[0, LRow + 1] := FI.FileName;
        Cells[1, LRow + 1] := GetFileKind(FI.FileName);
        Cells[2, LRow + 1] := FileSizeToString(FI.Size);
        if FI.State = dsReady then
          Cells[3, LRow + 1] := '대기중'
        else if FI.State = dsError then
          Cells[3, LRow + 1] := '실패'
        else if FI.State = dsUpload then
        begin
          Cells[3, LRow + 1] := '업로드중';
          ScrollInView(0, LRow + 1);
        end
        else if FI.State = dsFinished then
          Cells[3, LRow + 1] := '완료';
        Objects[0, LRow + 1] := FI;
      end;
    end;
  end;
end;

procedure TfrmGongDiskUp.WMCopyData(var Message: TWMCopyData);
var
  SL: TStringList;
  S: string;
begin
  SL := TStringList.Create;
  try
    S := StrPas(PAnsiChar(Message.CopyDataStruct^.lpData));
    ExtractStrings([','], [], PChar(S), SL);
    if SL.Count > 3 then
    begin
      ShowMainForm;
      if SL.Strings[0] = '' then
        FUploadMgr.ServerURL := GDefaultServerURL
      else
        FUploadMgr.ServerURL := SL.Strings[0];
      FUploadMgr.UserID := SL.Strings[1];
      FUploadMgr.ArticleID := SL.Strings[2];
      if SameText(Trim(SL.Strings[3]), 'F') then
        FUploadMgr.PathType := ptFile
      else
        FUploadMgr.PathType := ptFolder;
    end;
    if SL.Count > 4 then
      FUploadMgr.LocalPath := ExtractLongPathName(SL.Strings[4]);
    Sleep(100);
    FUploadMgr.RequestInfo;
    FUploadMgr.Start;
    UpdateFileList;
  finally
    FreeAndNil(SL);
  end;
end;

procedure TfrmGongDiskUp.WMSysCommand(var Message: TWMSysCommand);
begin
  with Message do
  begin
    if CmdType and $FFF0 = SC_MINIMIZE then
      HideMainForm
    else if CmdType and $FFF0 = SC_RESTORE then
      ShowMainForm
    else
      DefaultHandler(Message);
  end;
end;

procedure TfrmGongDiskUp.DoUploadComplete(ASender: TObject);

  function IsComplete: Boolean;
  var
    I: Integer;
    FI: TFileInfo;
  begin
    Result := True;
    for I := 0 to FUploadMgr.FileCount - 1 do
    begin
      FI := FUploadMgr.Files[I];
      if Assigned(FI) and (FI.State <> dsFinished) then
      begin
        Result := False;
        Exit;
      end;
    end;
  end;
begin
  CurrentProgressBar.Position := 0;
  TotalProgressBar.Position := 0;
  Caption := GDefaultCaption;
  TrayIcon.IconIndex := 10;
  UpdateFileList;
  Application.ProcessMessages;
  if Assigned(FUploadMgr) and (FUploadMgr.Code <> 1) then
  begin
    if FUploadMgr.CodeMsg <> '' then
      MessageDlg(FUploadMgr.CodeMsg, mtInformation, [mbOk], 0);
  end
  else
  begin
    if chkFinishedSysDown.Checked then
      ShutDown;
    if IsComplete then
      Close;
  end;
end;

end.
