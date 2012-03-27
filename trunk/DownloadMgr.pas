unit DownloadMgr;

interface

uses
  Classes, SysUtils, Forms, IdThreadSafe, IdComponent, IdThread, IdHTTP, IdFTP,
  IdFTPCommon, IdGlobal, IdAntiFreeze;

type
  TDownloadState = (dsReady, dsDownload, dsError, dsFinished);
  TFileInfo = class
    ArticleID: string;
    FileID: string;
    FileMD5: string;
    FileName: string;
    Dir: string;
    Kind: string;
    Size: Int64;
    FTPUser: string;
    FTPPass: string;
    FTPURL: string;
    FTPPort: string;
    FTPPath: string;
    State: TDownloadState;
  end;

  TDownloadMgr = class;

  TDownloadList = class(TObject)
  private
    FCode: Integer;
    FMsg: string;
    FList: TIdThreadSafeList;
    FOwner: TDownloadMgr;
    procedure DownFileDone(const AArticleID: string);
    procedure RequestFileInfo;
    function GetActiveFileInfo: TFileInfo;
    function GetItemCount: Integer;
    function GetItems(Index: Integer): TFileInfo;
    procedure SetItems(Index: Integer; const Value: TFileInfo);
  public
    constructor Create(AOwner: TDownloadMgr); reintroduce;
    destructor Destroy; override;
    procedure ClearItems;
    procedure Delete(Index: Integer);
    function IndexOf(const AFileID: string): Integer;
    property Items[Index: Integer]: TFileInfo read GetItems write SetItems;
    property ItemCount: Integer read GetItemCount;
    property ActiveFileInfo: TFileInfo read GetActiveFileInfo;
    property Code: Integer read FCode write FCode;
    property Msg: string read FMsg write FMsg;
  end;

  TDownloadEvent = procedure(ASender: TObject; APosition: Int64) of object;
  TDownloadBeginEvent = procedure(ASender: TObject; AMaximum: Int64) of object;
  TDownloadEndEvent = procedure(ASender: TObject) of object;

  TDownloadThread = class(TThread)
  private
    FOwner: TDownloadMgr;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TDownloadMgr); reintroduce;
    destructor Destroy; override;
  end;

  TDownloadMgr = class(TComponent)
  private
    FThread: TDownloadThread;
    FList: TDownloadList;
    FUserID: string;
    FArticleID: string;
    FAverageSpeed: Double;
    FRemainTime: Integer;
    FSaveDir: string;
    FServerURL: string;
    FStopped: Boolean;
    FIdAntiFreeze: TIdAntiFreeze;
    FIdFTP: TIdFTP;
    FMaximum: Int64;
    FReceivePosition: Int64;
    FStartTime: TDateTime;
    FTransferrignData: Boolean;
    FOnDownload: TDownloadEvent;
    FOnDownloadBegin: TDownloadBeginEvent;
    FOnDownloadComplete: TNotifyEvent;
    FOnDownloadEnd: TDownloadEndEvent;
    FOnDownloadError: TNotifyEvent;
    FOnGetFileList: TNotifyEvent;
    procedure DoFileDownload;
    procedure DoWorkBegin(ASender: TObject; AWorkMode: TWorkMode;
      AWorkCountMax: Int64);
    procedure DoWorkEnd(ASender: TObject; AWorkMode: TWorkMode);
    procedure DoWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
    function GetActiveFileInfo: TFileInfo;
    function GetFiles(Index: Integer): TFileInfo;
    function GetFileCount: Integer;
    function GetTotalSize: Int64;
    function GetTotalPosition: Int64;
    function GetCode: Integer;
    function GetCodeMsg: string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DeleteQueue(const AFileID: string);
    function IndexOfFileID(const AFileID: string): Integer;
    procedure RequestFileInfo;
    procedure Start;
    procedure Stop;
    property ActiveFileInfo: TFileInfo read GetActiveFileInfo;
    property ArticleID: string read FArticleID write FArticleID;
    property AverageSpeed: Double read FAverageSpeed write FAverageSpeed;
    property Code: Integer read GetCode;
    property CodeMsg: string read GetCodeMsg;
    property Files[Index: Integer]: TFileInfo read GetFiles;
    property FileCount: Integer read GetFileCount;
    property RemainTime: Integer read FRemainTime write FRemainTime;
    property SaveDir: string read FSaveDir write FSaveDir;
    property ServerURL: string read FServerURL write FServerURL;
    property Stopped: Boolean read FStopped;
    property TotalPosition: Int64 read GetTotalPosition;
    property TotalSize: Int64 read GetTotalSize;
    property UserID: string read FUserID write FUserID;
    property OnDownload: TDownloadEvent read FOnDownload write FOnDownload;
    property OnDownloadBegin: TDownloadBeginEvent read FOnDownloadBegin write FOnDownloadBegin;
    property OnDownloadComplete: TNotifyEvent read FOnDownloadComplete write FOnDownloadComplete;
    property OnDownloadEnd: TDownloadEndEvent read FOnDownloadEnd write FOnDownloadEnd;
    property OnDownloadError: TNotifyEvent read FOnDownloadError write FOnDownloadError;
    property OnGetFileList: TNotifyEvent read FOnGetFileList write FOnGetFileList;
  end;

implementation

uses
  CodeSiteLogging, HTTPApp,
  NativeXml, JclFileUtils,
  Common;

procedure TDownloadList.ClearItems;
begin
  FList.ClearAndFree;
end;

constructor TDownloadList.Create(AOwner: TDownloadMgr);
begin
  FList := TIdThreadSafeList.Create;
  FOwner := AOwner;
  FCode := 1;
end;

procedure TDownloadList.Delete(Index: Integer);
begin
  with FList.LockList do
  try
    Delete(Index);
  finally
    FList.UnlockList;
  end;
end;

destructor TDownloadList.Destroy;
begin
  ClearItems;
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TDownloadList.DownFileDone(const AArticleID: string);

  function TransferComplete: Boolean;
  var
    FI: TFileInfo;
    I: Integer;
  begin
    Result := True;
    with FList.LockList do
    try
      for I := 0 to Count - 1 do
      begin
        FI := TFileInfo(Items[I]);
        if Assigned(FI) and SameText(FI.ArticleID, AArticleID) and (FI.State <> dsFinished) then
          Result := False;
      end;
    finally
      FList.UnlockList;
    end;
  end;
var
  I, J: Integer;
  S: string;
  LXMLDoc: TNativeXml;
  LNode: TXmlNode;
begin
  if not TransferComplete then Exit;
  LXMLDoc := TNativeXml.Create(nil);
  try
    S := HTTPGet(FOwner.ServerURL + '/ws/wsftpupdown.asmx/WsDownFileDone?UserID=' + FOwner.UserID + '&ArticleID='
                     + FOwner.ArticleID);
    LXMLDoc.ExternalEncoding := seUTF8;
    LXMLDoc.XmlFormat := xfReadable;
    LXMLDoc.ReadFromString(S);
    if Assigned(LXMLDoc.Root) then
      with LXMLDoc.Root do
        for I := 0 to NodeCount - 1 do
        begin
          if SameText(Nodes[I].Name, 'Code') then
            FCode := StrToIntDef(Nodes[I].Value, 0)
          else if SameText(Nodes[I].Name, 'CodeMsg') then
            FMsg := Nodes[I].Value
        end;
  finally
    FreeAndNil(LXMLDoc);
  end;
end;

function TDownloadList.GetActiveFileInfo: TFileInfo;
var
  I: Integer;
  FI: TFileInfo;
begin
  Result := nil;
  with FList.LockList do
  try
    for I := 0 to Count - 1 do
    begin
      FI := TFileInfo(Items[I]);
      if Assigned(FI) and (FI.State <> dsFinished) and (FI.State <> dsError) then
      begin
        Result := TFileInfo(Items[I]);
        Exit;
      end;
    end;
  finally
    FList.UnlockList;
  end;
end;

function TDownloadList.GetItemCount: Integer;
begin
  with FList.LockList do
  try
    Result := Count;
  finally
    FList.UnlockList;
  end;
end;

function TDownloadList.GetItems(Index: Integer): TFileInfo;
begin
  with FList.LockList do
  try
    Result := TFileInfo(Items[Index]);
  finally
    FList.UnlockList;
  end;
end;

function TDownloadList.IndexOf(const AFileID: string): Integer;
var
  FI: TFileInfo;
  I: Integer;
begin
  Result := -1;
  with FList.LockList do
  try
    for I := 0 to Count - 1 do
    begin
      FI := TFileInfo(Items[I]);
      if Assigned(FI) then
        if SameText(FI.FileID, AFileID) then
        begin
          Result := I;
          Exit;
        end;
    end;
  finally
    FList.UnlockList;
  end;
end;

procedure TDownloadList.RequestFileInfo;
var
  S, LSaveDir: string;
  LXMLDoc: TNativeXml;
  LNode: TXmlNode;
  I, J: Integer;
  FI: TFileInfo;
begin
  ClearItems;
  Assert(FOwner <> nil);
  if FOwner.UserID = '' then Exit;
  if FOwner.ArticleID = '' then Exit;
  LXMLDoc := TNativeXml.Create(nil);
  try
    try
      S := HTTPGet(FOwner.ServerURL + '/ws/wsftpupdown.asmx/WsDownFile?UserID=' + FOwner.UserID + '&ArticleID='
                   + FOwner.ArticleID);
    except
      FCode := 0;
      FMsg := '서비스 이용이 불가합니다. 이용에 불편을 드려서 죄송합니다. 잠시 후 다시 시도하여 주시기 바랍니다.';
      Exit;
    end;
    LXMLDoc.ExternalEncoding := seUTF8;
    LXMLDoc.XmlFormat := xfReadable;
    LXMLDoc.ReadFromString(S);
    if Assigned(LXMLDoc.Root) then
      with LXMLDoc.Root do
        for I := 0 to NodeCount - 1 do
        begin
          if SameText(Nodes[I].Name, 'Code') then
            FCode := StrToIntDef(Nodes[I].Value, 0)
          else if SameText(Nodes[I].Name, 'CodeMsg') then
            FMsg := Nodes[I].Value
          else if SameText(Nodes[I].Name, 'LstFiles') then
          begin
            for J := 0 to Nodes[I].NodeCount - 1 do
            begin
              if SameText(Nodes[I].Nodes[J].Name, 'LstFile') then
              begin
                FI := TFileInfo.Create;
                FI.State := dsReady;
                LNode := Nodes[I].Nodes[J].FindNode('FileID');
                if Assigned(LNode) then
                  FI.FileID := LNode.Value;
                LNode := Nodes[I].Nodes[J].FindNode('FileMD5');
                if Assigned(LNode) then
                  FI.FileMD5 := LNode.Value;
                LNode := Nodes[I].Nodes[J].FindNode('Name');
                if Assigned(LNode) then
                  FI.FileName := UTF8ToString(HTTPDecode(UTF8String(LNode.Value)));
                LNode := Nodes[I].Nodes[J].FindNode('Dir');
                if Assigned(LNode) then
                begin
                  FI.Dir := UTF8ToString(HTTPDecode(UTF8String(LNode.Value)));
                  FI.Dir := StringReplace(FI.Dir, '/', '\', [rfReplaceAll]);
                end;
                LNode := Nodes[I].Nodes[J].FindNode('Kind');
                if Assigned(LNode) then
                  FI.Kind := UTF8ToString(HTTPDecode(UTF8String(LNode.Value)));
                LNode := Nodes[I].Nodes[J].FindNode('Size');
                if Assigned(LNode) then
                  FI.Size := StrToInt64(LNode.Value);
                LNode := Nodes[I].Nodes[J].FindNode('FtpUser');
                if Assigned(LNode) then
                  FI.FTPUser := LNode.Value;
                LNode := Nodes[I].Nodes[J].FindNode('FtpPass');
                if Assigned(LNode) then
                  FI.FTPPass := LNode.Value;
                LNode := Nodes[I].Nodes[J].FindNode('FtpUrl');
                if Assigned(LNode) then
                  FI.FTPUrl := LNode.Value;
                LNode := Nodes[I].Nodes[J].FindNode('FtpPort');
                if Assigned(LNode) then
                  FI.FTPPort := LNode.Value;
                LNode := Nodes[I].Nodes[J].FindNode('FtpPath');
                if Assigned(LNode) then
                  FI.FTPPath := UTF8ToString(HTTPDecode(UTF8String(LNode.Value)));

                if FI.Dir[1] = '\' then
                  LSaveDir := ExcludeTrailingPathDelimiter(FOwner.SaveDir) + FI.Dir
                else
                  LSaveDir := IncludeTrailingPathDelimiter(FOwner.SaveDir) + FI.Dir;
                LSaveDir := IncludeTrailingPathDelimiter(LSaveDir);
                if FI.Size = FileGetSize(LSaveDir + ExtractFileName(FI.FileName)) then
                  FI.State := dsFinished;

                with FList.LockList do
                try
                  Add(FI);
                finally
                  FList.UnlockList;
                end;
              end;
            end;
          end;
        end;
  finally
    FreeAndNil(LXMLDoc);
  end;
end;

procedure TDownloadList.SetItems(Index: Integer; const Value: TFileInfo);
begin
  with FList.LockList do
  try
    Items[Index] := Value;
  finally
    FList.UnlockList;
  end;
end;

{ TDownloadThread }

constructor TDownloadThread.Create(AOwner: TDownloadMgr);
begin
  inherited Create(False);
  FOwner := AOwner;
end;

destructor TDownloadThread.Destroy;
begin
  inherited Destroy;
end;

procedure TDownloadThread.Execute;
begin
  try
    Synchronize(FOwner.RequestFileInfo);
  except end;
  while not Terminated do
  try
    if not FOwner.Stopped then
      Synchronize(FOwner.DoFileDownload);
  except end;
end;

{ TDownloadMgr }

constructor TDownloadMgr.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOnDownloadBegin := nil;
  FOnDownload := nil;
  FOnDownloadEnd := nil;
  FOnDownloadComplete := nil;
  FStopped := False;
  FIdAntiFreeze := TIdAntiFreeze.Create(Self);
  FIdFTP := TIdFTP.Create(Self);
  FIdFTP.ConnectTimeout := GTimeOut;
  FIdFTP.ReadTimeout := GTimeOut;
  FIdFTP.TransferTimeout := GTimeOut;
  FIdFTP.Passive := True;
  FIdFTP.TransferType := ftBinary;
  FIdFTP.OnWork := DoWork;
  FIdFTP.OnWorkBegin := DoWorkBegin;
  FIdFTP.OnWorkEnd := DoWorkEnd;
  FList := TDownloadList.Create(Self);
  FThread := TDownloadThread.Create(Self);
end;

procedure TDownloadMgr.DeleteQueue(const AFileID: string);
var
  I: Integer;
begin
  I := IndexOfFileID(AFileID);
  if I > -1 then
    FList.Delete(I);
end;

destructor TDownloadMgr.Destroy;
begin
  FreeAndNil(FThread);
  FreeAndNil(FList);
  FreeAndNil(FIdFTP);
  FreeAndNil(FIdAntiFreeze);
  inherited Destroy;
end;

procedure TDownloadMgr.DoFileDownload;
var
  FI: TFileInfo;
  LSaveDir: string;
begin
  Assert(FSaveDir <> '');
  FI := GetActiveFileInfo;
  if not Assigned(FI) then
  begin
    Stop;
    if Assigned(FOnDownloadComplete) then
      FOnDownloadComplete(Self);
    Exit;
  end;
  with FIdFTP do
  try
    if FI.Dir <> EmptyStr then
    begin
      if FI.Dir[1] = '\' then
        LSaveDir := ExcludeTrailingPathDelimiter(FSaveDir) + FI.Dir
      else
        LSaveDir := IncludeTrailingPathDelimiter(FSaveDir) + FI.Dir;
      LSaveDir := StringReplace(LSaveDir, '\\', '\', [rfReplaceAll]);
      ForceDirectories(LSaveDir);
    end;

    Username := FI.FTPUser;
    Password := FI.FTPPass;
    Host := FI.FTPURL;
    Port := StrToInt(FI.FTPPort);
    try
      Connect;
      ChangeDir(FI.FTPPath);
      Get(FI.FileName, IncludeTrailingPathDelimiter(LSaveDir) +
        ExtractFileName(FI.FileName), False, True);
    except
      FI.State := dsError;
      if Assigned(FOnDownloadError) then
        FOnDownloadError(Self);
      Exit;
    end;
  finally
    Disconnect;
  end;
end;

procedure TDownloadMgr.DoWork(ASender: TObject; AWorkMode: TWorkMode;
  AWorkCount: Int64);
var
  TotalTime: TDateTime;
  Hour, Min, Sec, MSec: Word;
  DLTime: Double;
begin
  TotalTime := Now - FStartTime;
  DecodeTime(TotalTime, Hour, Min, Sec, MSec);
  Sec := Sec + Min * 60 + Hour * 3600;
  DLTime := Sec + MSec / 1000;
  if DLTime > 0 then
    FAverageSpeed := (AWorkCount / 1024) / DLTime;

  if FAverageSpeed > 0 then
    FRemainTime := Trunc(((GetTotalSize - (GetTotalPosition + AWorkCount)) / 1024) / FAverageSpeed);

  if Assigned(FOnDownload) then
    FOnDownload(Self, FReceivePosition + AWorkCount);
  Application.ProcessMessages;
end;

procedure TDownloadMgr.DoWorkBegin(ASender: TObject; AWorkMode: TWorkMode;
  AWorkCountMax: Int64);
var
  FI: TFileInfo;
  LSaveDir: string;
begin
  FI := GetActiveFileInfo;
  Assert(FI <> nil);

  FTransferrignData := True;
  FAverageSpeed := 0;
  FStartTime := Now;

  FI.State := dsDownload;
  if FI.Dir[1] = '\' then
    LSaveDir := ExcludeTrailingPathDelimiter(FSaveDir) + FI.Dir
  else
    LSaveDir := IncludeTrailingPathDelimiter(FSaveDir) + FI.Dir;
  LSaveDir := IncludeTrailingPathDelimiter(LSaveDir);
  FReceivePosition := FileGetSize(LSaveDir + ExtractFileName(FI.FileName));

  if AWorkCountMax > 0 then FMaximum := AWorkCountMax
  else FMaximum := FI.Size;

  if Assigned(FOnDownloadBegin) then
    FOnDownloadBegin(Self, FMaximum);
  Application.ProcessMessages;
end;

procedure TDownloadMgr.DoWorkEnd(ASender: TObject; AWorkMode: TWorkMode);
var
  FI: TFileInfo;
  LSaveDir: string;
begin
  FI := GetActiveFileInfo;
  Assert(FI <> nil);
  if FI.Dir[1] = '\' then
    LSaveDir := ExcludeTrailingPathDelimiter(FSaveDir) + FI.Dir
  else
    LSaveDir := IncludeTrailingPathDelimiter(FSaveDir) + FI.Dir;
  LSaveDir := IncludeTrailingPathDelimiter(LSaveDir);
  if FI.Size = FileGetSize(LSaveDir + ExtractFileName(FI.FileName)) then
    FI.State := dsFinished;

  FTransferrignData := False;
  FAverageSpeed := 0;
  if Assigned(FOnDownloadEnd) then
    FOnDownloadEnd(Self);
end;

function TDownloadMgr.GetActiveFileInfo: TFileInfo;
begin
  Assert(FList <> nil);
  Result := FList.ActiveFileInfo;
end;

function TDownloadMgr.GetCode: Integer;
begin
  Result := FList.Code;
end;

function TDownloadMgr.GetCodeMsg: string;
begin
  Result := FList.Msg;
end;

function TDownloadMgr.GetFileCount: Integer;
begin
  Assert(FList <> nil);
  Result := FList.ItemCount;
end;

function TDownloadMgr.GetFiles(Index: Integer): TFileInfo;
begin
  Assert(FList <> nil);
  Result := FList.Items[Index];
end;

function TDownloadMgr.GetTotalPosition: Int64;
var
  FI: TFileInfo;
  I: Integer;
begin
  Result := 0;
  if Assigned(FList) then
    for I := 0 to FList.ItemCount - 1 do
    begin
      FI := FList.Items[I];
      if FI.State = dsFinished then
        Result := Result + FI.Size;
    end;
end;

function TDownloadMgr.GetTotalSize: Int64;
var
  I: Integer;
begin
  Result := 0;
  if Assigned(FList) then
    for I := 0 to FList.ItemCount - 1 do
      Result := Result + FList.Items[I].Size;
end;

function TDownloadMgr.IndexOfFileID(const AFileID: string): Integer;
begin
  Result := -1;
  if Assigned(FList) then
    Result := FList.IndexOf(AFileID);
end;

procedure TDownloadMgr.RequestFileInfo;
begin
  if Assigned(FList) then
  begin
    FList.RequestFileInfo;
    if Assigned(FOnGetFileList) then
      FOnGetFileList(Self);
  end;
end;

procedure TDownloadMgr.Start;
begin
  FStopped := False;
end;

procedure TDownloadMgr.Stop;
begin
  try
    if FIdFTP.Connected then
    begin
      if FTransferrignData then
        FIdFTP.Abort;
      FIdFTP.Disconnect;
    end;
  except end;
  FStopped := True;
end;

end.
