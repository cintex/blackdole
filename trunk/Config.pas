unit Config;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, pngimage, AdvOfficeButtons, AdvGlowButton, StdCtrls,
  AdvEdit;

type
  TConfigForm = class(TForm)
    Shape1: TShape;
    Shape2: TShape;
    Image1: TImage;
    Image2: TImage;
    chkSaveDirectorySetting: TAdvOfficeCheckBox;
    btnSave: TAdvGlowButton;
    btnClose: TAdvGlowButton;
    edSaveDir: TAdvEdit;
    btnSearch: TAdvGlowButton;
    btnCloseW: TAdvGlowButton;
    procedure Shape2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnSearchClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ConfigForm: TConfigForm;

implementation

{$R *.dfm}

uses
  Common, FileCtrl;

procedure TConfigForm.btnSearchClick(Sender: TObject);
var
  S: string;
begin
  if SelectDirectory('업로드 하실 폴더를 선택해주세요', '', S) then
  begin
    edSaveDir.Text := S;
  end;
end;

procedure TConfigForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if ModalResult = mrOk then
  begin
    GSaveDirectory := edSaveDir.Text;
    GSaveDirectorySetting := chkSaveDirectorySetting.Checked;
  end;
end;

procedure TConfigForm.FormShow(Sender: TObject);
begin
  edSaveDir.Text := GSaveDirectory;
  chkSaveDirectorySetting.Checked := GSaveDirectorySetting;
end;

procedure TConfigForm.Shape2MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ReleaseCapture;
  SendMessage(Handle, WM_NCLBUTTONDOWN, HTCAPTION, 0);
end;

end.
