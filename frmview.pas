unit frmView;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  ExtCtrls,
  Types,
  LCLType,
  fileutil,
  AtermisClient
  ;

type

  { TViewForm }

  TViewForm = class (TForm)
    PaintBox1: TPaintBox;
    AScrollBox: TScrollBox;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormPaint(Sender: TObject);
    procedure FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: boolean);
    procedure PaintBox1Paint(Sender: TObject);
  private
    gX: Integer;
    gY: Integer;
  public
    FBmp1: TBitmap;
    FRect: TRect;
    PaintAction: Integer;
    AClient: TAtermisClient;
  end;

var
  ViewForm: TViewForm;

implementation

{$R *.lfm}

{ TViewForm }


procedure TViewForm.FormCreate(Sender: TObject);
begin
  AClient := nil;
  FBmp1 := TBitmap.Create;
end;

procedure TViewForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if AClient <> nil then
  begin
    AClient.doterminate();
  end;
end;

procedure TViewForm.FormDropFiles(Sender: TObject; const FileNames: array of string);
begin
  if DirectoryExists(FileNames[0]) then
    Exit;
  AClient.sendFile(FileNames[0]);
end;

procedure TViewForm.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if AClient = nil then
    Exit;
  //if key in [65..90] then
  //   AClient.sendKey(key-64,shift,2)
  //else
  AClient.sendKey(key, shift, 2);
  Key := 0;
end;

procedure TViewForm.FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if AClient = nil then
    Exit;
  //if key in [65..90] then
  //   AClient.sendKey(key-64,shift,1)
  //else
  AClient.sendKey(key, shift, 1);
  Key := 0;
end;

procedure TViewForm.FormPaint(Sender: TObject);
begin

end;

{
FPC 的 keydown 及 UTF8KeyPress, 在WINDOWS 上測試如下
順序: 先觸發 keydown 再 UTF8KeyPress
英數: 兩個都會收到, 但KEYDOWN 的英文字的KEY 值固定為大寫英文的
中文: UTF8KeyPress，keydown 不會收到任何東西。
CTRL: keydown 會收到，且KEY 會有值, SHIFT = 16, CTRL=17, ALT=18,CAPSLOCK=20,HOME=36,PAGEUP=33,
      PAGEDOWN=34,END=35 F1=112...F11=122, F12=123,
      BACKSPACE=8(UTF8KeyPress 也會收到),DEL=46,左=37,上=38,右=39,下=40
CTRL+英文: keydown 的KEY 會收到對應的ASCII 值及CTRL/ALT/SHIFT 的狀態，
           UTF8KeyPress會收到長度為1 的KEY，內容CTRL+a/A=1..CTRL+z/Z=26。

KeyDown: 只處理方向鍵，HOME鍵等特殊鍵，CTRL?
UTF8KeyPress: 處理英數，空白，中文等
}
procedure TViewForm.FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
begin
  if AClient = nil then
    Exit;
end;

procedure TViewForm.Image1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if AClient = nil then
    Exit;
  gX := X;
  gY := Y;
  AClient.sendMouseClick(gX, gY, 0, 2, Button, Shift);
end;

procedure TViewForm.Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if AClient = nil then
    Exit;
  gX := X;
  gY := Y;
  AClient.sendMouseMove(gX, gY);
end;

procedure TViewForm.Image1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if AClient = nil then
    Exit;
  gX := X;
  gY := Y;
  AClient.sendMouseClick(gX, gY, 0, 1, Button, Shift);
end;

procedure TViewForm.Image1MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: boolean);
begin
  Handled := True;
  if AClient = nil then
    Exit;
  AClient.sendMouseClick(gX, gY, WheelDelta, 1, mbMiddle, Shift);
end;


procedure TViewForm.PaintBox1Paint(Sender: TObject);
begin
  PaintBox1.Canvas.Lock;
  PaintBox1.Canvas.Draw(FRect.Left, FRect.Top, FBmp1);
  PaintBox1.Canvas.Unlock;
end;

end.
