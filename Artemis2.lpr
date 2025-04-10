program Artemis2;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, iphlpapi,
  //
  lazmouseandkeyinput, main, frmView, ScreenMon, AtermisClient,
  atermisworker, TCPWorkerThread, ServerThread, AudioThread;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TViewForm, ViewForm);
  Application.Run;
end.

