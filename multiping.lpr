program multiping;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, datetimectrls, frm_main, target, frm_log, frm_targetdm, frm_edit,
pingthread, configuration
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TForm_Main, Form_Main);
  Application.CreateForm(TForm_Log, Form_Log);
  Application.CreateForm(TTargetDatabase, TargetDatabase);
  Application.CreateForm(TForm_Edit, Form_Edit);
  Application.Run;
end.

