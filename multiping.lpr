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
  Forms, main, target, frm_details, targetdm, frm_change
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TForm_Main, Form_Main);
  Application.CreateForm(TForm_Details, Form_Details);
  Application.CreateForm(TTargetData, TargetData);
  Application.CreateForm(TForm_Change, Form_Change);
  Application.Run;
end.

