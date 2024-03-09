unit frm_log;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, LCLType,
  ExtCtrls, DateTimePicker, target, frm_targetdm;

type

  { TForm_Log }

  TForm_Log = class(TForm)
    CheckBox_AllEvents: TCheckBox;
    DatePicker_Log: TDateTimePicker;
    Memo_Log: TMemo;
    Panel1: TPanel;
    procedure CheckBox_AllEventsChange(Sender: TObject);
    procedure DatePicker_LogEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    isStartup: boolean;
  public
    LogTarget: TTarget;
    procedure ReadLog;
  end;

var
  Form_Log: TForm_Log;

implementation

{$R *.lfm}

{ TForm_Log }

procedure TForm_Log.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  Shift := Shift;
  if key = VK_ESCAPE then
    Close;
end;

procedure TForm_Log.FormShow(Sender: TObject);
begin
  isStartup:= False;
end;

procedure TForm_Log.CheckBox_AllEventsChange(Sender: TObject);
begin
  if not isStartup then
    ReadLog;
end;

procedure TForm_Log.DatePicker_LogEditingDone(Sender: TObject);
begin
  if not isStartup then
    ReadLog;
end;

procedure TForm_Log.FormCreate(Sender: TObject);
begin
  isStartup:= True;
end;

procedure TForm_Log.ReadLog;
begin
  Memo_Log.Clear;
  Memo_Log.Append(TargetData.ReadLog(LogTarget, CheckBox_AllEvents.Checked, DatePicker_Log.Date));
end;

end.
