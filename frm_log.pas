unit frm_log;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, LCLType;

type

  { TForm_Log }

  TForm_Log = class(TForm)
    Memo_Log: TMemo;
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure Memo_LogChange(Sender: TObject);
  private

  public

  end;

var
  Form_Log: TForm_Log;

implementation

{$R *.lfm}

{ TForm_Log }

procedure TForm_Log.Memo_LogChange(Sender: TObject);
begin

end;

procedure TForm_Log.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  Shift := Shift;
  if key = VK_ESCAPE then
    Close;
end;

end.
