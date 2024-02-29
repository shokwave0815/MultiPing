unit frm_details;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm_Details }

  TForm_Details = class(TForm)
    Memo1: TMemo;
    procedure Memo1Change(Sender: TObject);
  private

  public

  end;

var
  Form_Details: TForm_Details;

implementation

{$R *.lfm}

{ TForm_Details }

procedure TForm_Details.Memo1Change(Sender: TObject);
begin

end;

end.

