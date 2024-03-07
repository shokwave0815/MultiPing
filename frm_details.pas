unit frm_details;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm_Details }

  TForm_Details = class(TForm)
    Memo_Details: TMemo;
    procedure Memo_DetailsChange(Sender: TObject);
  private

  public

  end;

var
  Form_Details: TForm_Details;

implementation

{$R *.lfm}

{ TForm_Details }

procedure TForm_Details.Memo_DetailsChange(Sender: TObject);
begin

end;

end.

