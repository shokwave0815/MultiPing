unit frm_change;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons;

type

  { TForm_Change }

  TForm_Change = class(TForm)
    BitBtn_OK: TBitBtn;
    BitBtn_Cancel: TBitBtn;
    Edit_Target: TEdit;
  private

  public

  end;

var
  Form_Change: TForm_Change;

implementation

{$R *.lfm}

end.
