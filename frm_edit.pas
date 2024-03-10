unit frm_edit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons;

type

  { TForm_Edit }

  TForm_Edit = class(TForm)
    BitBtn_OK: TBitBtn;
    BitBtn_Cancel: TBitBtn;
    Edit_Target: TEdit;
  private

  public

  end;

var
  Form_Edit: TForm_Edit;

implementation

{$R *.lfm}

end.
