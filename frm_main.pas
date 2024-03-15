unit frm_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Grids, Spin, target, fgl, frm_log, LCLType, Menus, Types, INIFiles,
  frm_targetdm, frm_edit, pingthread;

type

  { TForm_Main }

  TForm_Main = class(TForm)
    Button_MoveDown: TButton;
    Button_MoveUp: TButton;
    Button_Active: TButton;
    Button_Edit: TButton;
    Button_Delete: TButton;
    Button_Log: TButton;
    Button_Start: TButton;
    Button_AddTarget: TButton;
    Edit_AddTarget: TEdit;
    Label_AddTarget: TLabel;
    Label_Time: TLabel;
    Panel_Footer: TPanel;
    Separator1: TMenuItem;
    MenuItem_Active: TMenuItem;
    MenuItem_Delete: TMenuItem;
    MenuItem_Change: TMenuItem;
    MenuItem_Log: TMenuItem;
    Panel_Header: TPanel;
    PopupMenu_StringGrid: TPopupMenu;
    StringGrid_Targets: TStringGrid;
    SpinEdit_Time: TSpinEdit;
    Timer: TTimer;
    procedure Button_MoveDownClick(Sender: TObject);
    procedure Button_MoveUpClick(Sender: TObject);
    procedure Button_ActiveClick(Sender: TObject);
    procedure Button_AddTargetClick(Sender: TObject);
    procedure Button_EditClick(Sender: TObject);
    procedure Button_LogClick(Sender: TObject);
    procedure Button_StartClick(Sender: TObject);
    procedure Button_DeleteClick(Sender: TObject);
    procedure Edit_AddTargetKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure PopupMenu_StringGridPopup(Sender: TObject);
    procedure SpinEdit_TimeChange(Sender: TObject);
    procedure SpinEdit_TimeKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure StringGrid_TargetsDrawCell(Sender: TObject; aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
    procedure StringGrid_TargetsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure StringGrid_TargetsMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TimerStartTimer(Sender: TObject);
    procedure TimerStopTimer(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    isStartup: boolean;
    AppDir: string;
    cfgIni: TIniFile;
    myHeight, myWidth: integer;
    TargetList: TTargetList;
    procedure EditTarget;
    Procedure DeleteTarget;
    procedure ActivateTarget;
    procedure ShowLog;
    procedure MoveUp;
    procedure MoveDown;
    procedure EnableTargetControls(AEnabled: boolean);
    procedure PingTarget(ATarget: TTarget);
    procedure PingThreadFinish(ATarget: TTarget);
    procedure ClearStringGrid;
    procedure PrepareDatabase;
    procedure SaveTargetsOrder;
    procedure FillStringGrid;
    procedure FillStringGridRow(ATarget: TTarget);
    procedure LoadTargets;
    procedure LoadErrors;
    procedure LoadConfig;
    procedure SaveConfig;
  public

  end;

var
  Form_Main: TForm_Main;

implementation

{$R *.lfm}

{ TForm_Main }

procedure TForm_Main.Button_StartClick(Sender: TObject);
begin
  if not Timer.Enabled then
    Timer.Interval := SpinEdit_Time.Value * 1000;

  Timer.Enabled := not Timer.Enabled;
end;

procedure TForm_Main.Button_DeleteClick(Sender: TObject);
begin
  DeleteTarget;
end;

procedure TForm_Main.Edit_AddTargetKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  Shift := Shift;
  if key = VK_RETURN then
    Button_AddTarget.Click;
end;

procedure TForm_Main.Button_AddTargetClick(Sender: TObject);
begin
  if Trim(Edit_AddTarget.Text) <> '' then
  begin
    TargetDatabase.AddTarget(Edit_AddTarget.Text, TargetList.Count);
    SaveTargetsOrder;
    LoadTargets;
    FillStringGrid;
    EnableTargetControls(StringGrid_Targets.Row > 0);
  end;
end;

procedure TForm_Main.Button_ActiveClick(Sender: TObject);
begin
  ActivateTarget;
end;

procedure TForm_Main.Button_MoveUpClick(Sender: TObject);
begin
  MoveUp;
end;

procedure TForm_Main.Button_MoveDownClick(Sender: TObject);
begin
  MoveDown;
end;

procedure TForm_Main.Button_EditClick(Sender: TObject);
begin
  EditTarget;
end;

procedure TForm_Main.Button_LogClick(Sender: TObject);
begin
  ShowLog;
end;

procedure TForm_Main.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Timer.Enabled := False;
  SaveConfig;
  FreeAndNil(TargetList);
  CloseAction := caFree;
end;

procedure TForm_Main.FormCreate(Sender: TObject);
begin
  isStartup := True;
  AppDir := ExtractFilePath(ParamStr(0));
  TargetList := TTargetList.Create;
end;

procedure TForm_Main.FormResize(Sender: TObject);
begin
  if WindowState = wsNormal then
  begin
    myHeight := Height;
    myWidth := Width;
  end;
end;

procedure TForm_Main.FormShow(Sender: TObject);
begin
  if isStartup then
  begin
    LoadConfig;

    PrepareDatabase;
    LoadTargets;
    FillStringGrid;

    Form_Log.DatePicker_Log.Date := now;
    EnableTargetControls(StringGrid_Targets.Row > 0);

    isStartup := False;
  end;
end;

procedure TForm_Main.FormWindowStateChange(Sender: TObject);
begin
  if WindowState = wsNormal then
  begin
    Sleep(10); //ohne sleep reduziert sich das Fenster zur Unkenntlichkeit
    Height := myHeight;
    Width := myWidth;
  end;
end;

procedure TForm_Main.PopupMenu_StringGridPopup(Sender: TObject);
begin
  MenuItem_Active.Enabled := StringGrid_Targets.Row > 0;
  MenuItem_Change.Enabled := StringGrid_Targets.Row > 0;
  MenuItem_Delete.Enabled := StringGrid_Targets.Row > 0;
  MenuItem_Log.Enabled := StringGrid_Targets.Row > 0;
end;

procedure TForm_Main.SpinEdit_TimeChange(Sender: TObject);
begin

end;

procedure TForm_Main.SpinEdit_TimeKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  Shift := Shift;
  if (key = VK_RETURN) and Button_Start.Enabled then
    Button_Start.Click;
end;

procedure TForm_Main.StringGrid_TargetsDrawCell(Sender: TObject; aCol, aRow: integer;
  aRect: TRect; aState: TGridDrawState);
begin
  StringGrid_Targets.Canvas.Brush.Color := clWindow;
  StringGrid_Targets.Canvas.Font.Color := clWindowText;

  if not (gdFixed in aState) then
  begin

    if Timer.Enabled then
    begin
      case StringGrid_Targets.Cells[2, aRow] of
        'ja': StringGrid_Targets.canvas.Brush.Color := TColor($A0DDA0); //erreichbares Ziel
        'nein': StringGrid_Targets.canvas.Brush.Color := TColor($A0A0FF); //nicht erreichbares Ziel
      end;
    end;

    //deaktiviertes Ziel
    if StringGrid_Targets.Cells[0, aRow] = 'nein' then
      StringGrid_Targets.canvas.Brush.Color := TColor($E0E0E0);

    if gdSelected in aState then
      StringGrid_Targets.Canvas.Font.Style := [fsBold] //selektierte Zeile
    else
      StringGrid_Targets.Canvas.Font.Style := [];//nicht selektierte Zeile

    StringGrid_Targets.Canvas.FillRect(arect);
    StringGrid_Targets.Canvas.TextOut(aRect.Left + 2, aRect.Top + 2, StringGrid_Targets.Cells[aCol, aRow]);
    StringGrid_Targets.Canvas.FrameRect(aRect);
  end;
end;

procedure TForm_Main.StringGrid_TargetsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_DELETE: DeleteTarget;
    VK_RETURN: ShowLog;
    VK_F2: EditTarget;
    VK_SPACE: ActivateTarget;
    VK_UP:
      if ssCtrl in Shift then
      begin
        MoveUp;
        Key:= 0;
      end;
    VK_DOWN:
      if ssCtrl in Shift then
      begin
        MoveDown;
        Key:= 0;
      end;
  end;
end;

procedure TForm_Main.StringGrid_TargetsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Col, Row: Integer;
begin
  Shift:= Shift;
  StringGrid_Targets.MouseToCell(X, Y, Col, Row);
  StringGrid_Targets.Row:= Row;

  if (Button = mbLeft) and (Col = 0) then
    Button_Active.Click;
end;

procedure TForm_Main.TimerStartTimer(Sender: TObject);
begin
  SpinEdit_Time.Enabled := False;
  Button_Start.Caption := 'Stop';
  TimerTimer(Sender);
end;

procedure TForm_Main.TimerStopTimer(Sender: TObject);
begin
  Button_Start.Caption := 'Start';
  SpinEdit_Time.Enabled := True;
  FillStringGrid;
end;

procedure TForm_Main.TimerTimer(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to TargetList.Count - 1 do
  begin
    if TargetList.Items[i].Active then
    begin
      PingTarget(TargetList.Items[i]);
    end;
  end;
end;

procedure TForm_Main.EditTarget;
var
  Target: TTarget;
begin
  Target := TargetList[StringGrid_Targets.Row - 1];
  Form_Edit.Edit_Target.Text := Target.Address;
  Form_Edit.Edit_Target.SelectAll;

  if Form_Edit.ShowModal = mrOk then
  begin
    Target.Address := Form_Edit.Edit_Target.Text;
    TargetDatabase.ChangeTarget(Target);
    FillStringGrid;
  end;
end;

procedure TForm_Main.DeleteTarget;
begin
  if StringGrid_Targets.Row > 0 then
  begin
    if MessageDlg('Das Ziel "' + TargetList.Items[StringGrid_Targets.Row - 1].Address +
      '" wirklich löschen?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      TargetDatabase.RemoveTarget(TargetList[StringGrid_Targets.Row - 1].ID);
      TargetList.Delete(StringGrid_Targets.Row - 1);
      SaveTargetsOrder;
      FillStringGrid;
      EnableTargetControls(StringGrid_Targets.Row > 0);
    end;
  end;
end;

procedure TForm_Main.ActivateTarget;
var
  Target: TTarget;
begin
  Target := TargetList[StringGrid_Targets.Row - 1];
  Target.Active := not Target.Active;
  TargetDatabase.ChangeTarget(Target);
  FillStringGrid;
end;

procedure TForm_Main.ShowLog;
begin
  if (StringGrid_Targets.Row > 0) then
  begin
    Form_Log.LogTarget := TargetList[StringGrid_Targets.Row - 1];
    Form_Log.Caption := 'Protokoll für ' + Form_Log.LogTarget.Address;
    Form_Log.ReadLog;
    Form_Log.Label_Errors.Caption:= 'Anzahl Fehler: ' + IntToStr(TargetDatabase.ReadErrors(Form_Log.LogTarget, Form_Log.CheckBox_Filter.Checked, Form_Log.DatePicker_Log.Date));
    Form_Log.Show;
  end;
end;

procedure TForm_Main.MoveUp;
begin
  if StringGrid_Targets.Row > 1 then
  begin
    TargetList.Move(StringGrid_Targets.Row - 1, StringGrid_Targets.Row - 2);
    SaveTargetsOrder;
    FillStringGrid;
    StringGrid_Targets.Row := StringGrid_Targets.Row - 1;
  end;
end;

procedure TForm_Main.MoveDown;
begin
  if StringGrid_Targets.Row < StringGrid_Targets.RowCount - 1 then
  begin
    TargetList.Move(StringGrid_Targets.Row - 1, StringGrid_Targets.Row);
    SaveTargetsOrder;
    FillStringGrid;
    StringGrid_Targets.Row := StringGrid_Targets.Row + 1;
  end;
end;

procedure TForm_Main.EnableTargetControls(AEnabled: boolean);
begin
  Panel_Footer.Enabled := AEnabled;
  Button_Start.Enabled := AEnabled;
end;

procedure TForm_Main.PingTarget(ATarget: TTarget);
var PT: TPingThread;
begin
  if not ATarget.Running then
  begin
    ATarget.Running:= True;
    PT:= TPingThread.Create(True, ATarget);
    PT.OnFinish:= @PingThreadFinish;
    PT.Start;
  end;
end;

procedure TForm_Main.PingThreadFinish(ATarget: TTarget);
begin
  TargetDatabase.AddLogEntry(ATarget);
  FillStringGridRow(ATarget);
  ATarget.Running:= False;
  Application.ProcessMessages;
end;

procedure TForm_Main.ClearStringGrid;
begin
  StringGrid_Targets.RowCount := 1;
  StringGrid_Targets.RowCount := TargetList.Count + 1;
end;

procedure TForm_Main.PrepareDatabase;
begin
  TargetDatabase.Filename := AppDir + 'targets.sqlite';
  if not FileExists(TargetDatabase.Filename) then
    TargetDatabase.CreateDatabase;

  TargetDatabase.SQLite3Connection.Connected := True;
end;

procedure TForm_Main.SaveTargetsOrder;
var
  i: integer;
begin
  for i := 0 to TargetList.Count - 1 do
  begin
    TargetList[i].Position := i;
    TargetDatabase.ChangeTarget(TargetList[i]);
  end;
end;

procedure TForm_Main.FillStringGrid;
var
  i: integer;
begin
  StringGrid_Targets.BeginUpdate;
  StringGrid_Targets.RowCount := TargetList.Count + 1;
  for i := 0 to TargetList.Count - 1 do
  begin
    FillStringGridRow(TargetList[i]);
  end;
  StringGrid_Targets.EndUpdate;
end;

procedure TForm_Main.FillStringGridRow(ATarget: TTarget);
var Row: Integer;
    LastLogEntry: TLogEntry;
begin
  Row:= ATarget.Position + 1;

  StringGrid_Targets.Cells[0, Row] := BoolToStr(ATarget.Active, 'ja', 'nein');
  StringGrid_Targets.Cells[1, Row] := ATarget.Address;
  StringGrid_Targets.Cells[5, Row] := IntToStr(ATarget.NumberOfErrors);

  LastLogEntry := ATarget.LastLogEntry;
  if (LastLogEntry <> nil) and (LastLogEntry.Start > 0) then
  begin
    StringGrid_Targets.Cells[2, Row] := BoolToStr(LastLogEntry.Result, 'ja', 'nein');
    StringGrid_Targets.Cells[3, Row] := IntToStr(LastLogEntry.PingTime) + ' ms';
    StringGrid_Targets.Cells[4, Row] := DateTimeToStr(LastLogEntry.Start);
  end;
end;

procedure TForm_Main.LoadTargets;
begin
  TargetList.Clear;
  TargetDatabase.ReadTargets(TargetList);
  LoadErrors;
end;

procedure TForm_Main.LoadErrors;
var i: Integer;
begin
  for i:= 0 to TargetList.Count - 1 do
  begin
    TargetList[i].NumberOfErrors:= TargetDatabase.ReadErrors(TargetList[i], True, now);
  end;
end;

procedure TForm_Main.LoadConfig;
begin
  cfgINI := TINIFile.Create(AppDir + 'config.ini');

  //Dimension und Position der MainForm
  Top := Scale96ToScreen(cfgINI.ReadInteger('Window', 'Top', 100));
  Left := Scale96ToScreen(cfgINI.ReadInteger('Window', 'Left', 200));
  Width := Scale96ToForm(cfgINI.ReadInteger('Window', 'Width', 800));
  Height := Scale96ToForm(cfgINI.ReadInteger('Window', 'Height', 600));
  WindowState := TWindowState(cfgINI.ReadInteger('Window', 'State', 0));
  myHeight := Height;
  myWidth := Width;

  Form_Log.Width := Scale96ToForm(cfgINI.ReadInteger('LogWindow', 'Width', 450));
  Form_Log.Height := Scale96ToForm(cfgINI.ReadInteger('LogWindow', 'Height', 400));

  //Größe der Spalten des StringGrid
  StringGrid_Targets.Columns.Items[0].Width := Scale96ToForm(cfgINI.ReadInteger('SG', '0', 80));
  StringGrid_Targets.Columns.Items[1].Width := Scale96ToForm(cfgINI.ReadInteger('SG', '1', 150));
  StringGrid_Targets.Columns.Items[2].Width := Scale96ToForm(cfgINI.ReadInteger('SG', '2', 100));
  StringGrid_Targets.Columns.Items[3].Width := Scale96ToForm(cfgINI.ReadInteger('SG', '3', 130));
  StringGrid_Targets.Columns.Items[4].Width := Scale96ToForm(cfgINI.ReadInteger('SG', '4', 170));
  StringGrid_Targets.Columns.Items[5].Width := Scale96ToForm(cfgINI.ReadInteger('SG', '5', 100));

  //Einstellungen
  SpinEdit_Time.Value := cfgINI.ReadInteger('Prefs', 'Time', 30);
  Form_Log.CheckBox_AllEvents.Checked := cfgIni.ReadBool('Prefs', 'allEvents', False);
  Form_Log.CheckBox_Filter.Checked := cfgIni.ReadBool('Prefs', 'Filtered', True);
  Form_Log.DatePicker_Log.Enabled := cfgIni.ReadBool('Prefs', 'Filtered', True);

  FreeAndNil(CfgINI);
end;

procedure TForm_Main.SaveConfig;
begin
  cfgINI := TINIFile.Create(AppDir + 'config.ini');

  //Dimension und Position der MainForm
  if (WindowState = wsNormal) then //nicht speichern, wenn maximiert, minimiert
  begin
    cfgINI.WriteInteger('Window', 'Top', ScaleScreenTo96(Top));
    cfgINI.WriteInteger('Window', 'Left', ScaleScreenTo96(Left));
    cfgINI.WriteInteger('Window', 'Width', ScaleFormTo96(Width));
    cfgINI.WriteInteger('Window', 'Height', ScaleFormTo96(Height));
  end;
  cfgINI.WriteInteger('Window', 'State', Ord(WindowState));

  cfgINI.WriteInteger('LogWindow', 'Width', ScaleFormTo96(Form_Log.Width));
  cfgINI.WriteInteger('LogWindow', 'Height', ScaleFormTo96(Form_Log.Height));

  //Größe der Spalten des StringGrid
  cfgINI.WriteInteger('SG', '0', ScaleFormTo96(StringGrid_Targets.Columns.Items[0].Width));
  cfgINI.WriteInteger('SG', '1', ScaleFormTo96(StringGrid_Targets.Columns.Items[1].Width));
  cfgINI.WriteInteger('SG', '2', ScaleFormTo96(StringGrid_Targets.Columns.Items[2].Width));
  cfgINI.WriteInteger('SG', '3', ScaleFormTo96(StringGrid_Targets.Columns.Items[3].Width));
  cfgINI.WriteInteger('SG', '4', ScaleFormTo96(StringGrid_Targets.Columns.Items[4].Width));
  cfgINI.WriteInteger('SG', '5', ScaleFormTo96(StringGrid_Targets.Columns.Items[5].Width));

  //Einstellungen
  cfgIni.WriteInteger('Prefs', 'Time', SpinEdit_Time.Value);
  cfgIni.WriteBool('Prefs', 'allEvents', Form_Log.CheckBox_AllEvents.Checked);
  cfgIni.WriteBool('Prefs', 'Filtered', Form_Log.CheckBox_Filter.Checked);
  FreeAndNil(cfgINI);
end;

end.
