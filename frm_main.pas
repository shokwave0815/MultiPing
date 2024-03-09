unit frm_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Grids, Spin, pingsend, target, fgl, frm_log, LCLType, Menus, Types, INIFiles,
  frm_targetdm, frm_change;

type

  { TForm_Main }

  TForm_Main = class(TForm)
    Button_Change: TButton;
    Button_Start: TButton;
    Button_AddTarget: TButton;
    Button_Delete: TButton;
    Button_Log: TButton;
    Edit_AddTarget: TEdit;
    Label_AddTarget: TLabel;
    Label_Time: TLabel;
    MenuItem_Delete: TMenuItem;
    MenuItem_Change: TMenuItem;
    MenuItem_Log: TMenuItem;
    Panel_Header: TPanel;
    PopupMenu_StringGrid: TPopupMenu;
    StringGrid_Targets: TStringGrid;
    SpinEdit_Time: TSpinEdit;
    Timer: TTimer;
    procedure Button_AddTargetClick(Sender: TObject);
    procedure Button_ChangeClick(Sender: TObject);
    procedure Button_LogClick(Sender: TObject);
    procedure Button_StartClick(Sender: TObject);
    procedure Button_DeleteClick(Sender: TObject);
    procedure Edit_AddTargetKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure SpinEdit_TimeKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure StringGrid_TargetsDrawCell(Sender: TObject; aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
    procedure TimerStartTimer(Sender: TObject);
    procedure TimerStopTimer(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    isStartup: Boolean;
    AppDir: string;
    cfgIni: TIniFile;
    myHeight, myWidth: integer;
    PingCmd: TPINGSend;
    TargetList: TTargetList;
    procedure PingTarget(ATarget: TTarget);
    procedure ClearGrid;
    procedure PrepareDatabase;
    procedure PrintTargets;
    procedure LoadTargets;
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
  begin
    Timer.Interval := SpinEdit_Time.Value * 1000;
  end;
  Timer.Enabled := not Timer.Enabled;
end;

procedure TForm_Main.Button_DeleteClick(Sender: TObject);
begin
  if StringGrid_Targets.Row > 0 then
  begin
    if MessageDlg('Das Ziel "' + TargetList.Items[StringGrid_Targets.Row - 1].Address +
      '" wirklich löschen?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      TargetData.RemoveTarget(TargetList[StringGrid_Targets.Row - 1].ID);
      TargetList.Delete(StringGrid_Targets.Row - 1);
      StringGrid_Targets.RowCount := StringGrid_Targets.RowCount - 1;
      PrintTargets;
    end;
  end;
end;

procedure TForm_Main.Edit_AddTargetKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  Shift := Shift;
  if key = VK_RETURN then
  begin
    Button_AddTarget.Click;
  end;
end;

procedure TForm_Main.Button_AddTargetClick(Sender: TObject);
begin
  if Trim(Edit_AddTarget.Text) <> '' then
  begin
    TargetData.AddTarget(Edit_AddTarget.Text);
    LoadTargets;
    PrintTargets;
  end;
end;

procedure TForm_Main.Button_ChangeClick(Sender: TObject);
var
  Target: TTarget;
begin
  Target := TargetList[StringGrid_Targets.Row - 1];
  Form_Change.Edit_Target.Text := Target.Address;
  Form_Change.Edit_Target.SelectAll;
  if Form_Change.ShowModal = mrOk then
  begin
    Target.Address := Form_Change.Edit_Target.Text;
    TargetData.ChangeTarget(Target);
    PrintTargets;
  end;
end;

procedure TForm_Main.Button_LogClick(Sender: TObject);
begin
  if (StringGrid_Targets.Row > 0) then
  begin
    Form_Log.LogTarget:= TargetList[StringGrid_Targets.Row - 1];
    Form_Log.ReadLog;
    Form_Log.Show;
  end;
end;

procedure TForm_Main.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Timer.Enabled := False;

  SaveConfig;

  FreeAndNil(TargetList);
  FreeAndNil(PingCmd);

  CloseAction := caFree;
end;

procedure TForm_Main.FormCreate(Sender: TObject);
begin
  isStartup := True;
  AppDir := ExtractFilePath(ParamStr(0));
  PingCmd := TPINGSend.Create;
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
    PrintTargets;

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

procedure TForm_Main.SpinEdit_TimeKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  Shift := Shift;
  if key = VK_RETURN then
  begin
    Button_Start.Click;
  end;
end;

procedure TForm_Main.StringGrid_TargetsDrawCell(Sender: TObject; aCol, aRow: integer;
  aRect: TRect; aState: TGridDrawState);
begin
  if not (gdFixed in aState) and not (gdSelected in aState) and Timer.Enabled then
  begin
    case StringGrid_Targets.Cells[1, aRow] of
      'ja': begin
        StringGrid_Targets.canvas.Brush.Color := TColor($80CC80);
      end;
      'nein': begin
        StringGrid_Targets.canvas.Brush.Color := TColor($A0A0FF);
      end;
      else
      begin
        StringGrid_Targets.Canvas.Font.Color := clBlack;
      end;
    end;

    StringGrid_Targets.Canvas.FillRect(arect);
    StringGrid_Targets.Canvas.TextOut(aRect.Left + 2, aRect.Top + 2, StringGrid_Targets.Cells[aCol, aRow]);
    StringGrid_Targets.Canvas.FrameRect(aRect);
  end;
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
  PrintTargets;
end;

procedure TForm_Main.TimerTimer(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to TargetList.Count - 1 do
  begin
    PingTarget(TargetList.Items[i]);
  end;
  PrintTargets;
end;

procedure TForm_Main.PingTarget(ATarget: TTarget);
begin
  ATarget.LastLogEntry.Result := PingCmd.Ping(ATarget.Address);
  ATarget.LastLogEntry.Start := now;
  ATarget.LastLogEntry.PingTime := PingCmd.PingTime;
  ATarget.LastLogEntry.Interval := Timer.Interval;

  TargetData.AddLogEntry(ATarget);
end;

procedure TForm_Main.ClearGrid;
begin
  StringGrid_Targets.RowCount := 1;
  StringGrid_Targets.RowCount := TargetList.Count + 1;
end;

procedure TForm_Main.PrepareDatabase;
begin
  TargetData.SQLite3Connection.DatabaseName := AppDir + 'targets.sqlite';
  if not FileExists(TargetData.SQLite3Connection.DatabaseName) then
  begin
    TargetData.CreateDatabase;
  end;
  TargetData.SQLite3Connection.Connected := True;
end;

procedure TForm_Main.PrintTargets;
var
  i: integer;
  LastLogEntry: TLogEntry;
begin
  StringGrid_Targets.RowCount := TargetList.Count + 1;
  for i := 0 to TargetList.Count - 1 do
  begin
    StringGrid_Targets.Cells[0, i + 1] := TargetList[i].Address;

    LastLogEntry:= TargetList[i].LastLogEntry;
    if (LastLogEntry <> NIL) and (LastLogEntry.Start > 0) then
    begin
      StringGrid_Targets.Cells[1, i + 1] := BoolToStr(LastLogEntry.Result, 'ja', 'nein');
      StringGrid_Targets.Cells[2, i + 1] := IntToStr(LastLogEntry.PingTime) + 'ms';
      StringGrid_Targets.Cells[3, i + 1] := DateTimeToStr(LastLogEntry.Start);
    end;
  end;
end;

procedure TForm_Main.LoadTargets;
begin
  TargetList.Clear;
  TargetData.ReadTargets(TargetList);
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

  //Größe der Spalten des StringGrid
  StringGrid_Targets.Columns.Items[0].Width := Scale96ToForm(cfgINI.ReadInteger('SG', '0', 300));
  StringGrid_Targets.Columns.Items[1].Width := Scale96ToForm(cfgINI.ReadInteger('SG', '1', 150));
  StringGrid_Targets.Columns.Items[2].Width := Scale96ToForm(cfgINI.ReadInteger('SG', '2', 130));
  StringGrid_Targets.Columns.Items[3].Width := Scale96ToForm(cfgINI.ReadInteger('SG', '3', 200));

  //Einstellungen
  SpinEdit_Time.Value := cfgINI.ReadInteger('Prefs', 'Time', 30);
  Form_Log.CheckBox_AllEvents.Checked := cfgIni.ReadBool('Prefs', 'allEvents', False);

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

  //Größe der Spalten des StringGrid
  cfgINI.WriteInteger('SG', '0', ScaleFormTo96(StringGrid_Targets.Columns.Items[0].Width));
  cfgINI.WriteInteger('SG', '1', ScaleFormTo96(StringGrid_Targets.Columns.Items[1].Width));
  cfgINI.WriteInteger('SG', '2', ScaleFormTo96(StringGrid_Targets.Columns.Items[2].Width));
  cfgINI.WriteInteger('SG', '3', ScaleFormTo96(StringGrid_Targets.Columns.Items[3].Width));

  //Einstellungen
  cfgIni.WriteInteger('Prefs', 'Time', SpinEdit_Time.Value);
  cfgIni.WriteBool('Prefs', 'allEvents', Form_Log.CheckBox_AllEvents.Checked);
  FreeAndNil(cfgINI);
end;

end.
